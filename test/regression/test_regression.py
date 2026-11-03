"""Regression tests: run run2LDRM on the coarsened AMZ2 case.

    pytest test/regression --exe PATH [--update-reference]

The reference outputs in ``reference/`` were produced by an earlier version
of the model, so a failure means results changed, not that they are wrong.
After an intended change in results, rerun with --update-reference and
commit the new references.
"""
import subprocess
from array import array

import pytest

from regression import (CASES, COMMON_ARGS, HERE, NC_REGIONS, OUTPUTS,
                        TARGET_LIST_ARGS, TARGET_MASK_ARGS, compare_csv, compare_grid, netcdf_args,
                        reference_path, replace_args, run_model, save_reference, with_target)

THREADS = 4


@pytest.mark.parametrize("output", OUTPUTS)
@pytest.mark.parametrize("case", CASES)
def test_matches_reference(run_case, update_reference, case, output):
    new = run_case(case, THREADS) / output
    reference = reference_path(case, output)
    if update_reference:
        changed = save_reference(new, reference)
        pytest.skip(f"reference {'updated' if changed else 'unchanged'}: {reference.name}")

    compare = compare_grid if output.endswith(".dat") else compare_csv
    errors = compare(new, reference)
    assert not errors, "\n".join(errors[:10])


@pytest.mark.parametrize("case", CASES)
def test_same_results_any_thread_count(run_case, case):
    """Each grid cell is computed independently, so 1 and 4 OpenMP threads
    must give bit-identical outputs; a difference points to a race condition."""
    serial, parallel = run_case(case, 1), run_case(case, THREADS)
    differing = [f for f in OUTPUTS if (serial / f).read_bytes() != (parallel / f).read_bytes()]
    assert not differing, f"outputs differ between 1 and {THREADS} threads: {differing}"


def test_toml_config_same_as_flags(exe, inputs, run_case):
    """The example configuration file (example/amz2_coarse.toml) must give the same
    outputs as the solver3_iterative case given with flags. It gives the target region
    as a mask (--target) while the flags give lists of cells. Its output directory has
    missing parents, which run2LDRM must create."""
    config = HERE.parent.parent / "example" / "amz2_coarse.toml"
    # -of bin overrides the file (NetCDF rho grid, the default) with the format of the flags run
    from_file, error = run_model(exe, inputs, "new_dir/solver3_iterative_toml", [], THREADS,
                                 common_args=["--config", str(config), "-of", "bin"])
    assert error is None, error
    from_flags = run_case("solver3_iterative", THREADS)
    differing = [f for f in OUTPUTS if (from_file / f).read_bytes() != (from_flags / f).read_bytes()]
    assert not differing, f"outputs differ between --config and flags: {differing}"


def test_topomask_optional(exe, inputs, run_case):
    """--topomask is optional: an all-ones mask must give the same outputs as no mask,
    and zeros in the mask (1-layer cells) must change the results of solver 1."""
    import struct
    from regression import CASES, NX, NY, OUTPUTS

    n = NX * NY
    (inputs / "mask_ones.dat").write_bytes(struct.pack(f"<{n}f", *[1.0] * n))
    (inputs / "mask_zeros.dat").write_bytes(struct.pack(f"<{n}f", *[float(k % 3 != 0) for k in range(n)]))

    case = "solver1_direct"
    no_mask = run_case(case, THREADS)
    for mask, should_match in [("mask_ones.dat", True), ("mask_zeros.dat", False)]:
        outdir, error = run_model(exe, inputs, f"{case}_{mask[:-4]}", CASES[case] + ["-mask", mask], THREADS)
        assert error is None, error
        differing = [f for f in OUTPUTS if (outdir / f).read_bytes() != (no_mask / f).read_bytes()]
        if should_match:
            assert not differing, f"all-ones mask differs from no mask: {differing}"
        else:
            assert differing, "a mask with zeros did not change the results"


def test_target_mask_same_as_lists(exe, inputs, run_case):
    """--target (mask) must give the same outputs as --itarget/--jtarget/--domsize (lists):
    the cells are visited in the same order, so the outputs are byte-identical."""
    case = "solver1_direct"
    outdir, error = run_model(exe, inputs, f"{case}_target_mask", CASES[case], THREADS,
                              common_args=with_target(TARGET_MASK_ARGS))
    assert error is None, error
    from_lists = run_case(case, THREADS)
    differing = [f for f in OUTPUTS if (outdir / f).read_bytes() != (from_lists / f).read_bytes()]
    assert not differing, f"outputs differ between --target and the lists of cells: {differing}"


def write_list_with_index_zero(inputs):
    """A list of cells with a 0 index, as 0-based lists (e.g. from the former notebooks)
    have when the target region touches the first column of the grid."""
    values = array("f", (inputs / "target_i.dat").read_bytes())
    values[0] = 0.0
    (inputs / "target_i0.dat").write_bytes(values.tobytes())


@pytest.mark.parametrize("target_args, message", [
    (TARGET_MASK_ARGS + TARGET_LIST_ARGS, "not both"),
    ([], "target region is missing"),
    (TARGET_LIST_ARGS[:4], "target region is missing"),  # no --domsize
    (TARGET_LIST_ARGS[:-1] + ["100"], "must have --domsize = 100"),
    (["-it", "target_i0.dat"] + TARGET_LIST_ARGS[2:], "1-based"),
], ids=["mask_and_lists", "none", "no_domsize", "wrong_domsize", "index_zero"])
def test_target_options_errors(exe, inputs, target_args, message):
    """Wrong target-region options must stop the run with a clear message."""
    write_list_with_index_zero(inputs)
    assert_fails(exe, inputs, with_target(target_args), message)


def assert_fails(exe, inputs, args, message):
    """run2LDRM with args must stop with an error whose message contains message."""
    cmd = [str(exe), "-po", "expected_errors"] + args + CASES["solver3_iterative"]
    result = subprocess.run(cmd, cwd=inputs, capture_output=True, text=True)
    assert result.returncode != 0, "run2LDRM did not fail"
    assert message in result.stdout + result.stderr, (result.stdout + result.stderr)[-2000:]


@pytest.mark.parametrize("case", CASES)
def test_netcdf_same_as_binary(exe, inputs, run_case, case):
    """NetCDF inputs (--input, and --source_regions/--target from the regions file, as written
    by the preprocessing programs) must give the same outputs as the binary files: they have
    the same values."""
    outdir, error = run_model(exe, inputs, f"{case}_netcdf", CASES[case], THREADS,
                              common_args=netcdf_args())
    assert error is None, error
    from_binary = run_case(case, THREADS)
    differing = [f for f in OUTPUTS if (outdir / f).read_bytes() != (from_binary / f).read_bytes()]
    assert not differing, f"outputs differ between NetCDF and binary inputs: {differing}"


def option(args, name, value):
    """args with the value of option name replaced."""
    i = args.index(name)
    return args[:i + 1] + [value] + args[i + 2:]


NC_ARGS = netcdf_args()


@pytest.mark.parametrize("args, message", [
    (option(NC_ARGS, "-nx", "31"), "--nx = 31 but"),
    (option(NC_ARGS, "-ny", "25"), "--ny = 25 but"),
    (option(NC_ARGS, "-udt", "1"), "--UV_dt = 1 must be equal to --data_dt"),
    (option(option(NC_ARGS, "-ddt", "6"), "-udt", "6"), "--data_dt = 6 hours, but the time step"),
    (option(NC_ARGS, "-d0", "2018-05-01_03:00:00"), "--datetime0 must be the first time"),
    (option(NC_ARGS, "-in", "input_nc_gap.txt"), "not consecutive"),
    (option(NC_ARGS, "-nreg", "3"), "--nregions = 3 but"),
    (NC_ARGS + ["-PP", "PP.txt"], "not both"),
    (replace_args(["-in", "input_nc.txt"], [], NC_ARGS), "input data is missing"),
    (option(NC_ARGS, "-in", "input_nc_regions.txt"), "No dimension 'slab' in"),
    (option(NC_ARGS, "-in", "PP.txt"), "is not a NetCDF file"),
], ids=["nx", "ny", "UV_dt", "data_dt", "datetime0", "not_consecutive", "nregions",
        "netcdf_and_binary", "no_input", "regions_as_input", "binary_as_netcdf"])
def test_netcdf_options_errors(exe, inputs, args, message):
    """Options that disagree with the NetCDF files must stop the run with a clear message."""
    first_file = (inputs / "input_nc.txt").read_text().split()[0]
    (inputs / "input_nc_gap.txt").write_text(f"{first_file}\n{first_file}\n")  # 2nd file starts too early
    (inputs / "input_nc_regions.txt").write_text(f"{NC_REGIONS}\n")
    assert_fails(exe, inputs, args, message)


def test_netcdf_output(exe, inputs, run_case):
    """The rho grid as NetCDF (--outformat nc, the default) must have the same values as the
    binary rho_grid.dat, with its time, region and the XLAT/XLONG of the NetCDF inputs."""
    netCDF4 = pytest.importorskip("netCDF4")
    case = "solver3_iterative"
    outputs = [f for f in OUTPUTS if f != "rho_grid.dat"] + ["rho_grid.nc"]
    outdir, error = run_model(exe, inputs, f"{case}_netcdf_output", CASES[case], THREADS,
                              common_args=option(netcdf_args(), "-of", "nc"), outputs=outputs)
    assert error is None, error
    binary = array("f", (run_case(case, THREADS) / "rho_grid.dat").read_bytes())
    with netCDF4.Dataset(outdir / "rho_grid.nc") as nc:
        rho = nc["rho"]
        assert rho.dimensions == ("time", "region", "south_north", "west_east")
        nt, nreg, ny, nx = rho.shape
        assert (nreg, ny, nx) == (2, 26, 32)
        assert nt * nreg * ny * nx == len(binary)
        # same memory order as the binary file: (nx, ny, nregions, time) in Fortran
        values = rho[:].filled(float("nan")).astype("<f4").tobytes()
        assert values == binary.tobytes(), "rho differs from the binary rho grid"
        assert list(nc["region"][:]) == [1, 2]
        # 2018-05-04 00:00 (-d1) to 2018-05-07 21:00 (-d2), 3-hourly, in hours since -d0
        assert nc["time"].units == "hours since 2018-05-01 00:00:00"
        assert list(nc["time"][:]) == [72 + 3 * i for i in range(nt)]
        assert nt == 32
        assert "XLAT" in nc.variables and "XLONG" in nc.variables


def test_rho_grid_monthly_files(exe, inputs, run_case):
    """--writerhogrid_option 2 writes one rho grid per month, named by the month. With the
    dates moved 4 days earlier (the data is the same), the run starts on 2018-04-30: the
    two monthly files together must be the rho grid of the whole run."""
    case = "solver3_iterative"
    args = option(option(option(COMMON_ARGS, "-d0", "2018-04-27_00:00:00"),
                         "-d1", "2018-04-30_00:00:00"), "-d2", "2018-05-03_21:00:00")
    outputs = [f for f in OUTPUTS if f != "rho_grid.dat"] + ["rho_grid_2018-04.dat", "rho_grid_2018-05.dat"]
    outdir, error = run_model(exe, inputs, f"{case}_monthly", CASES[case] + ["-wro", "2"], THREADS,
                              common_args=args, outputs=outputs)
    assert error is None, error
    april = (outdir / "rho_grid_2018-04.dat").read_bytes()
    may = (outdir / "rho_grid_2018-05.dat").read_bytes()
    assert len(april) == 8 * 2 * 26 * 32 * 4, "April must have the 8 steps of 2018-04-30"
    assert april + may == (run_case(case, THREADS) / "rho_grid.dat").read_bytes()
