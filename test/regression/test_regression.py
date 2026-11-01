"""Regression tests: run run2LDRM on the coarsened AMZ2 case.

    pytest test/regression --exe PATH [--update-reference]

The reference outputs in ``reference/`` were produced by an earlier version
of the model, so a failure means results changed, not that they are wrong.
After an intended change in results, rerun with --update-reference and
commit the new references.
"""
import pytest

from regression import (CASES, HERE, OUTPUTS, compare_csv, compare_grid,
                        reference_path, run_model, save_reference)

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
    outputs as the solver3_iterative case given with flags. Its output directory has
    missing parents, which run2LDRM must create."""
    config = HERE.parent.parent / "example" / "amz2_coarse.toml"
    from_file, error = run_model(exe, inputs, "new_dir/solver3_iterative_toml", [], THREADS,
                                 common_args=["--config", str(config)])
    assert error is None, error
    from_flags = run_case("solver3_iterative", THREADS)
    differing = [f for f in OUTPUTS if (from_file / f).read_bytes() != (from_flags / f).read_bytes()]
    assert not differing, f"outputs differ between --config and flags: {differing}"
