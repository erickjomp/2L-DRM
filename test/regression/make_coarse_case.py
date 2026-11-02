"""Build the small regression-test case from the AMZ2 CPM 4 km inputs.

The original inputs (1055 x 863 cells, 3-hourly, ~120 GB) are coarsened by
averaging BLOCK x BLOCK cells and cut to the first few days of May 2018.
The result is written gzipped to ``data/`` and committed to the repo.

Only needed to regenerate the case (requires access to GLADE, numpy and netCDF4):

    python make_coarse_case.py [--src DIR] [--out DIR] [--netcdf-only]

All binary files are raw float32 in Fortran order: (nx, ny, nslabs, nt).

The same data is also written as NetCDF, in the format of the preprocessing programs
(https://github.com/erickjomp/2L-DRM-preprocessing): amz2_coarse_1.nc and amz2_coarse_2.nc
as the outputs of prepro2LDRM-metdata split in two files, and amz2_coarse_regions.nc as
that of prepro2LDRM-regions. They are made from the binary files (--netcdf-only rewrites
only them), so both formats have the same values.
"""
import argparse
import gzip
from pathlib import Path

import numpy as np

SRC = Path("/glade/u/home/erickkc2/uiuc0017/2LDRM_AMZ2/01_inputs/CPM_2levels500")
# a wrfout file of the run, for XLAT and XLONG of the NetCDF files
WRF_FILE = Path("/glade/campaign/univ/uiuc0017/erickkc2/WRF-WVT_AMZ2/03_WRF-WVT_CPM/Runs/"
                "AMZ4km_2025-02-18T09:16:05/wrfout_d01_2018-05-01_00:00:00")
NX, NY, NSLABS = 1055, 863, 2
BLOCK = 32
DX = 4000.0

# Time window (3-hourly steps from 2018-05-01 00:00). Split into two files so
# the test also covers loading a second file while keeping the tracing history.
STEPS_FILE1 = 32  # 2018-05-01 00:00 .. 2018-05-04 21:00
STEPS_FILE2 = 24  # 2018-05-05 00:00 .. 2018-05-07 21:00
DATA_DT = 3  # hours
TIME0 = "2018-05-01 00:00:00"
PRESSURE_INTERSLAB = 50000.0  # Pa (CPM_2levels500)


def coarsen(a):
    """Block-average the last two axes (ny, nx), cropping the remainder."""
    ny, nx = a.shape[-2] // BLOCK * BLOCK, a.shape[-1] // BLOCK * BLOCK
    a = a[..., :ny, :nx]
    shape = a.shape[:-2] + (ny // BLOCK, BLOCK, nx // BLOCK, BLOCK)
    return a.reshape(shape).mean(axis=(-3, -1), dtype=np.float64).astype("<f4")


def majority(a):
    """Most frequent value in each block (for masks and region ids)."""
    ny, nx = a.shape[0] // BLOCK * BLOCK, a.shape[1] // BLOCK * BLOCK
    blocks = a[:ny, :nx].reshape(ny // BLOCK, BLOCK, nx // BLOCK, BLOCK)
    blocks = blocks.transpose(0, 2, 1, 3).reshape(ny // BLOCK, nx // BLOCK, -1)
    out = np.empty(blocks.shape[:2], dtype="<f4")
    for idx in np.ndindex(out.shape):
        vals, counts = np.unique(blocks[idx], return_counts=True)
        out[idx] = vals[np.argmax(counts)]
    return out


def read_static(name):
    return np.fromfile(SRC / name, dtype="<f4").reshape(NY, NX)


def read_steps(name, nlev, start, count):
    data = np.memmap(SRC / name, dtype="<f4", mode="r").reshape(-1, nlev, NY, NX)
    return np.asarray(data[start:start + count])


def write(path, array):
    # fixed mtime and level keep the .gz bytes identical when regenerated with the same data
    with open(path, "wb") as raw, gzip.GzipFile(fileobj=raw, mode="wb", compresslevel=9, mtime=0) as f:
        f.write(np.ascontiguousarray(array, dtype="<f4").tobytes())


def read_gz(path, shape):
    with gzip.open(path, "rb") as f:
        return np.frombuffer(f.read(), dtype="<f4").reshape(shape)


# name: (long_name, units, dimension of the levels or None for surface variables), as in
# the NetCDF files of prepro2LDRM-metdata
NC_VARIABLES = {
    "PW": ("precipitable water of the slab", "kg m-2", "slab"),
    "U": ("moisture-weighted u wind of the slab", "m s-1", "slab"),
    "V": ("moisture-weighted v wind of the slab", "m s-1", "slab"),
    "PWflux": ("moisture flux between slabs", "kg m-2", "interslab"),
    "PP": ("precipitation", "kg m-2", None),
    "ET": ("evapotranspiration", "kg m-2", None),
}


def write_netcdf(out, ny, nx):
    """NetCDF versions of the binary files in out, in the format of the preprocessing programs."""
    import netCDF4

    with netCDF4.Dataset(WRF_FILE) as nc:
        lat = coarsen(np.ma.filled(nc["XLAT"][0], np.nan))
        lon = coarsen(np.ma.filled(nc["XLONG"][0], np.nan))

    def add_lat_lon(nc):
        nc.createDimension("west_east", nx)
        nc.createDimension("south_north", ny)
        for name, data, long_name, units in [("XLAT", lat, "latitude", "degree_north"),
                                             ("XLONG", lon, "longitude", "degree_east")]:
            var = nc.createVariable(name, "f4", ("south_north", "west_east"))
            var.long_name, var.units = long_name, units
            var[:] = data

    nsteps_before = 0
    for ifile, nt in enumerate([STEPS_FILE1, STEPS_FILE2], 1):
        with netCDF4.Dataset(out / f"amz2_coarse_{ifile}.nc", "w") as nc:
            add_lat_lon(nc)
            nc.createDimension("slab", NSLABS)
            nc.createDimension("interslab", NSLABS - 1)
            nc.createDimension("time", None)
            var = nc.createVariable("time", "f8", ("time",))
            var.long_name, var.units, var.calendar = "start of the output step", f"hours since {TIME0}", "standard"
            # same time origin in all the files, as with --split in prepro2LDRM-metdata
            var[:] = (nsteps_before + np.arange(nt)) * DATA_DT
            var = nc.createVariable("slab", "i4", ("slab",))
            var.long_name = "slab number"
            var[:] = np.arange(1, NSLABS + 1)
            var = nc.createVariable("interslab", "f4", ("interslab",))
            var.long_name, var.units = "interslab level pressure", "Pa"
            var[:] = PRESSURE_INTERSLAB
            for name, (long_name, units, levels) in NC_VARIABLES.items():
                nlev = {"slab": NSLABS, "interslab": NSLABS - 1, None: None}[levels]
                shape = (nt, ny, nx) if nlev is None else (nt, nlev, ny, nx)
                dims = ("time", "south_north", "west_east") if nlev is None else \
                       ("time", levels, "south_north", "west_east")
                # compressed and chunked by time step, as prepro2LDRM-metdata
                var = nc.createVariable(name, "f4", dims, zlib=True, complevel=1, shuffle=True,
                                        chunksizes=(1,) + shape[1:])
                var.long_name, var.units, var.coordinates = long_name, units, "XLONG XLAT"
                var[:] = read_gz(out / f"{name}_{ifile}.dat.gz", shape)
            nc.title = "2L-DRM inputs"
            nc.source = ("test/regression/make_coarse_case.py of 2L-DRM, in the format of prepro2LDRM-metdata "
                         "(https://github.com/erickjomp/2L-DRM-preprocessing)")
        nsteps_before += nt

    regions = read_gz(out / "regions.dat.gz", (ny, nx))
    target = read_gz(out / "target.dat.gz", (ny, nx))
    with netCDF4.Dataset(out / "amz2_coarse_regions.nc", "w") as nc:
        add_lat_lon(nc)
        dims = ("south_north", "west_east")
        var = nc.createVariable("regions", "i4", dims, zlib=True)
        var.long_name, var.coordinates = "source region", "XLONG XLAT"
        var[:] = regions.astype(np.int32)
        var = nc.createVariable("target", "i1", dims, zlib=True)
        var.long_name, var.coordinates = "target region", "XLONG XLAT"
        var[:] = target.astype(np.int8)
        nc.title = "2L-DRM source and target regions"
        nc.source = ("test/regression/make_coarse_case.py of 2L-DRM, in the format of prepro2LDRM-regions "
                     "(https://github.com/erickjomp/2L-DRM-preprocessing)")
        nc.nregions = np.int32(regions.max())
        nc.domsize = np.int32(target.sum())


def main():
    global SRC
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--src", type=Path, default=SRC)
    parser.add_argument("--out", type=Path, default=Path(__file__).parent / "data")
    parser.add_argument("--netcdf-only", action="store_true",
                        help="only write the NetCDF files, from the binary files in --out")
    args = parser.parse_args()
    SRC = args.src
    args.out.mkdir(parents=True, exist_ok=True)
    if args.netcdf_only:
        ny, nx = NY // BLOCK, NX // BLOCK
        write_netcdf(args.out, ny, nx)
        return

    regions = majority(read_static("WRF_amazon.dat"))
    write(args.out / "regions.dat.gz", regions)

    # Target region = coarse cells of region 1: as a mask (--target, 1 inside and 0 outside)
    # and as lists of 1-based (i, j) indices (--itarget, --jtarget).
    write(args.out / "target.dat.gz", (regions == 1).astype("<f4"))
    jj, ii = np.nonzero(regions == 1)
    write(args.out / "target_i.dat.gz", (ii + 1).astype("<f4"))
    write(args.out / "target_j.dat.gz", (jj + 1).astype("<f4"))

    variables = {"PP": 1, "ET": 1, "PW": NSLABS, "U": NSLABS, "V": NSLABS, "PWflux": NSLABS - 1}
    for ifile, (start, count) in enumerate([(0, STEPS_FILE1), (STEPS_FILE1, STEPS_FILE2)], 1):
        for var, nlev in variables.items():
            data = coarsen(read_steps(f"AMZ2_CPM01_{var}.dat", nlev, start, count))
            write(args.out / f"{var}_{ifile}.dat.gz", data)

    ny, nx = regions.shape
    write_netcdf(args.out, ny, nx)
    print(f"nx={nx} ny={ny} dx={DX * BLOCK:g} domsize={ii.size} "
          f"steps={STEPS_FILE1}+{STEPS_FILE2}")


if __name__ == "__main__":
    main()
