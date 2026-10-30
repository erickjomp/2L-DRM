"""Build the small regression-test case from the AMZ2 CPM 4 km inputs.

The original inputs (1055 x 863 cells, 3-hourly, ~120 GB) are coarsened by
averaging BLOCK x BLOCK cells and cut to the first few days of May 2018.
The result is written gzipped to ``data/`` and committed to the repo.

Only needed to regenerate the case (requires access to GLADE and numpy):

    python make_coarse_case.py [--src DIR] [--out DIR]

All binary files are raw float32 in Fortran order: (nx, ny, nslabs, nt).
"""
import argparse
import gzip
from pathlib import Path

import numpy as np

SRC = Path("/glade/u/home/erickkc2/uiuc0017/2LDRM_AMZ2/01_inputs/CPM_2levels500")
NX, NY, NSLABS = 1055, 863, 2
BLOCK = 32
DX = 4000.0

# Time window (3-hourly steps from 2018-05-01 00:00). Split into two files so
# the test also covers loading a second file while keeping the tracing history.
STEPS_FILE1 = 32  # 2018-05-01 00:00 .. 2018-05-04 21:00
STEPS_FILE2 = 24  # 2018-05-05 00:00 .. 2018-05-07 21:00


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


def main():
    global SRC
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--src", type=Path, default=SRC)
    parser.add_argument("--out", type=Path, default=Path(__file__).parent / "data")
    args = parser.parse_args()
    SRC = args.src
    args.out.mkdir(parents=True, exist_ok=True)

    regions = majority(read_static("WRF_amazon.dat"))
    write(args.out / "regions.dat.gz", regions)
    write(args.out / "mask_topo.dat.gz", majority(read_static("mask_topo_2levels500.dat")))

    # Target region = coarse cells of region 1, as 1-based (i, j) indices.
    jj, ii = np.nonzero(regions == 1)
    write(args.out / "target_i.dat.gz", (ii + 1).astype("<f4"))
    write(args.out / "target_j.dat.gz", (jj + 1).astype("<f4"))

    variables = {"PP": 1, "ET": 1, "PW": NSLABS, "U": NSLABS, "V": NSLABS, "PWflux": NSLABS - 1}
    for ifile, (start, count) in enumerate([(0, STEPS_FILE1), (STEPS_FILE1, STEPS_FILE2)], 1):
        for var, nlev in variables.items():
            data = coarsen(read_steps(f"AMZ2_CPM01_{var}.dat", nlev, start, count))
            write(args.out / f"{var}_{ifile}.dat.gz", data)

    ny, nx = regions.shape
    print(f"nx={nx} ny={ny} dx={DX * BLOCK:g} domsize={ii.size} "
          f"steps={STEPS_FILE1}+{STEPS_FILE2}")


if __name__ == "__main__":
    main()
