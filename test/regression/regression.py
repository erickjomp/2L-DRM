"""Case definition and helpers shared by conftest.py and test_regression.py."""
import csv
import gzip
import math
import os
import shutil
import subprocess
import sys
from array import array
from pathlib import Path

HERE = Path(__file__).resolve().parent
CASE_DIR = HERE / "data"
REF_DIR = HERE / "reference"

# Coarse grid written by make_coarse_case.py
NX, NY, DX, DOMSIZE, NREGIONS = 32, 26, 128000, 389, 2

COMMON_ARGS = [
    "-lf", ".true.",
    "-ET", "ET.txt", "-PP", "PP.txt", "-PW", "PW.txt",
    "-U", "U.txt", "-V", "V.txt", "-PWf", "PWflux.txt",
    "-reg", "regions.dat", "-nreg", str(NREGIONS), "-mask", "mask_topo.dat",
    "-it", "target_i.dat", "-jt", "target_j.dat", "-ds", str(DOMSIZE),
    "-nx", str(NX), "-ny", str(NY), "-dx", str(DX), "-dy", str(DX),
    "-d0", "2018-05-01_00:00:00",
    "-d1", "2018-05-04_00:00:00",  # first step with 3 days of history (= mtr * tdt)
    "-d2", "2018-05-07_21:00:00",
    "-tdt", "900", "-mtr", "288",
    "-ddt", "3", "-udt", "3",
    "-ver", ".true.", "-wr", ".true.",
]

# Each case adds the options that differ; together they cover both solvers
# and both next-point methods.
CASES = {
    "solver3_iterative": ["-s", "3", "-next", "1"],
    "solver1_direct": ["-s", "1", "-next", "2"],
}

OUTPUTS = [
    "rho_domain_daily.csv",
    "rho_column_domain_daily.csv",
    "rho_column_domain_timeavg.csv",
    "precipareal_domain_daily.csv",
    "rho_grid.dat",
]

# The CSVs are written with 5-6 decimals; rho_grid.dat has full float32 precision.
CSV_ATOL = 2e-5
GRID_ATOL = 1e-4


def prepare_inputs(workdir):
    """Unzip the case into workdir and write the file lists for -lf .true."""
    for gz in CASE_DIR.glob("*.dat.gz"):
        with gzip.open(gz, "rb") as src, open(workdir / gz.name[:-3], "wb") as dst:
            shutil.copyfileobj(src, dst)
    for var in ["ET", "PP", "PW", "U", "V", "PWflux"]:
        files = sorted(workdir.glob(f"{var}_*.dat"))
        # relative names: the model stores paths in short fixed-length strings
        (workdir / f"{var}.txt").write_text("".join(f"{f.name}\n" for f in files))


def run_model(exe, workdir, outname, extra_args, threads, common_args=COMMON_ARGS):
    """Run run2LDRM in workdir, writing to workdir/outname. Returns (outdir, error)."""
    outdir = workdir / outname
    outdir.mkdir()
    cmd = [str(exe), "-po", outname] + common_args + extra_args
    env = dict(os.environ, OMP_NUM_THREADS=str(threads))
    result = subprocess.run(cmd, cwd=workdir, env=env, capture_output=True, text=True)
    (outdir / "stdout.txt").write_text(result.stdout + result.stderr)
    missing = [f for f in OUTPUTS if not (outdir / f).is_file()]
    if result.returncode != 0 or missing:
        log = (result.stdout + result.stderr)[-3000:]
        return outdir, f"run2LDRM failed (exit {result.returncode}, missing: {missing})\n{log}"
    return outdir, None


def save_reference(output, reference):
    """Write output as the new reference. Returns False if the content was unchanged."""
    is_gz = reference.suffix == ".gz"
    if reference.is_file():
        opener = gzip.open if is_gz else open
        with opener(reference, "rb") as f:
            # gzip bytes vary between Python versions, so compare the content
            if f.read() == output.read_bytes():
                return False
    reference.parent.mkdir(parents=True, exist_ok=True)
    if is_gz:
        with open(output, "rb") as src, open(reference, "wb") as raw, \
                gzip.GzipFile(fileobj=raw, mode="wb", compresslevel=9, mtime=0) as dst:
            shutil.copyfileobj(src, dst)
    else:
        shutil.copy(output, reference)
    return True


def reference_path(case, output):
    name = f"{output}.gz" if output.endswith(".dat") else output
    return REF_DIR / case / name


def read_csv(path):
    with open(path, newline="") as f:
        rows = [[c.strip() for c in row if c.strip()] for row in csv.reader(f)]
    return rows[0], rows[1:]


def compare_csv(new, ref):
    new_header, new_rows = read_csv(new)
    ref_header, ref_rows = read_csv(ref)
    if new_header != ref_header or len(new_rows) != len(ref_rows):
        return [f"shape/header differs: {len(new_rows)} rows vs {len(ref_rows)} reference rows"]
    errors = []
    for irow, (a_row, b_row) in enumerate(zip(new_rows, ref_rows), 2):
        for a, b in zip(a_row, b_row):
            try:
                x, y = float(a), float(b)
            except ValueError:  # datetime labels
                same = a == b
            else:
                same = (math.isnan(x) and math.isnan(y)) or abs(x - y) <= CSV_ATOL
            if not same:
                errors.append(f"line {irow}: {a} vs reference {b}")
    return errors


def read_floats(path, opener=open):
    values = array("f")
    with opener(path, "rb") as f:
        values.frombytes(f.read())
    if sys.byteorder != "little":
        values.byteswap()
    return values


def compare_grid(new, ref):
    a, b = read_floats(new), read_floats(ref, gzip.open)
    if len(a) != len(b):
        return [f"size differs: {len(a)} values vs {len(b)} in reference"]
    bad = [i for i, (x, y) in enumerate(zip(a, b))
           if math.isnan(x) != math.isnan(y) or (not math.isnan(x) and abs(x - y) > GRID_ATOL)]
    if not bad:
        return []
    worst = max((abs(a[i] - b[i]) for i in bad if not math.isnan(a[i] - b[i])), default=math.nan)
    return [f"{len(bad)} of {len(a)} values differ (max abs diff {worst:.3g}), first at index {bad[0]}"]
