"""Prepare a runnable example: a small, coarsened run over the Amazon.

    python example/prepare_example.py DIRECTORY
    cd DIRECTORY
    run2LDRM --config amz2_coarse.toml

The input data is copied from test/regression/data (also used by the regression
tests, so it is never modified here). Only the Python standard library is needed.
"""
import argparse
import shutil
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "test" / "regression"))
from regression import prepare_inputs  # noqa: E402

CONFIG = "amz2_coarse.toml"


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("directory", type=Path, help="new directory for the example")
    args = parser.parse_args()

    args.directory.mkdir(parents=True, exist_ok=True)
    prepare_inputs(args.directory)
    shutil.copy(HERE / CONFIG, args.directory / CONFIG)

    print(f"Example ready in {args.directory}. Run it with:\n"
          f"    cd {args.directory}\n"
          f"    run2LDRM --config {CONFIG}")


if __name__ == "__main__":
    main()
