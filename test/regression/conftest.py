from pathlib import Path

import pytest

import regression


def pytest_addoption(parser):
    parser.addoption("--exe", type=Path, help="path to the run2LDRM executable to test")
    parser.addoption("--update-reference", action="store_true",
                     help="overwrite the reference outputs instead of comparing against them")


@pytest.fixture(scope="session")
def exe(request):
    path = request.config.getoption("--exe")
    if path is None:
        pytest.fail("pass the executable with --exe, e.g. "
                    "--exe $(fpm run run2LDRM --runner echo | tail -1)", pytrace=False)
    if not path.is_file():
        pytest.fail(f"--exe {path} does not exist", pytrace=False)
    return path.resolve()


@pytest.fixture(scope="session")
def update_reference(request):
    return request.config.getoption("--update-reference")


@pytest.fixture(scope="session")
def inputs(tmp_path_factory):
    """Directory with the unpacked case. The model is run from here."""
    workdir = tmp_path_factory.mktemp("amz2_coarse")
    regression.prepare_inputs(workdir)
    return workdir


@pytest.fixture(scope="session")
def run_case(exe, inputs):
    """run_case(case, threads) -> output directory. Each run happens only once."""
    done = {}

    def run(case, threads):
        if (case, threads) not in done:
            done[case, threads] = regression.run_model(
                exe, inputs, f"{case}_threads{threads}", regression.CASES[case], threads)
        outdir, error = done[case, threads]
        if error:
            pytest.fail(f"[{case}, {threads} threads] {error}", pytrace=False)
        return outdir

    return run
