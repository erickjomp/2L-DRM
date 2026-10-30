# Regression test data — do not modify

Input files of the small coarsened AMZ2 case (32 x 26 cells, 7 days), used by the
regression tests (`test/regression`) and by the example (`example/prepare_example.py`,
which only copies them).

Any change to these files changes the model results, so the regression tests will fail
against the stored references in `test/regression/reference/`. To regenerate them, use
`test/regression/make_coarse_case.py` and update the references with
`--update-reference` in the same commit.
