context("runs")

run_dir <- use_run_dir()

test_that("run dir is created by use_run_dir", {
  expect_true(dir.exists(run_dir))
})

test_that("run_dir can read the run_dir", {
  expect_identical(run_dir, run_dir())
})

test_that("latest_run retreives run_dir", {
  expect_identical(run_dir, latest_run())
})

test_that("clean_runs removes runs", {
  clean_runs()
  expect_length(latest_run(), 0)
})

test_that("environent variable can establish a run directory", {
  Sys.setenv(TENSORFLOW_RUN_DIR = tempdir())
  use_run_dir()
  expect_identical(run_dir(), Sys.getenv("TENSORFLOW_RUN_DIR"))
})
