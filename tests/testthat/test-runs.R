context("runs")

run_dir <- initialize_run()

test_that("run dir is created by initialize_run()", {
  expect_true(dir.exists(run_dir))
})

test_that("run_dir can read the run_dir", {
  expect_identical(run_dir, run_dir())
})

test_that("list runs returns a data frame", {
  runs <- list_runs()
  expect_true(is.data.frame(runs))
  expect_true(all(c("created", "run_dir") %in% colnames(runs)))
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
  on.exit(Sys.unsetenv("TENSORFLOW_RUN_DIR"), add = TRUE)
  initialize_run()
  expect_identical(run_dir(), Sys.getenv("TENSORFLOW_RUN_DIR"))
})
