context("runs")

run_dir <- training_run()

test_that("run dir is created by initialize_run()", {
  expect_true(dir.exists(run_dir))
})

test_that("list runs returns a data frame", {
  runs <- list_runs()
  expect_true(is.data.frame(runs))
  expect_true(all(c("start", "run_dir") %in% colnames(runs)))
})

test_that("latest_run retreives run_dir", {
  expect_identical(run_dir, latest_run())
})

test_that("clean_runs removes runs", {
  clean_runs()
  expect_length(latest_run(), 0)
})

