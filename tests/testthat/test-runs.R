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

test_that("flags.yml can be passed to training_run", {
  training_run(flags = "flags-learning-rate.yml")
  expect_equal(list_runs(latest_n = 1)$flag_learning_rate, 0.05)
})

