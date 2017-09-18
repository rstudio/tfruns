context("runs")

run_dir <- training_run()$run_dir

test_that("run dir is created by initialize_run()", {
  expect_true(dir.exists(run_dir))
})

test_that("list runs returns a data frame", {
  runs <- ls_runs()
  expect_true(is.data.frame(runs))
  expect_true(all(c("start", "run_dir") %in% colnames(runs)))
})

test_that("latest_run retreives run_dir", {
  expect_identical(run_dir, latest_run()$run_dir)
})

test_that("completed flag is set by training_run", {
  expect_true(ls_runs(latest_n = 1)$completed)
})


test_that("clean_runs and purge_runs remove runs", {
  # then clean
  clean_runs(confirm = FALSE)
  expect_equal(nrow(ls_runs()), 0)
  expect_true(nrow(ls_runs(runs_dir = "runs/archive")) > 0)

  # then purge
  purge_runs(confirm = FALSE)
  expect_equal(nrow(ls_runs(runs_dir = "runs/archive")), 0)
})

test_that("flags.yml can be passed to training_run", {
  training_run(flags = "flags-learning-rate.yml")
  expect_equal(ls_runs(latest_n = 1)$flag_learning_rate, 0.05)
})

test_that("training errors are handled", {
  tryCatch({
      training_run("train-error.R")
    },
    error = function(e) {
      expect_equal(e$message, "Training error occurred")
    }
  )

  run <- ls_runs(latest_n = 1)

  expect_true(!run$completed)
  expect_equal(run$error_message, "Training error occurred")
})

