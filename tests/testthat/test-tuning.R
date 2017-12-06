context("tuning")

source("utils.R")

test_that("tuning_run can execute multiple runs", {

  runs <- tuning_run("write_run_data.R", confirm = FALSE, flags = list(
    learning_rate = c(0.01, 0.02),
    max_steps = c(2500, 500)
  ))

  expect_equal(nrow(runs), 4)

})
