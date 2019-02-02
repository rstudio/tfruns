context("tuning")

source("utils.R")

test_that("tuning_run can execute multiple runs", {
  runs <- tuning_run("write_run_data.R", confirm = FALSE, flags = list(
    learning_rate = c(0.01, 0.02),
    max_steps = c(2500, 500)
  ))

  expect_equal(nrow(runs), 4)
})

test_that("tuning_run can correctly handles interaction of flags flag_grid", {

  # specify only flag_grid
  grid <- expand.grid(
    learning_rate = c(0.01, 0.02),
    max_steps = c(2500, 500, 99)
  )
  runs <- tuning_run("write_run_data.R",
    confirm = FALSE, flags = grid
  )
  expect_equal(nrow(runs), 6)

  # specify none
  expect_error(
    tuning_run("write_run_data.R", confirm = FALSE),
    "flags must be specified as a named list"
  )

  # supply unnamed flags
  # specify both flag_grid and flags
  expect_error(
    tuning_run("write_run_data.R",
      confirm = FALSE,
      flags = list(c(0.01, 0.02), c(2500, 500))
    ),
    "as a named list"
  )
})
