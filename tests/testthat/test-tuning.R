context("tuning")


test_that("tuning_run throws graceful errors with wrong sample argument", {

  with_tests_dir({
    expect_error(
      tuning_run("write_run_data.R",
                 confirm = FALSE,
                 flags = list(
                   learning_rate = c(0.01, 0.02),
                   max_steps = c(2500, 500)
                 ),
                 sample = 1.1,
                 echo = FALSE
      )
    )
  })

  with_tests_dir({
    expect_error(
      tuning_run("write_run_data.R",
                 confirm = FALSE,
                 flags = list(
                   learning_rate = c(0.01, 0.02),
                   max_steps = c(2500, 500)
                 ),
                 sample = 0,
                 echo = FALSE
      )
    )
  })

  runs <- with_tests_dir({
    tuning_run("write_run_data.R",
               confirm = FALSE,
               flags = list(
                 learning_rate = c(0.01, 0.02),
                 max_steps = c(2500, 500)
               ),
               sample = 1e-6,
               echo = FALSE
    )
  })
  expect_equal(nrow(runs), 1)

})

test_that("tuning_run can execute multiple runs", {

  runs <- with_tests_dir({
    tuning_run("write_run_data.R",
               confirm = FALSE,
               flags = list(
                 learning_rate = c(0.01, 0.02),
                 max_steps = c(2500, 500)
               ),
               sample = 1,
               echo = FALSE
    )
  })

  expect_equal(nrow(runs), 4)
})

test_that("tuning_run can correctly handle different types of inputs for flags", {

  # specify flags as data frame
  grid <- expand.grid(
    learning_rate = c(0.01, 0.02),
    max_steps = c(2500, 500, 99)
  )
  runs <- with_tests_dir(tuning_run("write_run_data.R",
    confirm = FALSE, flags = grid
  ))
  expect_equal(nrow(runs), 6)

  # specify none
  expect_error(
    with_tests_dir(tuning_run("write_run_data.R", confirm = FALSE),
    "flags must be specified as a named list"
  ))

  # supply unnamed flags
  expect_error(
    tuning_run("write_run_data.R",
      confirm = FALSE,
      flags = list(c(0.01, 0.02), c(2500, 500))
    ),
    "as a named list"
  )
})


# tear down
runs_dirs <- with_tests_dir(normalizePath(list.dirs("runs", recursive = FALSE)))
unlink(runs_dirs, recursive = TRUE)
