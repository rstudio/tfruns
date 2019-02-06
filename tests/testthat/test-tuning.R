context("tuning")

test_that("tuning_run can execute multiple runs", {

  runs <- with_tests_dir({
    tuning_run("write_run_data.R",
               confirm = FALSE,
               flags = list(
                 learning_rate = c(0.01, 0.02),
                 max_steps = c(2500, 500)
               ),
               echo = FALSE
    )
  })

  expect_equal(nrow(runs), 4)

})


# tear down
runs_dirs <- with_tests_dir(normalizePath(list.dirs("runs", recursive = FALSE)))
unlink(runs_dirs, recursive = TRUE)
