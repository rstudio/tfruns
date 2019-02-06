context("flags")

test_that("flags can be defined", {
  with_tests_dir({
    FLAGS <- define_flags()
    expect_equivalent(FLAGS, readRDS("flags.rds"))
  })
})

test_that("flags are assigned the correct types", {
  with_tests_dir({
    FLAGS <- define_flags()
    expect_type(FLAGS$learning_rate, "double")
    expect_type(FLAGS$max_steps, "integer")
    expect_type(FLAGS$data_dir, "character")
    expect_type(FLAGS$fake_data, "logical")
  })
})

test_that("flags_parse returns defaults when there are no overrides", {
  with_tests_dir({
    FLAGS <- define_flags()
    expect_equivalent(FLAGS, readRDS("flags.rds"))
  })
})

test_that("flags_parse overrides based on command line args", {
  with_tests_dir({
    FLAGS <- define_flags(arguments = c("--learning-rate", "0.02"))
    expect_equal(FLAGS$learning_rate, 0.02)
  })
})

test_that("flags_parse throws an error for unknown command line args", {
  with_tests_dir({
    expect_error({
      FLAGS <- define_flags(arguments = c("--learn-rate", "0.02"))
    })
  })
})

test_that("flags_parse overrides based on config file values", {
  with_tests_dir({
    FLAGS <- define_flags(file = "flags-override.yml")
    expect_equal(FLAGS$learning_rate, 0.02)
    FLAGS <- define_flags(file = "flags-profile-override.yml", config = "myconfig")
    expect_equal(FLAGS$learning_rate, 0.03)
  })
})

test_that("flags_parse skips --args for passthrough args", {
  with_tests_dir({
    FLAGS <- flags(
      flag_numeric("gradient_descent_optimizer", 0.5),
      arguments = list(
        "--gradient-descent-optimizer",
        "0.47",
        "--args",
        "--job-dir",
        "gs://rstudio-cloudml/mnist/staging/2")
    )

    expect_equal(FLAGS$gradient_descent_optimizer, 0.47)
  })
})
