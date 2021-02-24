context("run_data")


run_dir <- with_tests_dir({
  x <- training_run("write_run_data.R", echo = FALSE)$run_dir
  normalizePath(x, winslash = "/")
})


run_data <- function(...) {
  with_tests_dir({
    run_dir <- training_run("write_run_data.R", echo = FALSE)$run_dir
    run_dir <- normalizePath(run_dir, winslash = "/")
    file.path(run_dir, "tfruns.d", ...)
  })
}

expect_run_data <- function(...) {
  expect_true(file.exists(run_data(...)))
}

test_that("flags are written to run_dir", {
  expect_run_data("flags.json")
})

test_that("sources are written to run_dir", {
  expect_run_data("source.tar.gz")
})

test_that("metrics are written to run_dir", {
  expect_run_data("metrics.json")
})

test_that("properites are written to run_dir", {
  expect_run_data("properties", "foo")
  expect_run_data("properties", "index")
  expect_equal(readLines(file.path(run_dir, "tfruns.d", "properties", "foo")), "bar")
})

test_that("custom run data can be written", {
  expect_run_data("foo")
})


test_that("created and modified files are copied to the run_dir", {
  expect_true(file.exists(file.path(run_dir, 'extra.dat')))
  expect_true(file.exists(file.path(run_dir, 'subdir', 'extra.dat')))
})

test_that("precision of metrics", {
  run_dir <- with_tests_dir({
    x <- training_run("flags-precision.R", echo = FALSE)$run_dir
    normalizePath(x, winslash = "/")
  })

  flags <- jsonlite::read_json(
    path = file.path(run_dir, "tfruns.d", "flags.json")
  )

  expect_equal(flags$learning_rate, 2e-5)
  expect_equal(flags$max_steps, 1e-6)

})
