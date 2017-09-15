context("run_data")

source("utils.R")

run_dir <- training_run("write_run_data.R")$run_dir

run_data <- function(...) {
  file.path(run_dir, "tfruns.d", ...)
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
