context("run_data")

source("utils.R")

run_dir <- initialize_run()

run_data <- function(...) {
  file.path(run_dir(), "tfruns.d", ...)
}

expect_run_data <- function(...) {
  expect_true(file.exists(run_data(...)))
}

test_that("flags are written to run_dir", {
  FLAGS <- define_flags()
  expect_run_data("flags.json")
})

test_that("sources are written to run_dir", {
  expect_run_data("source", "utils.R")
})

test_that("metrics are written to run_dir", {
  metrics <- readRDS("metrics.rds")
  write_run_data("metrics", metrics)
  expect_run_data("metrics.json")
})

test_that("custom run data can be written", {
  write_run_data("foo", function(data_dir) {
    file.create(file.path(data_dir, "foo"))
  })
  expect_run_data("foo")
})
