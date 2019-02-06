context("copy")


# context 3 fake training runs (one of which that generates files)

test_that("can create training runs",{
  with_tests_dir({
    lr <- training_run(view = FALSE, echo = FALSE)
    expect_is(lr, "data.frame")
  })
  with_tests_dir({
    training_run(view = FALSE, echo = FALSE)
    expect_is(lr, "data.frame")
  })
  with_tests_dir({
    training_run("write_run_data.R", view = FALSE, echo = FALSE)
    expect_is(lr, "data.frame")
  })

  with_tests_dir({
    lr <- latest_run()
    expect_is(lr, "tfruns_run")
  })
})

test_that("copy_run copies run directory", {
  with_tests_dir({
    copy_run(latest_run(), rename = "copied-run-dir")
    expect_true(file.exists(file.path("copied-run-dir", "subdir", "extra.dat")))
    expect_true(file.exists(file.path("copied-run-dir", "tfruns.d", "source.tar.gz")))
    unlink("copied-run-dir", recursive = TRUE)
  })
})

test_that("copy_run_files copies run artifacts", {
  with_tests_dir({
    copy_run_files(latest_run(), rename = "run-artifacts")
    expect_true(file.exists(file.path("run-artifacts", "extra.dat")))
    unlink("run-artifacts", recursive = TRUE)
  })
})

test_that("copy_runs successfully copies run directories", {
  with_tests_dir({
    last_3_runs <- ls_runs()[1:3,]
    copy_run(last_3_runs, "last-three")
    expect_true(all(
      utils::file_test(
        "-d", file.path("last-three", basename(as_run_dir(last_3_runs))))
    ))
    unlink("last-three", recursive = TRUE)
  })
})







