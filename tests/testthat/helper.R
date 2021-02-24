with_tests_dir <- function(code){
  test_dir <- testthat::test_path()
  withr::with_dir(test_dir, code)
}


define_flags <- function(...) {
  flags(
    flag_numeric('learning_rate', 0.01, 'Initial learning rate.'),
    flag_integer('max_steps', 5000, 'Number of steps to run trainer.'),
    flag_string('data_dir', 'MNIST-data', 'Directory for training data'),
    flag_boolean('fake_data', FALSE, 'If true, use fake data for testing'),
    ...
  )
}


expect_success <- function(expr) {
  expect_error(force(expr), NA)
}
