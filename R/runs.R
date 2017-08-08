

#' Initialize training run directory
#'
#' Timestamped directory for storing training/logging data in a separate
#' location for each training run.
#'
#' The `initialize_run()` function establishes a unique run directory (by default
#' in a sub-directory named "runs") and stores it's value for saving various
#' artifacts of training (e.g. model checkpoints, tensorflow logs, etc.).
#'
#' If you utilize the automatic creation of run directories within the "runs"
#' directory then you can use the `latest_run()` and `latest_runs()` functions
#' to get the path(s) to the most recently created run directories and the
#' `clean_runs()` function to remove previously created run directories.
#'
#' @note You can also establish a run directory by defining the
#'   `TENSORFLOW_RUN_DIR` environment variable (this is technically equivalent
#'   to calling `initialize_run()` within an R script).
#'
#' @param run_dir Path to run directory (`NULL` to automatically create a
#'   timestamped directory within the `runs_dir`)
#' @param runs_dir Parent directory for runs
#' @param quiet `FALSE` to prevent printing the path to the run dir
#'
#' @examples \dontrun{
#' library(tfruns)
#' run_dir <- initialize_run()
#' }
#'
#' @export
initialize_run <- function(run_dir = NULL, runs_dir = "runs", quiet = FALSE) {

  # if no run_dir is specified by the caller then figure out the right value
  if (is.null(run_dir)) {
    run_dir <- environment_run_dir() # check e.g. TENSORFLOW_RUN_DIR
    if (is.null(run_dir))
      run_dir <- unique_dir(runs_dir, format = "%Y-%m-%dT%H-%M-%SZ")
  }

  # create the directory if necessary
  if (!utils::file_test("-d", run_dir))
    if (!dir.create(run_dir, recursive = TRUE))
      stop("Unable to create run directory at ", run_dir)

  # this is new definition for the run_dir, save it
  .globals$run_dir$path <- run_dir

  # execute any pending writes
  for (name in ls(.globals$run_dir$pending_writes))
    .globals$run_dir$pending_writes[[name]](meta_dir(run_dir))

  # show message
  if (!quiet)
    message(paste("Using run directory at:", run_dir))

  # return invisibly
  invisible(run_dir)
}



#' Training run directory
#'
#' Returns the current run directory (if any)
#'
#' @return Current run direcotry (or `NULL` if no run directory is in use)
#'
#' @note You can also establish a run directory by defining the
#'  `TENSORFLOW_RUN_DIR` environment variable (this is technically equivalent
#'  to calling [initialize_run()] within an R script).
#'
#' @export
run_dir <- function() {

  # do we already have a run_dir?
  if (!is.null(.globals$run_dir$path)) {

    .globals$run_dir$path

  # is there an environment variable that could establish a run_dir?
  } else if (!is.null(environment_run_dir())) {

    # set the environment variable as our current run directory
    initialize_run(environment_run_dir())

    # no run_dir currently established
  } else {

    NULL

  }
}


#' Enumerate recent training runs
#'
#' @inheritParams initialize_run
#' @param n Number of recent runs
#'
#' @export
latest_run <- function(runs_dir = "runs") {
  list_runs(runs_dir, latest_n = 1)
}


#' @rdname latest_run
#' @export
latest_runs <- function(runs_dir = "runs", n) {
  list_runs(runs_dir, latest_n = n)
}


#' Clean run directories
#'
#' Remove run directories from the filesystem.
#'
#' @inheritParams initialize_run
#' @param keep Number of most recent runs to keep when cleaning runs.
#'  `NULL` (the default) to remove all previous runs.
#'
#' @examples \dontrun{
#' clean_runs(keep = 10)
#' }
#' @export
clean_runs <- function(runs_dir = "runs", keep = NULL) {
  remove_runs <- list_runs(runs_dir)
  if (!is.null(keep)) {
    if (!is.numeric(keep))
      stop("keep must be a numeric value")
    if (keep >= length(remove_runs))
      invisible(0)
    else
      unlink(remove_runs[keep+1:length(remove_runs)], recursive = TRUE)
  } else {
    unlink(remove_runs, recursive = TRUE)
  }
}


#' Write data into the run directory
#'
#' Provide a function that will write data into the active run
#' directory (if and when it's avaialble).
#'
#' @param name Name of data to write (subsequent writes with the same name
#' will overwrite).
#' @param write_fn Function that writes the data. The function will be
#' passed a single `data_dir` argument.
#'
#' @export
write_run_data <- function(name, write_fn) {
  run_dir <- run_dir()
  if (!is.null(run_dir))
    write_fn(meta_dir(run_dir))
  else
    .globals$run_dir$pending_writes[[name]] <- write_fn
}


# get the meta dir for a run dir
meta_dir <- function(run_dir) {
  meta_dir <- file.path(run_dir, "tfruns.d")
  if (!utils::file_test("-d", meta_dir))
    dir.create(meta_dir, recursive = TRUE)
  meta_dir
}


# check for a run_dir provided by the environment
environment_run_dir <- function() {
  run_dir <- Sys.getenv("TENSORFLOW_RUN_DIR", unset = NA)
  if (!is.na(run_dir))
    run_dir
  else
    NULL
}



have_run_dir <- function() {
  !is.null(run_dir())
}


list_runs <- function(runs_dir = "runs", latest_n = NULL) {
  if (file.exists(runs_dir)) {
    runs <- list.files(runs_dir,
                       pattern = "\\d{4}-\\d{2}-\\d{2}T\\d{2}-\\d{2}-\\d{2}Z",
                       full.names = FALSE)
    if (length(runs) > 0) {
      runs <- runs[order(runs, decreasing = TRUE)]
      if (!is.null(latest_n))
        runs <- runs[1:min(length(runs),latest_n)]
      runs <- data.frame(created = as.POSIXct(runs, format = "%Y-%m-%dT%H-%M-%SZ"),
                         run_dir = file.path(runs_dir, runs),
                         stringsAsFactors = FALSE)
      runs <- runs[order(runs$created , decreasing = TRUE ),]
      runs$run_dir
    } else {
      character()
    }
  }
  else
    character()
}


unique_dir <- function(parent_dir, prefix = NULL, format = "%Y-%m-%dT%H-%M-%SZ") {
  while(TRUE) {
    dir <- file.path(parent_dir,
                     paste0(prefix, strftime(Sys.time(), format = format, tz = "GMT")))
    if (!file.exists(dir))
      return(dir)
    else
      Sys.sleep(0.1)
  }
}






