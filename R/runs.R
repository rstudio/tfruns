

#' Training run directory
#'
#' Returns the current training run directory. If a training run is
#' not currently active (see [is_run_active()]) then the current
#' working directory is returned.
#'
#' @return Active run direcotry (or current working directory as a fallback)
#'
#' @export
run_dir <- function() {

  # do we already have a run_dir?
  if (is_run_active()) {

    .globals$run_dir$path

  # is there an environment variable that could establish a run_dir?
  } else if (!is.null(environment_run_dir())) {

    # intialize from environment variable
    initialize_run()

    # no run_dir currently established
  } else {

    getwd()

  }
}


#' Check for an active training run
#'
#' @return `TRUE` if a training tun is currently active
#'
#' @export
is_run_active <- function() {
  !is.null(.globals$run_dir$path)
}


#' Enumerate recent training runs
#'
#' @param runs_dir Directory containing runs.
#'
#' @param n Number of recent runs
#'
#' @return Path to run directory or `NULL` if no run
#'   directories were found.
#'
#' @export
latest_run <- function(runs_dir = "runs") {

  if (missing(runs_dir))
    runs_dir <- environment_runs_dir(default = "runs")

  list_runs(runs_dir, latest_n = 1)
}


#' @rdname latest_run
#' @export
latest_runs <- function(runs_dir = "runs", n) {

  if (missing(runs_dir))
    runs_dir <- environment_runs_dir(default = "runs")

  list_runs(runs_dir, latest_n = n)
}


#' Clean run directories
#'
#' Remove run directories from the filesystem.
#'
#' @inheritParams latest_run
#' @param keep Number of most recent runs to keep when cleaning runs.
#'  `NULL` (the default) to remove all previous runs.
#'
#' @examples \dontrun{
#' clean_runs(keep = 10)
#' }
#' @export
clean_runs <- function(runs_dir = "runs", keep = NULL) {

  if (missing(runs_dir))
    runs_dir <- environment_runs_dir(default = "runs")

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


list_runs <- function(runs_dir = "runs", latest_n = NULL) {

  if (missing(runs_dir))
    runs_dir <- environment_runs_dir(default = "runs")

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


initialize_run <- function(runs_dir = "runs", flags = NULL) {

  # clear any existing run
  clear_run()

  # get the runs dir and run dir from the environment (if available)
  runs_dir <- environment_runs_dir(default = runs_dir)
  run_dir <- environment_run_dir(default = unique_dir(runs_dir,
                                                      format = "%Y-%m-%dT%H-%M-%SZ"))

  # create the directory if necessary
  if (!utils::file_test("-d", run_dir))
    if (!dir.create(run_dir, recursive = TRUE))
      stop("Unable to create run directory at ", run_dir)

  # this is new definition for the run_dir, save it
  .globals$run_dir$path <- run_dir

  # save flags (they'll get processed later in flags())
  .globals$run_dir$flags <- flags

  # write source files
  write_run_metadata("source", getwd())

  # execute any pending writes
  for (name in ls(.globals$run_dir$pending_writes))
    .globals$run_dir$pending_writes[[name]](meta_dir(run_dir))

  # return invisibly
  invisible(run_dir)

}

clear_run <- function() {
  .globals$run_dir$path <- NULL
  .globals$run_dir$flags <- NULL
  .globals$run_dir$pending_writes <- new.env(parent = emptyenv())
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

# check for a runs_dir provided by the environment
environment_runs_dir <- function(default = NULL) {
  runs_dir <- Sys.getenv("TENSORFLOW_RUNS_DIR", unset = NA)
  if (!is.na(runs_dir))
    runs_dir
  else
    default
}

# check for a run_dir provided by the environment
environment_run_dir <- function(default = NULL) {
  run_dir <- Sys.getenv("TENSORFLOW_RUN_DIR", unset = NA)
  if (!is.na(run_dir))
    run_dir
  else
    default
}








