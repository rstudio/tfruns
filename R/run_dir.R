
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


#' Create a unique run directory
#'
#' Create a new uniquely named run directory within the specified
#' `runs_dir`.
#'
#' The directory name will be a timestamp (in GMT time). If a
#' duplicate name is generated then the function will wait
#' long enough to return a unique one.
#'
#' @inheritParams ls_runs
#'
#' @export
unique_run_dir <- function(runs_dir = getOption("tfruns.runs_dir", "runs")) {
  while(TRUE) {
    run_dir <- file.path(
      runs_dir,
      paste0(strftime(Sys.time(), format = "%Y-%m-%dT%H-%M-%SZ", tz = "GMT"))
    )
    if (!file.exists(run_dir)) {
      dir.create(run_dir, recursive = TRUE)
      return(run_dir)
    }
    else
      Sys.sleep(0.1)
  }
}


#' Extract run directory from an object
#'
#' @param x Object to extract run directory from
#'
#' @return Run directory path(s)
#'
#' @keywords internal
#'
#' @export
as_run_dir <- function(x) {
  UseMethod("as_run_dir")
}

#' @export
as_run_dir.character <- function(x) {
  x
}

#' @export
as_run_dir.tfruns_run <- function(x) {
  x$run_dir
}

#' @export
as_run_dir.list <- function(x) {
  if (!is.null(x$run_dir))
    x$run_dir
  else
    stop("List does not contain a run_dir")
}

#' @export
as_run_dir.data.frame <- function(x) {
  if (!is.null(x$run_dir))
    x$run_dir
  else
    stop("Data frame does not contain a run_dir")
}











