
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








