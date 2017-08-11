
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
  # check global state, if it's null then see if we can bootstrap
  # from a TENSORFLOW_RUN_DIR environment variable
  if (!is.null(.globals$run_dir$path)) {
    TRUE
  } else if (!is.null(environment_run_dir())) {
    initialize_run()
    TRUE
  } else {
    FALSE
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








