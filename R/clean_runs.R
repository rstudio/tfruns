#' Clean run directories
#'
#' Remove run directories from the filesystem.
#'
#' @inheritParams latest_run
#' @param runs Runs to clean
#' @param keep Number of most recent runs to keep when cleaning runs.
#'  This will preserve the number of runs specified even if they
#'  are provided in the list of `runs` to clean.
#' @param confirm `TRUE` to confirm before cleaning the runs.
#'
#' @examples \dontrun{
#' clean_runs(ls_runs(completed == FALSE))
#' clean_runs(keep = 10)
#' }
#' @export
clean_runs <- function(runs = ls_runs(),
                       keep = NULL,
                       confirm = FALSE,
                       runs_dir = getOption("tfruns.runs_dir", "runs")) {

  # check for a run list that's been specified (otherwise default to all)
  if (!missing(runs))
    run_dirs <- as_run_dir(runs)
  else
    run_dirs <- list_run_dirs(runs_dir = runs_dir)

  # limit removal by keep if specified
  if (!is.null(keep) && (keep < length(run_dirs)))
    remove_dirs <- run_dirs[keep+1:length(run_dirs)]
  else
    remove_dirs <- run_dirs

  # confirm if requested
  if ((length(remove_dirs)) > 0 && confirm) {
    prompt <- readline(sprintf("Remove %d run directories? [y/n] ", length(remove_dirs)))
    if (tolower(prompt) != 'y')
      return(invisible(NULL))
  }

  # perform the removal
  unlink(remove_dirs, recursive = TRUE)

  # message
  message(sprintf("Removed %d run directories.", length(remove_dirs)))

  # return NULL
  invisible(NULL)
}
