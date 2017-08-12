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
clean_runs <- function(keep = NULL, runs_dir = "runs") {
  all_runs <- file.path(runs_dir, list_run_dirs(runs_dir))
  if (!is.null(keep)) {
    if (!is.numeric(keep))
      stop("keep must be a numeric value")
    if (keep >= length(all_runs))
      invisible(0)
    else
      unlink(all_runs[keep+1:length(all_runs)], recursive = TRUE)
  } else {
    unlink(all_runs, recursive = TRUE)
  }
}
