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

  remove_runs <- list_runs(runs_dir)$run_dir
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
