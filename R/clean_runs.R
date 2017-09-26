#' Clean run directories
#'
#' Remove run directories from the filesystem.
#'
#' The `clean_runs()` function moves the specified runs (by default,
#' all runs) into an "archive" subdirectory of the "runs" directory.
#'
#' The `purge_runs()` function permanently deletes the "archive"
#' subdirectory.
#'
#' @inheritParams latest_run
#' @param runs Runs to clean. Can be specified as a data frame
#' (as returned by [ls_runs()]) or as a character vector of
#' run directories.
#' @param confirm `TRUE` to confirm before performing operation
#'
#' @examples \dontrun{
#' clean_runs(ls_runs(completed == FALSE))
#' }
#'
#' @family run management
#'
#' @export
clean_runs <- function(runs = ls_runs(runs_dir = runs_dir),
                       runs_dir = getOption("tfruns.runs_dir", "runs"),
                       confirm = interactive()) {

  # check for a run list that's been specified (otherwise default to all)
  if (!missing(runs))
    run_dirs <- as_run_dir(runs)
  else
    run_dirs <- list_run_dirs(runs_dir = runs_dir)

  # check for no runs
  if (length(run_dirs) == 0) {
    message("No runs found to clean.")
    return(invisible(NULL))
  }

  # compute archive dir
  archive_dir <- file.path(runs_dir, "archive")

  # confirm if requested
  if (confirm) {
    prompt <- readline(sprintf("Move %d run directories to %s? [Y/n]: ",
                               length(run_dirs), archive_dir))
    if (nzchar(prompt) && tolower(prompt) != 'y')
      return(invisible(NULL))
  }

  # move to the archive directory
  if (!utils::file_test("-d", archive_dir))
    dir.create(archive_dir, recursive = TRUE)
  file.rename(run_dirs, file.path(archive_dir, basename(run_dirs)))

  # print message
  message(sprintf('Moved %d runs to %s (purge_runs() to remove permanently)',
                  length(run_dirs), archive_dir))

  # return NULL
  invisible(NULL)
}

#' @rdname clean_runs
#' @export
purge_runs <- function(runs_dir = getOption("tfruns.runs_dir", "runs"),
                       confirm = interactive()) {

  # enumerate dirs
  archive_dir <- file.path(runs_dir, "archive")
  run_dirs <- list_run_dirs(runs_dir = archive_dir)

  # prompt
  if (confirm) {
    prompt <- readline(sprintf("Permanently remove %d run directories from %s? [Y/n]: ",
                               length(run_dirs), archive_dir))
    if (nzchar(prompt) && tolower(prompt) != 'y')
      return(invisible(NULL))
  }

  # remove
  unlink(run_dirs, recursive = TRUE)

  # print message
  message(sprintf("Permanently removed %d runs", length(run_dirs)))

  # return NULL
  invisible(NULL)
}


