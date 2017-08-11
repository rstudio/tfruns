
#' List training runs
#'
#' @param runs_dir Directory containing runs.
#' @param latest_n Limit to a number of (most recent runs)
#'
#' @return Data frame with `created` (POSIXct) and `run_dir` (path relative to
#'   `runs_dir`).
#'
#' @details
#'
#' The `list_runs()` function returns a data frame. To view the runs pass the
#' data frame to the `View()` function (e.g. `View(list_runs())`).
#'
#' @export
list_runs <- function(runs_dir = "runs", latest_n = NULL) {

  if (missing(runs_dir))
    runs_dir <- environment_runs_dir(default = "runs")

  # default empty run list
  run_list <- data.frame(stringsAsFactors = FALSE,
    created = double(),
    run_dir = character()
  )

  if (file.exists(runs_dir)) {

    # list files in runs_dir
    runs <- list.files(runs_dir,
                       pattern = "\\d{4}-\\d{2}-\\d{2}T\\d{2}-\\d{2}-\\d{2}Z",
                       full.names = FALSE)
    if (length(runs) > 0) {

      # filter and order runs
      runs <- runs[order(runs, decreasing = TRUE)]
      if (!is.null(latest_n))
        runs <- runs[1:min(length(runs),latest_n)]

      # create data frame
      run_list <- data.frame(stringsAsFactors = FALSE,
        created = as.POSIXct(runs, format = "%Y-%m-%dT%H-%M-%SZ"),
        run_dir = file.path(runs_dir, runs)
      )
      run_list <- run_list[order(run_list$created , decreasing = TRUE ),]
    }
  }

  # return run_list
  run_list

}


#' Enumerate recent training runs
#'
#' @inheritParams list_runs
#' @param n Number of recent runs
#'
#' @return Path to run directory or `NULL` if no run
#'   directories were found.
#'
#' @export
latest_run <- function(runs_dir = "runs") {

  if (missing(runs_dir))
    runs_dir <- environment_runs_dir(default = "runs")

  list_runs(runs_dir, latest_n = 1)$run_dir
}


#' @rdname latest_run
#' @export
latest_runs <- function(runs_dir = "runs", n) {

  if (missing(runs_dir))
    runs_dir <- environment_runs_dir(default = "runs")

  list_runs(runs_dir, latest_n = n)$run_dir
}


combine_runs <- function(x, y) {
  x[, c(as.character(setdiff(colnames(y), colnames(x))))] <- NA
  y[, c(as.character(setdiff(colnames(x), colnames(y))))] <- NA
  return(rbind(x, y))
}


