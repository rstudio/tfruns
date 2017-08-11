
#' List training runs
#'
#' @param runs_dir Directory containing runs.
#' @param latest_n Limit to a number of (most recent runs)
#'
#' @return A data frame with `created` (POSIXct) and `run_dir` (path relative to
#'   `runs_dir`).
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
    runs <- list_run_dirs(runs_dir, latest_n = latest_n)

    # popuate data frame from runs
    for (run in runs) {
      run_df <- run_record(runs_dir, run)
      run_list <- combine_runs(run_list, run_df)
    }

    # order decreasing
    run_list <- run_list[order(run_list$created , decreasing = TRUE ),]

  }

  # return run_list
  tibble::as_tibble(run_list)
}


#' Enumerate recent training run directories
#'
#' @inheritParams list_runs
#' @param n Number of recent runs
#'
#' @return Character vector with run directory path(s)
#'
#' @export
latest_run <- function(runs_dir = "runs") {
  if (missing(runs_dir))
    runs_dir <- environment_runs_dir(default = "runs")
  latest_runs(runs_dir, n = 1)
}


#' @rdname latest_run
#' @export
latest_runs <- function(runs_dir = "runs", n) {
  if (missing(runs_dir))
    runs_dir <- environment_runs_dir(default = "runs")
  file.path(runs_dir, list_run_dirs(runs_dir, latest_n = n))
}


list_run_dirs <- function(runs_dir, latest_n = NULL) {

  # list directories
  runs <- list.files(runs_dir,
                     pattern = "\\d{4}-\\d{2}-\\d{2}T\\d{2}-\\d{2}-\\d{2}Z",
                     full.names = FALSE)

  # filter and order
  if (length(runs) > 0) {
    runs <- runs[order(runs, decreasing = TRUE)]
    if (!is.null(latest_n))
      runs <- runs[1:min(length(runs),latest_n)]
  }

  # return runs
  runs
}

run_record <- function(runs_dir, run) {

  # compute run name and meta dir
  run_dir <- file.path(runs_dir, run)
  meta_dir <- file.path(run_dir, "tfruns.d")
  props_dir <- file.path(meta_dir, "properties")
  if (!utils::file_test("-d", props_dir))
    props_dir <- NULL

  # function to read a property
  read_property <- function(name) {
    if (!is.null(props_dir)) {
      props_file <- file.path(props_dir, name)
      if (file.exists(props_file))
        readLines(props_file)
      else
        NA
    } else {
      NA
    }
  }

  # function to read columns from a json file
  read_json_columns <- function(file, prefix) {
    json_path <- file.path(meta_dir, file)
    if (file.exists(json_path)) {
      columns <- jsonlite::read_json(json_path)
      names(columns) <- paste0(prefix, "_", names(columns))
      columns
    } else {
      NULL
    }
  }

  # core columns
  columns <- list()
  columns$run_dir <- run

  # flags
  columns <- append(columns, read_json_columns("flags.json", "flag"))

  # metrics
  completed <- TRUE
  metrics_json_path <- file.path(meta_dir, "metrics.json")
  if (file.exists(metrics_json_path)) {
    # read metrics
    metrics <- jsonlite::read_json(metrics_json_path)
    if (length(metrics) > 0) {
      # NA indicates incomple run
      completed <- all(!is.na(metrics[[1]]))
      for (metric in names(metrics)) {
        last_value <- max(which(!is.na(metrics[[metric]])))
        columns[[paste0("metric_", metric)]] <- last_value
      }
    }
  }

  # evaluation
  columns <- append(columns, read_json_columns("evaluation.json", "eval"))

  # completed indicator (from metrics NA values)
  columns$completed <- completed

  # type
  columns$type <- read_property("type")

  columns$created <- as.POSIXct(run, format = "%Y-%m-%dT%H-%M-%SZ")

  # convert to data frame for calls to rbind
  tibble::as_data_frame(columns)
}



combine_runs <- function(x, y) {
  if (nrow(x) == 0)
    y
  else if (nrow(y) == 0)
    x
  else {
    x[, c(as.character(setdiff(colnames(y), colnames(x))))] <- NA
    y[, c(as.character(setdiff(colnames(x), colnames(y))))] <- NA
    rbind(x, y)
  }


}


