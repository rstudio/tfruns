
#' List training runs
#'
#' @param latest_n Limit to a number of (most recent runs)
#' @param project_dir Project to list runs for
#'
#' @return A data frame with `created` (POSIXct) and `run_dir` (path relative to
#'   `runs_dir`).
#'
#' @export
list_runs <- function(latest_n = NULL, project_dir = ".") {

  # compute runs_dir
  runs_dir <- file.path(project_dir, runs_dir())

  # default empty run list
  run_list <- data.frame(stringsAsFactors = FALSE,
    type = character(),
    run_dir = character(),
    start = numeric(),
    end = numeric()
  )

  if (file.exists(runs_dir)) {

    # list runs
    runs <- list_run_dirs(latest_n = latest_n, project_dir = project_dir)

    # popuate data frame from runs
    for (run in runs) {
      run_df <- run_record(run)
      run_list <- combine_runs(run_list, run_df)
    }

    # most recent runs first
    run_list <- run_list[order(run_list$start , decreasing = TRUE ),]

  }

  # convert date columns
  run_list$start <- as.POSIXct(run_list$start, tz = "GMT", origin = "1970-01-01")
  run_list$end <- as.POSIXct(run_list$end, tz = "GMT", origin = "1970-01-01")

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
latest_run <- function(project_dir = ".") {
  latest_runs(n = 1, project_dir = project_dir)
}


#' @rdname latest_run
#' @export
latest_runs <- function(n, project_dir = ".") {
  list_run_dirs(latest_n = n, project_dir = project_dir)
}


list_run_dirs <- function(latest_n = NULL, project_dir = ".") {

  # list directories
  runs <- list.files(file.path(project_dir, runs_dir()),
                     pattern = "\\d{4}-\\d{2}-\\d{2}T\\d{2}-\\d{2}-\\d{2}Z",
                     full.names = FALSE)

  # filter and order
  if (length(runs) > 0) {
    runs <- runs[order(runs, decreasing = TRUE)]
    if (!is.null(latest_n))
      runs <- runs[1:min(length(runs),latest_n)]
  }

  # return runs
  if (identical(project_dir, "."))
    file.path(runs_dir(), runs)
  else
    file.path(project_dir, runs_dir(), runs)
}

run_record <- function(run_dir) {

  # compute run name and meta dir
  run <- basename(run_dir)
  meta_dir <- file.path(run_dir, "tfruns.d")
  props_dir <- file.path(meta_dir, "properties")
  if (!utils::file_test("-d", props_dir))
    props_dir <- NULL

  # read all properties into a list
  read_properties <- function() {
    properties <- list.files(props_dir)
    values <- lapply(properties, function(file) {
      readLines(file.path(props_dir, file))
    })
    names(values) <- properties
    values
  }

  # type converters for properties
  as_type <- function(properties, name, converter) {
    value <- properties[[name]]
    if (is.null(value))
      NULL
    else
      converter(value)
  }
  as_numeric <- function(properties, name) {
    as_type(properties, name, as.numeric)
  }
  as_integer <- function(properties, name) {
    as_type(properties, name, as.integer)
  }
  as_logical <- function(properties, name) {
    as_type(properties, name, function(value) {
      if (value %in% c("TRUE", "true", "yes", "1"))
        value <- TRUE
      else if (value %in% c("FALSE", "false", "no", "0"))
        value <- FALSE
      as.logical(value)
    })
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
  columns$run_dir <- run_dir

  # read properties and do type conversions for known values
  properties <- read_properties()
  properties$start <- as_numeric(properties, "start")
  properties$end <- as_numeric(properties, "end")
  properties$samples <- as_integer(properties, "samples")
  properties$validation_samples <- as_integer(properties, "validation_samples")
  properties$epochs <- as_integer(properties, "epochs")
  properties$batch_size <- as_integer(properties, "batch_size")

  # add properties to columns
  columns <- append(columns, properties)

  # evaluation
  columns <- append(columns, read_json_columns("evaluation.json", "eval"))

  # metrics
  completed <- TRUE
  metrics_json_path <- file.path(meta_dir, "metrics.json")
  if (file.exists(metrics_json_path)) {
    # read metrics
    metrics <- jsonlite::read_json(metrics_json_path)
    if (length(metrics) > 0) {
      for (metric in names(metrics)) {
        last_value <- metrics[[metric]][[max(which(!is.null(metrics[[metric]])))]]
        columns[[paste0("metric_", metric)]] <- last_value
      }
    }
  }

  # flags
  columns <- append(columns, read_json_columns("flags.json", "flag"))

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


