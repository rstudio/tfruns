
#' List or view training runs
#'
#' @param subset Logical expression indicating rows to keep (missing values are
#'   taken as false). See [subset()].
#' @param order Columns to order by (defaults to run start time)
#' @param decreasing `TRUE` to use decreasing order (e.g. list most recent runs
#'   first)
#' @param latest_n Limit query to the `latest_n` most recent runs
#' @param runs_dir Directory containing runs. Defaults to "runs" beneath the
#'   current working directory (or to the value of the `tfruns.runs_dir` R
#'   option if specified).
#'
#' @return Data frame with training runs
#'
#' @details When printing the results of `ls_runs()`, only `run_dir`,
#' `metric_loss`, `metric_val_loss`,  and any columns specified in `order` will
#' be printed.
#'
#' To view all fields, use `View(ls_runs())`.
#'
#' @export
ls_runs <- function(subset = NULL,
                    order = "start",
                    decreasing = TRUE,
                    latest_n = NULL,
                    runs_dir = getOption("tfruns.runs_dir", "runs")) {

  # default empty run list
  run_list <- NULL

  if (utils::file_test("-d", runs_dir)) {

    # list runs
    runs <- list_run_dirs(latest_n = latest_n, runs_dir = runs_dir)

    # popuate data frame from runs
    for (run in runs) {
      run_df <- run_record(run)
      if (is.null(run_list))
        run_list <- run_df
      else
        run_list <- combine_runs(run_list, run_df)
    }
  }

  if (!is.null(run_list)) {

    # resolve order
    order <- tidyselect::vars_select(colnames(run_list), !! rlang::enquo(order))

    # build order expression
    order_cols <- paste(paste0("run_list$", order), collapse = ",")
    order_expr <- sprintf("run_list[order(%s, decreasing = %s),]",
                          order_cols, deparse(decreasing))
    run_list <- eval(parse(text = order_expr))

    # convert date columns
    as_date <- function(value) {
      if (!is.null(value))
        as.POSIXct(value, tz = "GMT", origin = "1970-01-01")
      else
        NULL
    }
    run_list$start <- as_date(run_list$start)
    run_list$end <- as_date(run_list$end)
    run_list$cloudml_created <- as_date(run_list[["cloudml_created"]])
    run_list$cloudml_start <- as_date(run_list[["cloudml_start"]])
    run_list$cloudml_end <- as_date(run_list[["cloudml_end"]])

  } else {
    run_list <- data.frame(
      type = character(),
      run_dir = character(),
      start = numeric(),
      end = numeric(),
      stringsAsFactors = FALSE
    )
  }

  # apply subset
  if (!missing(subset)) {
    subset_call <- substitute(subset)
    rows <- eval(subset_call, run_list)
    run_list <- run_list[rows, ]
    rownames(run_list) <- seq(length = nrow(run_list))
  }

  # return runs
  return_runs(run_list, order)
}



#' Latest training run
#'
#' @inheritParams ls_runs
#'
#' @return Named list with run attributes (or `NULL` if no runs found)
#'
#' @export
latest_run <- function(runs_dir = getOption("tfruns.runs_dir", "runs")) {
  latest_run_df <- ls_runs(latest_n = 1, runs_dir = runs_dir)
  if (nrow(latest_run_df) > 0) {
    as_run_info(latest_run_df)
  } else {
    NULL
  }
}


#' Summary of training run
#'
#' @param run_dir Training run directory or data frame returned from
#'   [ls_runs()].
#'
#' @return Training run summary object with timing, flags, model info, training
#'   and evaluation metrics, etc. If more than one `run_dir` is passed then
#'   a list of training run summary objects is returned.
#'
#' @seealso [view_run()]
#'
#' @export
run_info <- function(run_dir) {
  run_dir <- as_run_dir(run_dir)
  if (length(run_dir) == 0)
    list()
  else if (length(run_dir) == 1)
    as_run_info(run_record(run_dir))
  else
    lapply(run_dir, function(dir) {
      as_run_info(run_record(dir))
    })
}


#' @importFrom utils str
#'
#' @export
print.tfruns_run <- function(x, ...) {

  # summarize longer fields
  summarize <- function(field, summary) {
    if (!is.null(x[[field]]))
      summary
    else
      NULL
  }
  x$metrics <- summarize("metrics", "(metrics data frame)")
  x$model <- summarize("model", "(model summary)")
  x$source_code <- summarize("source_code", "(source archive)")
  x$output <- summarize("output", "(script ouptut)")

  # print
  str(x, no.list = TRUE)
}

#' @export
print.tfruns_model_summary <- function(x, ...) {
  cat(x)
}

#' @export
print.tfruns_runs_df <- function(x, ...) {
  cols <- colnames(x)
  if (nrow(x) == 0) {
    cat("No training runs found.\n")
  } else if (nrow(x) == 1) {
    print(as_run_info(x))
  } else  {
    # if no subsetting of columns has taken place then use default display
    if (identical(colnames(x), attr(x, "original_cols"))) {

      # calculate and apply display columns
      order_cols <- attr(x, "order")
      eval_cols <- cols[grepl("^eval_", cols)]
      metric_cols <- cols[grepl("^metric_", cols)]
      display_cols <- unique(c("run_dir", order_cols, eval_cols, metric_cols))
      display_cols <- intersect(display_cols, cols)
      x <- x[, display_cols, drop = FALSE]

      # print extra cols
      extra_cols <- cols[!is.element(cols, display_cols)]
      if (length(extra_cols) > 0) {
        extra_output <- paste0(extra_cols, collapse = ", ")
        extra_output <- paste(
          paste("#  ", strwrap(extra_output)),
          collapse = "\n"
        )
        extra_output <- paste0(
          "# ... with ", length(extra_cols), " more columns:\n",
          extra_output
        )
      }
    } else {
      extra_output <- NULL
    }

    # see if there are extra rows
    original_rows <- nrow(x)
    x <- utils::head(x, n = 10)
    extra_rows <- original_rows - nrow(x)
    if (extra_rows > 0) {
      extra_output <- paste0("# ... with ", extra_rows, " more rows\n",
                             extra_output)
    }

    # print header
    cat(sprintf("Data frame: %d x %d", original_rows, length(cols)), "\n")

    # print with default df method
    cls <- class(x)
    cls <- cls[cls != "tfruns_runs_df"]
    class(x) <- cls
    print(x)

    # print extra output
    if (!is.null(extra_output))
      cat(extra_output)
  }
}


list_run_dirs <- function(latest_n = NULL, runs_dir) {

  # return empty character vector for dir not found
  if (!utils::file_test("-d", runs_dir))
    return(character())

  # list directories
  runs <- list.files(runs_dir, full.names = FALSE)
  runs <- runs[utils::file_test("-d", file.path(runs_dir, runs, "tfruns.d"))]

  # filter and order
  if (length(runs) > 0) {
    runs <- runs[order(runs, decreasing = TRUE)]
    if (!is.null(latest_n))
      runs <- runs[1:min(length(runs),latest_n)]
  }

  # return runs
  file.path(runs_dir, runs)
}

run_record <- function(run_dir) {

  # validate that it exists
  if (!utils::file_test("-d", run_dir))
    stop("Run directory ", run_dir, " does not exist", call. = FALSE)

  # compute run name and meta dir
  run <- basename(run_dir)
  meta_dir <- file.path(run_dir, "tfruns.d")
  props_dir <- file.path(meta_dir, "properties")
  if (!utils::file_test("-d", props_dir))
    props_dir <- NULL

  # read all properties into a list
  read_properties <- function() {
    if (!is.null(props_dir) && file.exists(props_dir)) {
      properties <- list.files(props_dir)
      values <- lapply(properties, function(file) {
        paste(readLines(file.path(props_dir, file)), collapse = "\n")
      })
      names(values) <- properties

      # default 'type' and 'context' (data migration)
      if (is.null(values$type) || identical(values$type, 'local'))
        values$type <- 'training'
      if (is.null(values$context))
        values$context <- 'local'

      # return values
      values
    } else {
      list()
    }
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
      if (length(columns) > 0) {
        names(columns) <- paste0(prefix, "_", names(columns))
      }
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
  for (unit in valid_steps_units)
    properties[[unit]] <- as_integer(properties, unit)
  properties$batch_size <- as_integer(properties, "batch_size")
  properties$completed <- as_logical(properties, "completed")
  properties$learning_rate <- as_numeric(properties, "learning_rate")
  properties$cloudml_created <- as_integer(properties, "cloudml_created")
  properties$cloudml_start <- as_integer(properties, "cloudml_start")
  properties$cloudml_end <- as_integer(properties, "cloudml_end")
  properties$cloudml_ml_units <- as_numeric(properties, "cloudml_ml_units")

  # add properties to columns
  columns <- append(columns, properties)

  # evaluation
  columns <- append(columns, read_json_columns("evaluation.json", "eval"))

  # metrics
  epochs_completed <- 0L
  metrics_json_path <- file.path(meta_dir, "metrics.json")
  if (file.exists(metrics_json_path)) {
    # read metrics
    metrics <- jsonlite::read_json(metrics_json_path, simplifyVector = TRUE)
    if (length(metrics) > 0) {
      for (metric in names(metrics)) {
        if (metric == "epoch")
          next
        values <- metrics[[metric]]
        available_values <- values[!is.na(values)]
        epochs_completed <- length(available_values)
        if (epochs_completed > 0) {
          last_value <- available_values[[epochs_completed]]
          columns[[paste0("metric_", metric)]] <- last_value
        }
      }
    }
  }

  steps_completed_unit <- get_steps_completed_unit(get_steps_unit(columns))
  # epochs completed
  columns[[steps_completed_unit]] <- epochs_completed

  # flags
  columns <- append(columns, read_json_columns("flags.json", "flag"))

  # error
  error_json_path <- file.path(meta_dir, "error.json")
  if (file.exists(error_json_path)) {
    error <- jsonlite::read_json(error_json_path, simplifyVector = TRUE)
    columns[["error_message"]] <- error$message
    columns[["error_traceback"]] <- paste(error$traceback, collapse = "\n")
  }


  # add metrics and source fields
  meta_dir <- meta_dir(run_dir, create = FALSE)
  metrics_json <- file.path(meta_dir, "metrics.json")
  if (file.exists(metrics_json))
    columns$metrics <- metrics_json
  source_code <- file.path(meta_dir, "source.tar.gz")
  if (file.exists(source_code))
    columns$source_code <- source_code

  # convert to data frame for calls to rbind
  as.data.frame(columns, stringsAsFactors = FALSE)
}

combine_runs <- function(x, y) {
  x[, c(as.character(setdiff(colnames(y), colnames(x))))] <- NA
  y[, c(as.character(setdiff(colnames(x), colnames(y))))] <- NA
  rbind(x, y)
}

return_runs <- function(runs, order = NULL) {

  # re-order columns
  select_cols <- function(cols) {
    intersect(cols, colnames(runs))
  }
  cols_with_prefix <- function(prefix) {
    cols <- colnames(runs)
    cols[grepl(paste0("^", prefix, "_"), cols)]
  }
  cols <- character()
  cols <- c(cols, cols_with_prefix("eval"))
  cols <- c(cols, cols_with_prefix("metric"))
  cols <- c(cols, cols_with_prefix("flag"))
  cols <- c(cols, select_cols(c("samples", "validation_samples")))
  cols <- c(cols, select_cols(c("batch_size")))
  for (unit in valid_steps_units)
    cols <- c(cols, select_cols(c(unit, paste0(unit, "_completed"))))
  cols <- c(cols, select_cols(c("metrics")))
  cols <- c(cols, select_cols(c("model", "loss_function", "optimizer", "learning_rate")))
  cols <- c(cols, select_cols(c("script", "source")))
  cols <- c(cols, select_cols(c("start", "end", "completed")))
  cols <- c(cols, select_cols(c("output", "error_message", "error_traceback")))
  cols <- c(cols, select_cols(c("source_code")))
  cols <- c(cols, select_cols(c("context", "type")))
  cols <- c(cols, setdiff(colnames(runs), cols))

  # promote any ordered columns to the front
  if (identical(unname(order), "start"))
    order <- NULL
  initial_cols <- c(select_cols(c("run_dir")), order)
  cols <- setdiff(cols, initial_cols)
  cols <- c(initial_cols, cols)

  # re-order cols (always have type and run_dir at the beginning)
  runs <- runs[, cols]

  # apply special class and add order attribute
  class(runs) <- c("tfruns_runs_df", class(runs))
  attr(runs, "order") <- order
  attr(runs, "original_cols") <- colnames(runs)

  # return runs
  runs
}

as_run_info <- function(run_record) {

  # re-order columns
  runs <- return_runs(run_record)

  # convert to list
  run_info <- list()
  for (col in colnames(runs))
    run_info[[col]] <- runs[[col]]

  # give the model a class that will make it print nicely
  if (!is.null(run_info$model))
    class(run_info$model) <- c("tfruns_model_summary", "character")

  # convert metrics to data frame
  if (!is.null(run_info$metrics) && file.exists(run_info$metrics)) {
    run_info$metrics <- as.data.frame(
      jsonlite::read_json(run_info$metrics, simplifyVector = TRUE))
  }

  # return with class
  structure(class = "tfruns_run", run_info)
}

run_source_code <- function(script, run_dir) {
  source_tarball <- file.path(meta_dir(run_dir), "source.tar.gz")
  if (file.exists(source_tarball)) {
    source_tmp_dir <- tempfile()
    on.exit(unlink(source_tmp_dir, recursive = TRUE))
    utils::untar(source_tarball, exdir = source_tmp_dir, compressed = TRUE)
    source_dir <- file.path(source_tmp_dir, "source")
    source_files <- list.files(source_dir, recursive = TRUE)
    source_files <- c(script, source_files[!grepl(paste0("^", script, "$"), source_files)])
    names(source_files) <- source_files
    lapply(source_files, function(file) {
      paste(readLines(file.path(source_dir, file)), collapse = "\n")
    })
  } else {
    list()
  }
}

valid_steps_units <- c("steps", "epochs")

get_steps_unit <- function(run) {
  steps_unit <- "steps"
  for (unit in valid_steps_units) {
    if (!is.null(run[[unit]])) {
      steps_unit <- unit
      break
    }
  }
  steps_unit
}

get_steps_completed_unit <- function(steps_unit) {
  paste0(steps_unit, "_completed")
}

