#' Run a training script
#'
#' @inheritParams  flags
#' @param file Path to training script (defaults to "train.R")
#' @param type Run type (defaults to "local")
#' @param flags Named character vector with flag values (see [flags()]) or path
#'   to YAML file containing flag values.
#' @param properties Named character vector with run properties. Properties are
#'   additional metadata about the run which will be subsequently available via
#'   [ls_runs()].
#' @param run_dir Directory to store run data within
#' @param echo Print expressions within training script
#' @param envir The environment in which the script should be evaluated
#' @param encoding The encoding of the training script; see [file()].
#'
#' @return Single row data frame with run flags, metrics, etc.
#'
#' @export
training_run <- function(file = "train.R",
                         type = "local",
                         config = Sys.getenv("R_CONFIG_ACTIVE", unset = "default"),
                         flags = NULL,
                         properties = NULL,
                         run_dir = NULL,
                         echo = FALSE,
                         envir = parent.frame(),
                         encoding = getOption("encoding")) {

  # verify that the file exists
  if (!file.exists(file))
    stop("The specified R script '", file, "' does not exist.")

  # setup run context
  run_dir <- initialize_run(
    type = type,
    config = config,
    flags = flags,
    properties = properties,
    run_dir = run_dir
  )

  # execute the training run
  do_training_run(file, run_dir, echo = echo, envir = envir, encoding = encoding)

  # return the run invisibly
  invisible(return_runs(run_record(run_dir)))
}


do_training_run <- function(file, run_dir, echo, envir, encoding) {

  # write script
  write_run_property("script", basename(file))

  # write begin and end times
  write_run_property("start", as.double(Sys.time()))
  on.exit(write_run_property("end", as.double(Sys.time())), add = TRUE)

  # clear run on exit
  on.exit(clear_run(), add = TRUE)

  # notify user of run dir
  message("Using run directory ", run_dir)

  # perform the run
  write_run_property("completed", FALSE)
  withCallingHandlers({
      source(file = file, local = envir, echo = echo, encoding = encoding)
      write_run_property("completed", TRUE)
    },
    error = function(e) {
      write_run_property("error", e$message)
      stop(e)
    }
  )
}

initialize_run <- function(type = "local",
                           config = Sys.getenv("R_CONFIG_ACTIVE", unset = "default"),
                           flags = NULL,
                           properties = NULL,
                           run_dir = NULL) {

  # clear any existing run
  clear_run()

  # generate the run_dir
  if (is.null(run_dir))
    run_dir <- unique_run_dir()

  # create the directory if necessary
  if (!utils::file_test("-d", run_dir))
    if (!dir.create(run_dir, recursive = TRUE))
      stop("Unable to create run directory at ", run_dir)

  # if flags is a YAML file then read the flags from the file
  if (is.character(flags) && length(flags) == 1 &&
      is.null(names(flags)) && file.exists(flags)) {
    flags_file <- flags
    flags <- NULL
  } else {
    flags_file <- NULL
  }

  # this is new definition for the run_dir, save it
  .globals$run_dir$path <- run_dir

  # save config and flags (they'll get processed later in flags())
  .globals$run_dir$config <- config
  .globals$run_dir$flags <- flags
  .globals$run_dir$flags_file <- flags_file

  # write type
  write_run_metadata("properties", list(type = type))

  # write properties
  write_run_metadata("properties", properties)

  # write source files
  write_run_metadata("source", getwd())

  # execute any pending writes
  for (name in ls(.globals$run_dir$pending_writes))
    .globals$run_dir$pending_writes[[name]](meta_dir(run_dir))

  # return invisibly
  invisible(run_dir)
}

clear_run <- function() {
  .globals$run_dir$path <- NULL
  .globals$run_dir$config <- NULL
  .globals$run_dir$flags <- NULL
  .globals$run_dir$flags_file <- NULL
  .globals$run_dir$pending_writes <- new.env(parent = emptyenv())
}


#' View a training run
#'
#' View metrics and other attributes of a training run.
#'
#' @inheritParams run_info
#' @param viewer Viewer to display training run information within
#'   (default to an internal page viewer if available, otherwise
#'   to the R session default web browser).
#'
#' @seealso [ls_runs()], [run_info()]
#'
#' @export
view_run <- function(run_dir = latest_run(), viewer = getOption("tfruns.viewer")) {

  # verify run_dir
  if (is.null(run_dir))
    stop("No runs available in the current directory")

  # get run info
  run <- run_info(run_dir)

  # helper to extract prefaced properties
  with_preface <- function(preface, strip_preface = TRUE) {
    preface_pattern <- paste0("^", preface, "_")
    prefaced <- run[grepl(preface_pattern, names(run))]
    if (length(prefaced) > 0) {
      if (strip_preface)
        names(prefaced) <- sub(preface_pattern, "", names(prefaced))
      prefaced
    } else {
      NULL
    }
  }

  # helpers for formatting numbers
  format_integer <- function(x) {
    prettyNum(x, mode = "integer", big.mark = ",")
  }
  format_numeric <- function(x, digits = 4) {
    formatC(x, format='f', digits= digits)
  }

  # default some potentially empty sections to null
  data <- list(
    metrics = NULL,
    evaluation = NULL,
    flags = NULL
  )

  # tabs
  data$tabs <- list(
    list(href = "#summary", title = "Summary"),
    list(href = "#code", title = "Code"),
    list(href = "#tensorboard", title = "TensorBoard")
  )

  # run_dir
  data$run_dir <- run$run_dir

  # attributes
  data$attributes <- list(
    type = run$type,
    script = basename(run$script),
    run_dir = run$run_dir,
    started = paste(as.POSIXct(run$start, origin="1970-01-01", tz = "GMT"),
                    "GMT"),
    time = format(as.POSIXct(as.character(Sys.Date()), tz = "GMT") +
                  run$end - run$start,
                  "%H:%M:%S")
  )

  # metrics
  metrics <- with_preface("metric")
  if (!is.null(metrics))
    data$metrics <- lapply(metrics, format_numeric)

  # evaluation
  evaluation <- with_preface("eval", strip_preface = FALSE)
  if (!is.null(evaluation))
    data$evaluation <- evaluation

  # flags
  flags <- with_preface("flags")
  if (!is.null(flags))
    data$flags <- flags

  # optimization
  data$optimization <- list(
    loss = run$loss_function,
    optimizer = run$optimizer,
    learning_rate = run$learning_rate
  )

  # training
  if (run$epochs > run$epochs_completed)
    epochs <- paste(format_integer(run$epochs_completed),
                    format_integer(run$epochs),
                    sep = "/")
  else
    epochs <- format_integer(run$epochs)
  data$training <- list(
    samples = format_integer(run$samples),
    epochs = epochs,
    batch_size = format_integer(run$batch_size)
  )

  data$history <- run$metrics
  data$model <- sub("^Model\n", "", run$model)
  data$model <- sub("^_+\n", "", data$model)

  view_page("view_run", data, viewer)
}


#' Compare training runs
#'
#' Render a visual comparison of two training runs.
#'
#' @inheritParams view_run
#'
#' @param runs @param run_dir Character vector of 2 training run directories or
#'   data frame returned from [ls_runs()] with at least 2 elements.
#'
#' @export
compare_runs <- function(runs = ls_runs(latest_n = 2),
                         viewer = getOption("tfruns.viewer")) {

  # cast to run_info
  runs <- run_info(runs)

  # verify at least 2 runs provided
  if (length(runs) < 2)
    stop("You must pass at least 2 run directories to compare_runs")

  # data for view
  data <- list(
    run_a = unclass(runs[[1]]),
    run_b = unclass(runs[[2]])
  )

  # show view
  view_page("compare_runs", data, viewer)
}


