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
  write_run_property("script", file)

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


view_run <- function(run_dir = latest_run()) {

  # verify run_dir
  if (is.null(run_dir))
    stop("No runs available in the current directory")

  # get run info
  run <- run_info(run_dir)

  # helper to extract prefaced properties
  with_preface <- function(preface) {
    prefaced <- run[grepl(paste0("^", preface, "_"), names(run))]
    if (length(prefaced) > 0)
      prefaced
    else
      NULL
  }

  data <- list(
    evaluation = NULL,
    flags = NULL
  )
  data$run_dir <- run$run_dir
  data$attributes <- list(
    type = run$type,
    script = basename(run$script),
    started = format(as.POSIXct(run$start, origin="1970-01-01")),
    time = format(as.POSIXct(as.character(Sys.Date()), tz = "GMT") +
                  run$end - run$start,
                  "%H:%M:%S")
  )
  if (!is.null(run$metrics)) {
    rows <- nrow(run$metrics)
    na_to_null <- function(x) ifelse(is.na(x), NULL, x)
    metrics <- names(run$metrics)
    names(metrics) <- metrics
    data$metrics = lapply(metrics, function(metric) {
      run$metrics[[metric]][[rows]]
    })
  } else {
    data$metrics <- NULL
  }
  evaluation <- with_preface("eval")
  if (!is.null(evaluation))
    data$evaluation <- evaluation
  flags <- with_preface("flags")
  if (!is.null(flags))
    data$flags <- flags
  data$training <- list(
    samples = run$samples,
    epochs = run$epochs,
    batch_size = run$batch_size
  )
  data$history <- run$metrics
  data$model <- sub("^Model\n", "", run$model)
  data$model <- sub("^_+\n", "", data$model)
  data$loss_function <- run$loss_function
  data$optimizer <- run$optimizer
  data$learning_rate <- run$learning_rate

  data_json <- jsonlite::toJSON(data,
                                dataframe = "columns",
                                na = "null",
                                null = "null",
                                auto_unbox = TRUE)


  viewer_dir <- tempfile("view-training-run")
  dir.create(viewer_dir)
  viewer_html <- file.path(viewer_dir, "index.html")
  render_view("training_run", viewer_html, list(data = data_json))
  #getOption("page_viewer")(viewer_html)
  browser_viewer(viewer_dir)(viewer_html)
}



