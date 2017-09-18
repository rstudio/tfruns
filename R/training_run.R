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
#' @param view View the results of the run after training. The default "auto"
#'   will view the run when in an interactive session. Pass `TRUE` or `FALSE`
#'   to control whether the view is shown explictly. Pass an R function that
#'   accepts a single "url" argument to provide a custom viewer. The default
#'   viewer function can be specified using the `tfruns.viewer` R global option.
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
                         echo = TRUE,
                         view = "auto",
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

  # run viewing helper
  view_this_run <- function(...) {
    message('\nview_run("', run_dir, '")\n')
    view_run(run_dir, ...)
  }

  # resolve 'auto' view
  if (identical(view, "auto"))
    view <- interactive()
  # view if requested
  if (is.function(view))
    view_this_run(viewer = view)
  else if (isTRUE(view))
    view_this_run()

  # return the run invisibly
  invisible(return_runs(run_record(run_dir)))
}


do_training_run <- function(file, run_dir, echo, envir, encoding) {

  with_changed_file_copy(dirname(file), run_dir, {

    # write script
    write_run_property("script", basename(file))

    # write begin and end times
    write_run_property("start", as.double(Sys.time()))
    on.exit(write_run_property("end", as.double(Sys.time())), add = TRUE)

    # clear run on exit
    on.exit(clear_run(), add = TRUE)

    # set width for run
    old_width <- getOption("width")
    options(width = min(100, old_width))
    on.exit(options(width = old_width), add = TRUE)

    # sink output
    properties_dir <- file.path(meta_dir(run_dir), "properties")
    output_file <- file(file.path(properties_dir, "output"), open = "wt", encoding = "UTF-8")
    sink(file = output_file, type = "output", split = TRUE)
    on.exit({ sink(type = "output"); close(output_file); }, add = TRUE)

    # sink plots
    grDevices::png(file.path(run_dir, "Rplot%03d.png"))
    dev_number <- dev.cur()
    on.exit(dev.off(dev_number), add = TRUE)

    # notify user of run dir
    message("Using run directory ", run_dir)

    # perform the run
    write_run_property("completed", FALSE)
    withCallingHandlers({
      source(file = file, local = envir, echo = echo, encoding = encoding)
      write_run_property("completed", TRUE)
    },
    error = function(e) {

      # write error
      write_run_metadata("error", list(
        message = e$message,
        traceback = capture_stacktrace(sys.calls())
      ))

      # forward error
      stop(e)
    })
  })
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

capture_stacktrace <- function(stack) {
  stack <- stack[-(2:7)]
  stack <- utils::head(stack, -2)
  stack <- vapply(stack, function(frame) {
    frame <- deparse(frame)
    frame <- paste(frame, collapse = "\n")
    frame
  }, FUN.VALUE = "frame")
  rev(stack)
}

with_changed_file_copy <- function(training_dir, run_dir, expr) {

  # snapshot the files in the training script directory before training
  files_before <- utils::fileSnapshot(path = training_dir, recursive = TRUE)

  # copy changed files on exit
  on.exit({
    # snapshot the files in the run_dir after training then compute changed files
    files_after <- utils::fileSnapshot(path = training_dir, recursive = TRUE)
    changed_files <- utils::changedFiles(files_before, files_after)

    # filter out files within the run_dir
    changed_files <- c(changed_files$changed, changed_files$added)
    changed_files <- changed_files[!grepl(basename(run_dir), changed_files)]

    # copy the changed files to the run_dir
    for (file in changed_files) {
      dir <- dirname(file)
      target_dir <- file.path(run_dir, dir)
      if (!utils::file_test("-d", target_dir))
        dir.create(target_dir, recursive = TRUE)
      file.copy(from = file.path(training_dir, file), to = target_dir)
    }
  }, add = TRUE)

  # execute the expression
  force(expr)
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
    if (!is.null(x))
      prettyNum(x, mode = "integer", big.mark = ",")
    else
      NULL
  }
  format_numeric <- function(x, digits = 4) {
    if (!is.null(x))
      formatC(x, format='f', digits= digits)
    else
      NULL
  }

  # default some potentially empty sections to null
  data <- list(
    history = NULL,
    model = NULL,
    metrics = NULL,
    evaluation = NULL,
    optimization = NULL,
    training = NULL,
    flags = NULL,
    output = NULL,
    error = NULL
  )

  # tabs
  data$tabs <- list(
    list(href = "#summary", title = "Summary"),
    list(href = "#output", title = "Output"),
    list(href = "#code", title = "Code")
  )

  # run_dir
  data$run_dir <- run$run_dir

  # attributes
  script <- basename(run$script)
  data$attributes <- list(
    type = run$type,
    script = script,
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
  optimization <- list()
  optimization$loss <- run$loss_function
  optimization$optimizer <- run$optimizer
  optimization$lr <- run$learning_rate
  if (length(optimization) > 0)
    data$optimization <- optimization

  # training
  if (!is.null(run[["epochs"]]) && !is.null(run$epochs_completed)) {
    if (run$epochs > run$epochs_completed)
      epochs <- paste(format_integer(run$epochs_completed),
                      format_integer(run$epochs),
                      sep = "/")
    else
      epochs <- format_integer(run$epochs)
  } else {
    epochs <- NULL
  }
  training <- list()
  training$samples <- format_integer(run$samples)
  training$validation_samples <- format_integer(run$validation_samples)
  training$epochs <- epochs
  training$batch_size <- format_integer(run$batch_size)
  if (length(training) > 0)
    data$training <- training

  # metrics history
  if (!is.null(run$metrics))
    data$history <- run$metrics

  # model summary
  if (!is.null(run$model)) {
    data$model <- sub("^Model\n", "", run$model)
    data$model <- sub("^_+\n", "", data$model)
  }

  # source code
  data$source_code <- run_source_code(script, run$run_dir)

  # output
  if (!is.null(run$output))
    data$output <- run$output

  # error
  if (!is.null(run$error_message)) {
    data$error <- list(
      message = run$error_message,
      traceback = run$error_traceback
    )
  }

  # view the page
  view_page("view_run",
            stem = paste0("view-run-", basename(run$run_dir)),
            data = data,
            viewer = viewer)
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
  view_page("compare_runs",
            data = data,
            viewer = viewer)
}


