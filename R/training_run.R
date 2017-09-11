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

  if (is.null(run_dir))
    stop("No runs available in the current directory")

  run <- run_info(run_dir)
  data <- list()
  data$run_dir <- run$run_dir
  data$attributes <- list(
    type = run$type,
    script = run$script,
    time = format(as.POSIXct(as.character(Sys.Date()), tz = "GMT") +
                  run$end - run$start,
                  "%H:%M:%S")
  )
  data$metrics = list(
    acc = 0.9869,
    val_acc = 0.9814,
    loss = 0.0497,
    val_loss = 0.1085
  )
  data$evaluation = list(
    eval_acc = 0.9835,
    eval_loss = 0.0951
  )
  data$flags = list(
    learning_rate = 0.01,
    batch_size = 128,
    hidden1 = 64,
    hidden2 = 32
  )
  data$training = list(
    samples = "48,000",
    val_samples = "12,000",
    epochs = 30,
    batch_size = 128
  )
  data$history = I(list(
    loss = I(c(0.4232,0.2001,0.1523,0.1313,0.1146,0.1041,0.0962,0.0874,0.0862,0.0806,0.0764,0.0774,0.0717,0.0675,0.0633,0.062,0.0632,0.0613,0.0625,0.0614,0.0557,0.0601,0.059,0.0553,0.0557,0.0551,0.0526,0.0543,0.0525,0.0524)),
    acc = I(c(0.8709,0.9408,0.9539,0.9621,0.9658,0.9694,0.9726,0.9743,0.9749,0.9767,0.9778,0.9774,0.9801,0.9799,0.9815,0.982,0.983,0.9827,0.9825,0.9834,0.9846,0.9836,0.9838,0.9847,0.9848,0.9853,0.9861,0.986,0.9858,0.9855)),
    val_loss = I(c(0.1606,0.1228,0.1069,0.0979,0.0967,0.0927,0.0915,0.0882,0.0885,0.0888,0.0928,0.094,0.0928,0.0907,0.0955,0.0953,0.0941,0.101,0.0957,0.097,0.1006,0.0981,0.0992,0.1046,0.1064,0.1051,0.1093,0.1104,0.1137,0.1108)),
    val_acc = I(c(0.9508,0.9653,0.9705,0.9717,0.9743,0.9747,0.9757,0.9775,0.9772,0.979,0.9786,0.9778,0.9793,0.9801,0.979,0.9785,0.9806,0.9796,0.9799,0.9802,0.9796,0.9813,0.9813,0.9798,0.9814,0.9805,0.9808,0.9802,0.9793,0.9813))
  ))
  data$model = "Layer (type)                        Output Shape                    Param #\n================================================================================\ndense_1 (Dense)                     (None, 256)                     200960\n________________________________________________________________________________\ndropout_1 (Dropout)                 (None, 256)                     0\n________________________________________________________________________________\ndense_2 (Dense)                     (None, 128)                     32896\n________________________________________________________________________________\ndropout_2 (Dropout)                 (None, 128)                     0\n________________________________________________________________________________\ndense_3 (Dense)                     (None, 10)                      1290\n================================================================================\n\nTotal params: 235,146\nTrainable params: 235,146\nNon-trainable params: 0"
  data$loss = "categorical_crossentropy"
  data$optimizer = "RMSProp"
  data$learning_rate = "0.01"

  data_json <- jsonlite::toJSON(data,
                                dataframe = "columns",
                                na = "null",
                                auto_unbox = TRUE)


  viewer_dir <- tempfile("view-training-run")
  dir.create(viewer_dir)
  viewer_html <- file.path(viewer_dir, "index.html")
  render_view("training_run", viewer_html, list(data = data_json))
  #getOption("page_viewer")(viewer_html)
  browser_viewer(viewer_dir)(viewer_html)
}



