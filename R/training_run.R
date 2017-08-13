#' Training run
#'
#' Run a training script with the specified `flags` within a unique run directory.
#'
#'
#' @inheritParams  flags
#' @param file Path to training script (defaults to "train.R")
#' @param type Run type (defaults to "local")
#' @param flags Named character vector with flag values (see [flags()])
#' @param run_dir Directory to store run data within
#' @param echo Print expressions within training script
#' @param envir The environment in which the script should be evaluated
#' @param encoding The encoding of the training script; see [file()].
#'
#' @return The directory used for the training run.
#'
#' @export
training_run <- function(file = "train.R",
                         type = "local",
                         config = Sys.getenv("R_CONFIG_ACTIVE", unset = "default"),
                         flags = NULL,
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
    run_dir = run_dir
  )
  on.exit(clear_run(), add = TRUE)

  # notify user of run dir
  message("Using run directory ", run_dir)

  # write begin and end times
  write_run_property("start", as.double(Sys.time()))
  on.exit(write_run_property("end", as.double(Sys.time())), add = TRUE)

  # perform the run
  source(file = file, local = envir, echo = echo, encoding = encoding)

  # return the run_dir
  invisible(run_dir)
}

initialize_run <- function(type = "local",
                           config = Sys.getenv("R_CONFIG_ACTIVE", unset = "default"),
                           flags = NULL,
                           run_dir = NULL) {

  # clear any existing run
  clear_run()

  # generate the run_dir
  if (is.null(run_dir))
    run_dir <- unique_dir("runs")

  # create the directory if necessary
  if (!utils::file_test("-d", run_dir))
    if (!dir.create(run_dir, recursive = TRUE))
      stop("Unable to create run directory at ", run_dir)

  # this is new definition for the run_dir, save it
  .globals$run_dir$path <- run_dir

  # save config and flags (they'll get processed later in flags())
  .globals$run_dir$config <- config
  .globals$run_dir$flags <- flags

  # write type
  write_run_metadata("properties", list(type = type))

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
  .globals$run_dir$pending_writes <- new.env(parent = emptyenv())
}

unique_dir <- function(parent_dir, prefix = NULL, format = "%Y-%m-%dT%H-%M-%SZ") {
  while(TRUE) {
    dir <- file.path(parent_dir,
                     paste0(prefix, strftime(Sys.time(), format = format, tz = "GMT")))
    if (!file.exists(dir))
      return(dir)
    else
      Sys.sleep(0.1)
  }
}


