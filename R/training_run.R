#' Training run
#'
#' Run a training script with the specified `flags` within a unique run directory.
#'
#'
#' @inheritParams  flags
#' @param file Path to training script (defaults to "train.R")
#' @param runs_dir Directory to create run directories within
#' @param type Run type (defaults to "local")
#' @param flags Named character vector with flag values (see [flags()])
#' @param echo Print expressions within training script
#' @param envir The environment in which the script should be evaluated
#' @param chdir If `TRUE` the R working directory is temporarily changed to the
#'   directory containing file for evaluating.
#' @param encoding The encoding of the training script; see [file()].
#'
#' @return The directory used for the training run.
#'
#' @export
training_run <- function(file = "train.R",
                         runs_dir = "runs",
                         type = "local",
                         config = Sys.getenv("R_CONFIG_ACTIVE", unset = "default"),
                         flags = NULL,
                         echo = FALSE,
                         envir = parent.frame(),
                         chdir = TRUE,
                         encoding = getOption("encoding")) {

  # verify that the file exists
  if (!file.exists(file))
    stop("The specified R script '", file, "' does not exist.")

  # chdir if requested
  wd <- getwd()
  if (chdir) {
    on.exit(setwd(wd), add = TRUE)
    setwd(dirname(file))
    file <- basename(file)
  }

  # setup run context
  run_dir <- initialize_run(
    runs_dir = runs_dir,
    type = type,
    config = config,
    flags = flags
  )
  on.exit(clear_run(), add = TRUE)

  # notify user of run dir (print full path if it's not relative to the invocation dir)
  absolute_run_dir <- normalizePath(run_dir)
  if (!identical(absolute_run_dir, normalizePath(file.path(wd, run_dir), mustWork = FALSE)))
    run_dir <- gsub(Sys.getenv("HOME"), "~", absolute_run_dir)
  message("Using run directory at ", run_dir)

  # perform the run
  source(file = file,
         local = envir,
         echo = echo,
         chdir = chdir,
         encoding = encoding)

  # return the run_dir
  invisible(absolute_run_dir)
}

initialize_run <- function(runs_dir = "runs",
                           type = "local",
                           config = Sys.getenv("R_CONFIG_ACTIVE", unset = "default"),
                           flags = NULL) {

  # clear any existing run
  clear_run()

  # generate the run_dir
  run_dir <- unique_dir(runs_dir, format = "%Y-%m-%dT%H-%M-%SZ")

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


