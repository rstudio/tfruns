#' Training run
#'
#' Run a training script with the specified `flags` within a unique run
#' directory.
#'
#'
#' @inheritParams  flags
#' @param path Path to training script. Defaults to "train.R" within the current
#'   working directory but can be any R script or directory that contains a
#'   training script. If a directory is specified then it must either have a
#'   file named "train.R" within it or otherwise have only a single R script
#'   (failing that you should specify the full path to the R script).
#' @param type Run type (defaults to "local")
#' @param flags Named character vector with flag values (see [flags()]) or path
#'   to YAML file containing flag values.
#' @param run_dir Directory to store run data within
#' @param echo Print expressions within training script
#' @param envir The environment in which the script should be evaluated
#' @param encoding The encoding of the training script; see [file()].
#'
#' @return The directory used for the training run.
#'
#' @export
training_run <- function(path = ".",
                         type = "local",
                         config = Sys.getenv("R_CONFIG_ACTIVE", unset = "default"),
                         flags = NULL,
                         run_dir = NULL,
                         echo = FALSE,
                         envir = parent.frame(),
                         encoding = getOption("encoding")) {

  # resolve training script
  script <- resolve_training_script(path)

  # if no run_dir is specified then use "runs" within the directory
  # of the training script
  if (is.null(run_dir))
    run_dir <- unique_dir(file.path(dirname(script), "runs"))

  # normalize run_dir (remove leading './', use '~' for home)
  run_dir <- normalize_run_dir(run_dir)

  # setup run context
  initialize_run(
    type = type,
    config = config,
    flags = flags,
    run_dir = run_dir
  )

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
      source(file = script, local = envir, echo = echo, encoding = encoding)
      write_run_property("completed", TRUE)
    },
    error = function(e) {
      write_run_property("error", e$message)
      stop(e)
    }
  )

  # return the run_dir
  invisible(run_dir)
}


resolve_training_script <- function(path) {
  if (utils::file_test("-d", path)) {
    train_r <- file.path(path, "train.R")
    if (file.exists(train_r)) {
      train_r
    } else {
      r_scripts <- list.files(path = path,
                              pattern = glob2rx("*.r"),
                              ignore.case = TRUE)
      if (length(r_scripts) == 1)
        file.path(path, r_scripts)
      else if (length(r_scripts) == 0)
        stop("There are no R scripts located within '", path, "'", call. = FALSE)
      else
        stop("There is more than one R script in '", path, "'. Please either ",
             "specify an R script or name one of the scripts 'train.R' to ",
             "indicate it is the main training script.", call. = FALSE)

    }
  } else if (file.exists(path)) {
    path
  } else {
    stop("The specified path '", path, "' does not exist.", call. = FALSE)
  }
}

normalize_run_dir <- function(run_dir) {
  sub(Sys.getenv("HOME"), "~", run_dir)
  sub("^\\.[/\\]", "", run_dir)
}

initialize_run <- function(type = "local",
                           config = Sys.getenv("R_CONFIG_ACTIVE", unset = "default"),
                           flags = NULL,
                           run_dir = NULL) {

  # clear any existing run
  clear_run()

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

  # write source files
  write_run_metadata("source", getwd())

  # execute any pending writes
  for (name in ls(.globals$run_dir$pending_writes))
    .globals$run_dir$pending_writes[[name]](meta_dir(run_dir))

  NULL
}

clear_run <- function() {
  .globals$run_dir$path <- NULL
  .globals$run_dir$config <- NULL
  .globals$run_dir$flags <- NULL
  .globals$run_dir$flags_file <- NULL
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


