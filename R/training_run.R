#' Training run
#'
#' Run a training script with the specified `flags` within a unique run directory.
#'
#' @param file Path to training script (defaults to "train.R")
#' @param runs_dir Directory to create run directories within
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
  run_dir <- initialize_run(runs_dir, flags)
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
