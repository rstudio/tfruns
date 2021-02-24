

#' Write run metadata
#'
#' Record various types of training run metadata This function can be called
#' even when a run directory isn't active (metadata will only be written if
#' and when a run directory is initialized).
#'
#' @param type Type of metadata to write. Standard types include "flags",
#'   "sources", "properties", "metrics", and "evaluation". You can
#'   also specify a custom type (see *Custom Types* section below).
#'
#' @param data Metadata to write:
#'
#'   - "flags" --- Named list of training flags
#'   - "source" --- Directory to copy source files from
#'   - "properties" --- Named list of arbitrary properties. Note
#'     that properties will be stored as strings.
#'   - "metrics" --- Data frame with training run metrics
#'     (see *Metrics Data Frame* below).
#'   - "evaluation" --- Named list of evaluation metrics.
#'   - "error" --- Named list with 'message' and 'traceback'
#'   - "<custom>" -- Function used to write the data
#'     (see *Custom Types* section below).
#'
#' @param run_dir Run directory to write metadata into (defaults
#'   to currently active run)
#'
#' @template roxlate-metrics-format
#'
#' @section Custom Types:
#'
#' You can pass a type with an arbitary name along with a function that
#' should be used to writes the data. The function will be passed a
#' single `data_dir` argument. For example:
#'
#' ```r
#' write_run_metadata("images", function(data_dir) {
#'   # write into data_dir here
#' })
#' ````
#'
#' @note
#' `write_run_data()` is deprecated and is provided as an alias
#' for backward compatibility.
#'
#' @keywords internal
#'
#' @export
write_run_metadata <- function(type, data, run_dir = NULL) {

  # we need to create a write_fn so that the write can be deferred
  # until after a run_dir is actually established. Create the function
  # automatically for known types, for unknown types the `data`
  # argument is the write_fn

  # helper function to write simple named lists of values
  named_list_write_fn <- function(type) {
    function(data_dir) {
      jsonlite::write_json(
        data,
        path = file.path(data_dir, paste0(type, ".json")),
        auto_unbox = TRUE, # length-1 vectors as scalar
        pretty = TRUE,     # formatted output
        force = TRUE, # flags as unclassed named list,
        digits = NA # save data with full precision
      )
    }
  }

  # simple named list
  if (type %in% c("flags", "evaluation", "error")) {

    write_fn <- named_list_write_fn(type)

  # properties (written individually to support multiple writes)
  } else if (identical(type, "properties")) {

    write_fn <- function(data_dir) {
      properties_dir <- file.path(data_dir, "properties")
      if (!utils::file_test("-d", properties_dir))
        dir.create(properties_dir, recursive = TRUE)
      for (name in names(data)) {
        property_file <- file.path(properties_dir, name)
        writeLines(as.character(data[[name]]), property_file)
      }
    }

  # metrics data frame
  } else if (identical(type, "metrics")) {

    write_fn <- function(data_dir) {
      write_metrics_json(data, file.path(data_dir, "metrics.json"))
    }

  # source code
  } else if (identical(type, "source")) {

    write_fn <- function(data_dir) {
      write_source_archive(data, data_dir, "source.tar.gz")
    }

  # custom type
  } else {
    if (!is.function(data))
      stop("The data parameter must be a function for custom data types")
    write_fn <- data
  }

  # check for a run_dir. if we have one write the run data, otherwise
  # defer the write until we (maybe) acquire a run_dir later
  if (!is.null(run_dir))
    write_fn(meta_dir(run_dir))
  else if (is_run_active())
    write_fn(meta_dir(run_dir()))
  else
    .globals$run_dir$pending_writes[[type]] <- write_fn

  # return nothing
  invisible(NULL)
}

write_run_property <- function(name, value) {
  properties <- list()
  properties[[name]] <- value
  write_run_metadata("properties", properties)
}


#' Write run data (deprecated)
#'
#' Deprecated alias for [write_run_metadata()].
#'
#' @inheritParams write_run_metadata
#'
#' @keywords internal
#' @export
write_run_data <- function(type, data) {
  write_run_metadata(paste0("custom", type), data)
}


write_source_archive <- function(sources_dir, data_dir, archive) {

  # normalize paths since we'll be changing the working dir
  sources_dir <- normalizePath(sources_dir)
  data_dir <- normalizePath(data_dir)

  # change to sources_dir
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(sources_dir)

  # enumerate source files. note that we used to do this recursively but ran
  # into performance issues when script files were in a directory with large
  # subdirectories. Here's the commit where we backed this out:
  #  https://github.com/rstudio/tfruns/commit/2dbcac627c82a2ecccecc2ba5ecada61d91255c7
  # (note that if we bring this back we need to continue ignoring the packrat dir)
  # the right solution might be to override `source` for the duration of the
  # run and just track which R scripts are sourced.
  files <- list.files(path = ".",
                      pattern = utils::glob2rx("*.r"),
                      ignore.case = TRUE)

  # create temp dir for sources
  sources_tmp_dir <- file.path(tempfile("tfruns-sources"), "source")
  on.exit(unlink(sources_tmp_dir), add = TRUE)
  dir.create(sources_tmp_dir, recursive = TRUE)

  # copy the sources to the temp dir
  for (file in files) {
    dir <- dirname(file)
    target_dir <- file.path(sources_tmp_dir, dir)
    if (!utils::file_test("-d", target_dir))
      dir.create(target_dir, recursive = TRUE)
    file.copy(from = file, to = target_dir)
  }

  # create the tarball
  # tar and prevent "storing paths of more than 100 bytes is not
  # portable" warning issued by R
  setwd(file.path(sources_tmp_dir, ".."))
  suppressWarnings(
    utils::tar(
      file.path(data_dir, archive),
      files = "source",
      compression = "gzip",
      tar = "internal"
    )
  )
}


# get the meta dir for a run dir
meta_dir <- function(run_dir, create = TRUE) {
  meta_dir <- file.path(run_dir, "tfruns.d")
  if (create && !utils::file_test("-d", meta_dir))
    dir.create(meta_dir, recursive = TRUE)
  meta_dir
}
