

#' Write data into the current run directory
#'
#' Record various types of training run data. This function can be called
#' even when a run directory isn't active (data will only be written if
#' and when a run directory is initialized).
#'
#' @param type Type of data to write. Standard types include "flags",
#'   "sources", "properties", "metrics", and "evaluation". You can
#'   also specify a custom type (see *Custom Types* section below).
#'
#' @param data Data to write:
#'
#'   - "flags" --- Named list of training flags
#'   - "source" --- Character vector of source file paths
#'   - "properties" --- Named list of arbitrary properties.
#'   - "metrics" --- Data frame with training run metrics
#'     (see *Metrics Data Frame* below).
#'   - "evaluation" --- Named list of evaluation metrics.
#'   - "<custom>" -- Function used to write the data
#'     (see *Custom Types* section below).
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
#' write_run_data("images", function(data_dir) {
#'   # write into data_dir here
#' })
#' ````
#'
#' @export
write_run_data <- function(type, data) {

  # we need to create a write_fn so that the write can be deferred
  # until after a run_dir is actually established. Create the function
  # automatically for known types, for unknown types the `data`
  # argument is the write_fn

  # helper function to write simple property pags
  properties_write_fn <- function(type) {
    function(data_dir) {
      jsonlite::write_json(
        data,
        path = file.path(data_dir, paste0(type, ".json")),
        auto_unbox = TRUE, # length-1 vectors as scalar
        pretty = TRUE,     # formatted output
        force = TRUE)      # flags as unclassed named list
    }
  }

  # simple property-bag
  if (type %in% c("flags", "properties", "evaluation")) {

    write_fn <- properties_write_fn(type)

  # metrics data frame
  } else if (identical(type, "metrics")) {

    write_fn <- function(data_dir) {
      write_metrics_json(data, file.path(data_dir, "metrics.json"))
    }

  # source code
  } else if (identical(type, "source")) {

    write_fn <- function(data_dir) {
      sources_dir <- file.path(data_dir, "source")
      unlink(sources_dir, recursive = TRUE)
      dir.create(sources_dir)
      file.copy(data, sources_dir)
    }

  # custom type
  } else {
    if (!is.function(data))
      stop("The data parameter must be a function for custom data types")
    write_fn <- data
  }

  # check for a run_dir. if we have one write the run data, otherwise
  # defer the write until we (maybe) acquire a run_dir later
  run_dir <- run_dir()
  if (!is.null(run_dir))
    write_fn(meta_dir(run_dir))
  else
    .globals$run_dir$pending_writes[[type]] <- write_fn
}


# get the meta dir for a run dir
meta_dir <- function(run_dir) {
  meta_dir <- file.path(run_dir, "tfruns.d")
  if (!utils::file_test("-d", meta_dir))
    dir.create(meta_dir, recursive = TRUE)
  meta_dir
}
