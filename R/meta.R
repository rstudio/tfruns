

#' Write run metadata into the run directory
#'
#' Provide a function that will write data into the active run
#' directory (if any, no-op if there is none).
#'
#' @param name Name of metadata to write (subsequent writes with the same name
#' will overwrite).
#' @param write_fn Function that writes the data. The function will be
#' passed a single `data_dir` argument.
#'
#' @family run data
#'
#' @export
write_run_data <- function(name, write_fn) {
  run_dir <- run_dir()
  if (!is.null(run_dir))
    write_fn(meta_dir(run_dir))
  else
    .globals$run_dir$pending_writes[[name]] <- write_fn
}

#' Write training run metrics
#'
#' Write training metrics (e.g. accuracy, loss, etc.) into the active run
#' directory (if any, no-op if there is none).
#'
#' @param metrics Data frame containing run metrics.
#'
#' @template roxlate-metrics-format
#'
#' @family run data
#'
#' @export
write_run_metrics <- function(metrics) {
  write_run_data("metrics", function(data_dir) {
    write_metrics_json(metrics, file.path(data_dir, "metrics.json"))
  })
}




# write flags as json to run direcotory (if any)
write_run_flags <- function(FLAGS) {
  write_run_data("flags", function(data_dir) {
    jsonlite::write_json(FLAGS,
                         path = file.path(data_dir, "flags.json"),
                         auto_unbox = TRUE, # length-1 vectors as scalar
                         pretty = TRUE,     # formatted output
                         force = TRUE)      # flags as unclassed named list
  })
  FLAGS
}


# get the meta dir for a run dir
meta_dir <- function(run_dir) {
  meta_dir <- file.path(run_dir, "tfruns.d")
  if (!utils::file_test("-d", meta_dir))
    dir.create(meta_dir, recursive = TRUE)
  meta_dir
}
