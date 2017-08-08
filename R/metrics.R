
#' View metrics for a training run
#'
#' Interactive D3 visualization of metrics for a training run. Metrics will
#' be displayed in the RStudio Viewer (if available), otherwise will be
#' displayed in an external web browser.
#'
#' @param metrics Data frame containing run metrics.
#' @param viewer Viewer object returned from `view_run_metrics()`.
#'
#' @section Data Format:
#'
#'   Metrics should be passed as a data frame with one column for each metric to
#'   be plotted. If the metrics are not yet complete (e.g. only metrics for the
#'   first several epochs are provided) then metrics in yet to be completed
#'   epochs should use `NA` as their values. For example:
#'
#'   ```
#'   data.frame':	30 obs. of  4 variables:
#'   $ loss    : num  0.423 0.201 NA NA NA ...
#'   $ acc     : num  0.873 0.942 NA NA NA ...
#'   $ val_loss: num  0.174 0.121 NA NA NA ...
#'   $ val_acc : num  0.949 0.964 NA NA NA ...
#'   ```
#'
#'   Metrics and their corresponding validation metric will be plotted together
#'   if you preface the name of the validation metric with `"val_"` (e.g. for a
#'   metric named `"loss"` provide validation metrics in `"val_loss"`).
#'
#'   Metrics named `"acc"` or `"accuracy"` will automatically use `1.0` as the
#'   maximum value on their y-axis scale.
#'
#' @section Realtime Updates:
#'
#'   Metrics can be updated in real-time by calling the `update_run_metrics()`
#'   with the run viewer instance returned from `view_run_metrics()`. For example:
#'
#'   ```
#'   # view metrics
#'   viewer <- view_run_metrics(metrics)
#'
#'   # update with new metrics
#'   update_run_metrics(viewer, updated_metrics)
#'   ```
#'
#' @export
view_run_metrics <- function(metrics) {

  # create a new temp directory for the viewer's UI/data
  viewer_dir <- tempfile("keras-metrics")
  dir.create(viewer_dir, recursive = TRUE)

  # create the metrics_viewer instance
  metrics_viewer <- structure(class = "tfruns_metrics_viewer", list(
    viewer_dir = viewer_dir
  ))

  # copy dependencies to the viewer dir
  lib_dir <- system.file("lib", package = "tfruns")
  file.copy(from = file.path(lib_dir, c("d3.min.js","c3.min.js", "c3.min.css")),
            to = viewer_dir)
  metrics_viewer_html <- system.file("views", "metrics", package = "tfruns")
  file.copy(from = file.path(metrics_viewer_html, c("metrics.js", "metrics.css")),
            to = viewer_dir)

  # write the history
  update_run_metrics(metrics_viewer, metrics)

  # view it
  viewer <- getOption("viewer", default = browser_viewer(viewer_dir))
  viewer(file.path(viewer_dir, "index.html"))

  # return metrics_viewer instance (invisibly) for subsequent
  # calls to update_run_history
  invisible(metrics_viewer)
}


#' @rdname view_run_metrics
#' @export
update_run_metrics <- function(viewer, metrics) {

  # re-write index.html with embedded history
  history_json <- jsonlite::toJSON(metrics, dataframe = "columns", na = "null")
  history_html <- system.file("views", "metrics", "index.html", package = "tfruns")
  history_html_lines <- readLines(history_html, encoding = "UTF-8")
  history_html_lines <- sprintf(history_html_lines, history_json)
  writeLines(history_html_lines, file.path(viewer$viewer_dir, "index.html"))

  # write metrics.json for polling
  history_json <- file.path(viewer$viewer_dir, "metrics.json")
  jsonlite::write_json(metrics, history_json, dataframe = "columns", na = "null")
}

# non-rstudio viewer function
browser_viewer <- function(viewer_dir) {

  function(url) {
    # determine help server port
    port <- tools::startDynamicHelp(NA)
    if (port <= 0)
      port <- tools::startDynamicHelp(TRUE)
    if (port <= 0) {
      warning("Unable to view run metrics (couldn't access help server port)",
              call. = FALSE)
      return(invisible(NULL))
    }

    # determine path to history html
    path <- paste("/session", basename(viewer_dir), "index.html", sep = "/")

    # build URL and browse it
    url <- paste0("http://127.0.0.1:", port, path)
    utils::browseURL(url)
  }
}


