
#' View metrics for a training run
#'
#' Interactive D3 visualization of metrics for a training run. Metrics will
#' be displayed in the RStudio Viewer (if available), otherwise will be
#' displayed in an external web browser.
#'
#' @inheritParams write_run_metrics
#'
#' @param metrics Data frame containing run metrics
#' @param viewer Viewer object returned from `view_run_metrics()`.
#'
#' @template roxlate-metrics-format
#'
#' @note
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
  viewer_dir <- tempfile("tfruns-metrics")
  dir.create(viewer_dir, recursive = TRUE)

  # create the metrics_viewer instance
  metrics_viewer <- structure(class = "tfruns_metrics_viewer", list(
    viewer_dir = viewer_dir
  ))

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

  # re-write index.html with embedded metrics
  metrics_json <- jsonlite::toJSON(metrics, dataframe = "columns", na = "null")
  metrics_html_path <- file.path(viewer$viewer_dir, "index.html")
  render_view("metrics",metrics_html_path, variables = list(metrics_json = metrics_json))

  # write metrics.json for polling
  metrics_json_path <- file.path(viewer$viewer_dir, "metrics.json")
  write_metrics_json(metrics, metrics_json_path)
}

# write metrics as json
write_metrics_json <- function(metrics, path) {
  jsonlite::write_json(metrics, path,
                       pretty = TRUE,
                       dataframe = "columns",
                       na = "null")
}

# non-rstudio viewer function
browser_viewer <- function(viewer_dir, browser = utils::browseURL) {

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
    path <- paste("/session", basename(viewer_dir), basename(url), sep = "/")

    # build URL and browse it
    url <- paste0("http://127.0.0.1:", port, path)
    browser(url)
  }
}


