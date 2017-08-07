

#' View
#'
#'
#'
#'
#' @export
view_run_history <- function(history) {

  # create a new temp directory for the viewer's UI/data
  viewer_dir <- tempfile("viewhtml")
  dir.create(viewer_dir)

  # create the history_viewer instance
  history_viewer <- structure(class = "tfruns_history_viewer",
    viewer_dir = viewer_dir
  )

  # write the history
  update_run_history(history_viewer, history)

  # render run viewer html
  run_viewer_template <- system.file("views/run.html", package = "tfruns")
  run_viewer_html <- htmltools::htmlTemplate(run_template)
  run_viewer_file <- file.path(viewer_dir, "index.html")
  save_html(run_viewer_html,
            file = run_viewer_file,
            background = "white",
            libdir = "lib")

  # view it
  getOption("viewer")(run_viewer_file)

  # return history_viewer instance (invisibly) for subsequent
  # calls to update_run_history
  invisible(run_viewer)
}


#' @rdname view_run_history
#' @export
update_run_history <- function(history_viewer, history) {
  history_json <- file.path(history_viewer$viewer_dir, "history.json")
  jsonlite::write_json(history, history_json)
}










