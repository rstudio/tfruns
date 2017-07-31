

#' Training Run Viewer
#'
#'
#'
#'
#' @export
view_run <- function(run_dir, run_viewer = NULL) {

  # get the viewer (error if don't have one)
  viewer <- getOption("viewer")
  if (is.null(viewer))
    stop("The run_viewer function must be run within RStudio")

  # function to update the data in a run_viewer



  # create a temp new directory for the viewer's UI/data
  viewer_dir <- tempfile("viewhtml")
  dir.create(viewer_dir)

  # create the run_viewer instance
  run_viewer <- structure(class = "tfruns_run_viewer",
    run_dir = run_dir,
    viewer_dir = viewer_dir
  )

  # write the metrics into the directory
  run_viewer_update(run_viewer)

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

  # return run_viewer instance (invisibly)
  invisible(run_viewer)
}

view_run_update <- function(run_viewer) {

  # alias directories
  run_dir <- viewer$run_dir
  viewer_dir <- viewer$viewer_dir

  history_json <- file.path(viewer_dir, "history.json")
  jsonlite::write_json(list(foo = runif(1, 5.0, 7.5)), history_json)


}










