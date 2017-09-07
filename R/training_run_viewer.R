



view_training_run <- function() {
  viewer_dir <- tempfile("view-training-run")
  dir.create(viewer_dir)
  viewer_html <- file.path(viewer_dir, "index.html")
  render_view("training_run", viewer_html)
  #getOption("page_viewer")(viewer_html)
  browser_viewer(viewer_dir)(viewer_html)
}

