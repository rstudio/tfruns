



view_training_run <- function() {
  view_tmp <- tempfile(fileext = ".html")
  render_view("training_run", view_tmp)
  getOption("page_viewer")(view_tmp)
}

