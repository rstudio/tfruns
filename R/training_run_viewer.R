



view_training_run <- function() {

  cat("Time: ", system.time({
    training_run_html <- render_view("training_run")
    view_tmp <- tempfile(fileext = ".html")
    writeLines(training_run_html, view_tmp, useBytes = TRUE)
  }), "\n")




  getOption("page_viewer")(view_tmp)

}


