
add_in_training_run <- function() {
  editor_context <- rstudioapi::getSourceEditorContext()
  editor_path <- editor_context$path
  if (isTRUE(nzchar(editor_path))) {
    rstudioapi::sendToConsole(sprintf('tfruns::training_run("%s")', editor_path))
  } else {
    message("Unable to perform training run (active source file is not an R script)")
  }
}

add_in_view_all_runs <- function() {
  rstudioapi::sendToConsole("View(tfruns::ls_runs())")
}
