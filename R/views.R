


#' @import whisker
render_view <- function(view, output_file, variables = list()) {

  # read a component
  read_component <- function(name) {

    if (!exists(name, envir = .globals$view_components)) {
      lines <- readLines(system.file("views", "components", paste0(name, ".html"),
                                     package = "tfruns"),
                         encoding = "UTF-8")
      component <- paste(lines, collapse = "\n")
      assign(name, component, envir = .globals$view_components)
    }

    get(name, envir = .globals$view_components)
  }

  # add components to variables
  variables <- append(variables, list(
    jquery = read_component("jquery"),
    roboto = read_component("roboto"),
    material_icons = read_component("material_icons"),
    materialize = read_component("materialize"),
    vue_js = read_component("vue_min_js"),
    c3 = read_component("c3"),
    metrics_charts = read_component("metrics_charts"),
    highlight_js = read_component("highlight_js"),
    diff2html = read_component("diff2html"),
    dashboard = read_component("dashboard")
  ))

  # read the template
  template <- readLines(system.file("views", paste0(view, ".html"), package = "tfruns"),
                        encoding = "UTF-8")

  # render the template
  html <- whisker.render(template = template, data = variables)

  # write it
  writeLines(html, output_file, useBytes = TRUE)
}


save_page <- function(page, data, target_file) {

  # convert data to json
  data_json <- jsonlite::toJSON(data,
                                dataframe = "columns",
                                na = "null",
                                null = "null",
                                auto_unbox = TRUE)

  # render it
  render_view(page, target_file, list(data = data_json))

}

viewer_temp_file <- function(stem) {

  viewer_dir <- tempfile("tfruns-")
  dir.create(viewer_dir)
  file.path(viewer_dir, paste0(stem, ".html"))

}

view_page <- function(viewer_html, viewer) {

  if (is.null(viewer))
    viewer <- getOption("page_viewer", default = utils::browseURL)
  if (identical(viewer, getOption("page_viewer"))) {
    args <- list(url = viewer_html)
    if (!is.null(formals(viewer)$self_contained))
      args$self_contained <- TRUE
    do.call(viewer, args)
  } else {
    browser_viewer(dirname(viewer_html), viewer)(viewer_html)
  }

  invisible(viewer_html)

}

