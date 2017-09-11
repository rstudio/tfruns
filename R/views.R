


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

view_page <- function(page, data, viewer) {

  # convert data to json
  data_json <- jsonlite::toJSON(data,
                                dataframe = "columns",
                                na = "null",
                                null = "null",
                                auto_unbox = TRUE)

  # render html
  viewer_dir <- tempfile(page)
  dir.create(viewer_dir)
  viewer_html <- file.path(viewer_dir, "index.html")
  render_view(page, viewer_html, list(data = data_json))

  # display html
  if (is.null(viewer))
    viewer <- getOption("page_viewer", default = utils::browseURL)
  if (identical(viewer, getOption("page_viewer")))
    viewer(viewer_html)
  else
    browser_viewer(viewer_dir, viewer)(viewer_html)
}
