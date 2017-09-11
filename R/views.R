


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
