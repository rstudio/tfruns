


#' @import whisker
render_view <- function(view, variables = list()) {

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
    c3 = read_component("c3"),
    metrics_charts = read_component("metrics_charts"),
    jquery = read_component("jquery"),
    material_bootstrap_js = read_component("material_bootstrap_js"),
    material_bootstrap_css = read_component("material_bootstrap_css")
  ))

  # read the template
  template <- readLines(system.file("views", paste0(view, ".html"), package = "tfruns"),
                        encoding = "UTF-8")

  # render the template
  whisker.render(template = template, data = variables)
}
