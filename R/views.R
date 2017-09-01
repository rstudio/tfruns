


#' @import whisker
render_view <- function(view, variables = list()) {

  # read a component
  read_component <- function(component) {
    lines <- readLines(system.file("views", "components", paste0(component, ".html"),
                                   package = "tfruns"),
             encoding = "UTF-8")
    paste(lines, collapse = "\n")
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
