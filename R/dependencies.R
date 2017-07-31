

html_dependency_d3 <- function()  {
  htmltools::htmlDependency(
    name = "d3",
    version = "3.5.17",
    src = system.file("lib/d3", package = "tfruns"),
    script = "d3.min.js"
  )
}

html_dependency_c3 <- function()  {
  htmltools::htmlDependency(
    name = "c3",
    version = "0.4.15",
    src = system.file("lib/c3", package = "tfruns"),
    script = "c3.min.js",
    stylesheet = "c3.min.css"
  )
}

