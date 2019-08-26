is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

# we cant use tensorflow::tf_version() to not create a
# dependency
tf_version <- function(tf) {

  if (reticulate::py_has_attr(tf, "version"))
    version_raw <- tf$version$VERSION
  else version_raw <- tf$VERSION

  tfv <- strsplit(version_raw, ".", fixed = TRUE)[[1]]
  version <- package_version(paste(tfv[[1]], tfv[[2]],
                                   sep = "."))

  version
}
