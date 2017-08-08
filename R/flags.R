
#' Flags for a training run
#'
#' Define the flags (name, type, default value, description) which paramaterize
#' a training run. Optionally read overrides of the default values from a
#' "flags.yml" config file and/or command line arguments.
#'
#' @param FLAGS Named list of flags
#' @param name Flag name
#' @param default Flag default value
#' @param description Flag description
#' @param config The configuration to use. Defaults to the active configuration
#'   for the current environment (as specified by the `R_CONFIG_ACTIVE`
#'   environment variable), or `default` when unset.
#' @param file The configuration file to read
#' @param arguments The command line arguments (as a character vector) to be
#'   parsed.
#'
#' @return Named list of training flags
#'
#' @section Config File Flags:
#'
#' Config file flags are defined a YAML configuration file (by default
#' named "flags.yml"). Flags can either appear at the top-level of
#' the YAML or can be inclued in named configuration sections
#' (see the \href{config package}{https://github.com/rstudio/config} for
#' details).
#'
#' @section Command Line Flags:
#'
#' Command line flags should be of the form `--key=value` or
#' `--key value`. The values are assumed to be valid `yaml` and
#' will be converted using [yaml.load()].
#'
#' @examples
#' \dontrun{
#' library(tfruns)
#'
#' # define flags and parse flag values from flags.yml and the command line
#' FLAGS <- flags() %>%
#'   flag_numeric('learning_rate', 0.01, 'Initial learning rate.') %>%
#'   flag_integer('max_steps', 5000, 'Number of steps to run trainer.') %>%
#'   flag_string('data_dir', 'MNIST-data', 'Directory for training data') %>%
#'   flag_boolean('fake_data', FALSE, 'If true, use fake data for testing') %>%
#'   flags_parse()
#' }
#'
#' @export
flags <- function() {
  structure(class = "tfruns_flags",
    types = character(),
    defaults = list(),
    docstrings = character(),
    list()
  )
}

#' @rdname flags
#' @export
flag_numeric <- function(FLAGS, name, default, description = NULL) {
  add_flag(FLAGS, name, "numeric", as.numeric(default), description)
}

#' @rdname flags
#' @export
flag_integer <- function(FLAGS, name, default, description = NULL) {
  add_flag(FLAGS, name, "integer", as.integer(default), description)
}

#' @rdname flags
#' @export
flag_boolean <- function(FLAGS, name, default, description = NULL) {
  add_flag(FLAGS, name, "boolean", as.logical(default), description)
}

#' @rdname flags
#' @export
flag_string <- function(FLAGS, name, default, description = NULL) {
  add_flag(FLAGS, name, "string", as.character(default), description)
}

#' @rdname flags
#' @export
flags_parse <- function(FLAGS,
                        config = Sys.getenv("R_CONFIG_ACTIVE", unset = "default"),
                        file = "flags.yml",
                        arguments = commandArgs(TRUE)) {

  # warn if the user has supplied a 'file' argument but no such file exists
  if (!missing(file) && !file.exists(file))
    warning(sprintf("configuration file '%s' does not exist", file))

  # read configuration file if it exists
  if (file.exists(file)) {

    # first load the raw YAML so we can provide defaults from FLAGS
    # (this obviates the need to also provide the defaults in flags.yml)
    config_yaml <- yaml::yaml.load_file(file)

    # check to see whether this is a profile-oriented config file

    # see what the structure of the config file is. if it's all lists at the
    # top level then treat it as a traditional profile-oriented config file
    if (is.list(config_yaml[["default"]])) {
      # synthesize default section based on what's already in the config file
      # and the defaults provided inline
      config_yaml[["default"]] <- config::merge(as.vector(FLAGS),
                                                config_yaml[["default"]])
    } else {
      # synthesize default section from the values at the top level
      default <- config_yaml
      config_yaml <- list()
      config_yaml[["default"]] <- default
    }

    # now write this to a temp file which we will read with config::get
    flags_tmp <- tempfile(pattern = "tfruns-flags", fileext = ".yml")
    on.exit(unlink(flags_tmp), add = TRUE)
    writeLines(yaml::as.yaml(config_yaml), flags_tmp)

    # read the config
    flags <- config::get(config = config, file = flags_tmp, use_parent = FALSE)

  } else {

    # no config file, just use default FLAGS
    flags <- as.vector(FLAGS)

  }

  # merge with command line arguments (if any)
  flags <- config::merge(flags, parse_command_line(arguments))

  # discover undeclared arguments
  undeclared <- setdiff(names(flags), names(FLAGS))
  if (length(undeclared) > 0) {
    stop("The following flags were provided but not declared: ",
         paste(undeclared), sep = ", ", call. = FALSE)
  }

  # set discovered values in FLAGS (set one at a time so that
  # we can do type coercion and preseve the attributes of FLAGS)
  names <- names(FLAGS)
  types <- attr(FLAGS, "types")
  for (i in 1:length(FLAGS)) {
    name <- names[[i]]
    type <- types[[i]]
    value <- switch(type,
      numeric = as.double(flags[[name]]),
      integer = as.integer(flags[[name]]),
      boolean = as.logical(flags[[name]]),
      string = as.character(flags[[name]])
    )
    FLAGS[[name]] <- value
  }

  # write flags
  write_run_data("flags", FLAGS)

  # return flags
  FLAGS
}

#' @export
as.data.frame.tfruns_flags <- function(x, ...) {
  data.frame(stringsAsFactors = FALSE,
    name = names(x),
    type = attr(x, "types"),
    value = as.character(x),
    description = attr(x, "descriptions")
  )
}

#' @export
print.tfruns_flags <- function(x, ...) {
  print(as.data.frame(x), row.names = FALSE)
}

# add a flag definition
add_flag <- function(FLAGS, name, type, default, description) {

  # validate uniqueness of name
  if (!is.null(FLAGS[[name]]))
    stop("A flag named '", name, "' has already been defined", call. = FALSE)

  # add flag
  FLAGS[[name]] <- default
  attr(FLAGS, "types") <- c(attr(FLAGS, "types"), type)
  attr(FLAGS, "defaults") <- c(attr(FLAGS, "defaults"), default)
  attr(FLAGS, "descriptions") <- c(attr(FLAGS, "descriptions"),
                                   ifelse(is.null(description), NA, description))

  # write flags
  write_run_data("flags", FLAGS)

  # return flags
  FLAGS
}

# parse command line arguments
parse_command_line <- function(arguments) {

  # initialize some state
  values <- list()

  i <- 0; n <- length(arguments)
  while (i < n) {
    i <- i + 1
    argument <- arguments[[i]]

    # skip any command line arguments without a '--' prefix
    if (!grepl("^--", argument))
      next

    # check to see if an '=' was specified for this argument
    equals_idx <- regexpr("=", argument)
    if (identical(c(equals_idx), -1L)) {
      # no '='; the next argument is the value for this key
      key <- substring(argument, 3)
      val <- arguments[[i + 1]]
      i <- i + 1
    } else {
      # found a '='; the next argument is all the text following
      # that character
      key <- substring(argument, 3, equals_idx - 1)
      val <- substring(argument, equals_idx + 1)
    }

    # convert '-' to '_' in key
    key <- gsub("-", "_", key)

    # update our map of argument values
    values[[key]] <- yaml::yaml.load(val)
  }

  values

}







