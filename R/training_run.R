#' Run a training script
#'
#' @inheritParams flags
#' @inheritParams ls_runs
#' @param file Path to training script (defaults to "train.R")
#' @param context Run context (defaults to "local")
#' @param flags Named list with flag values (see [flags()]) or path
#'   to YAML file containing flag values.
#' @param properties Named character vector with run properties. Properties are
#'   additional metadata about the run which will be subsequently available via
#'   [ls_runs()].
#' @param run_dir Directory to store run data within
#' @param artifacts_dir Directory to capture created and modified files within.
#'   Pass `NULL` to not capture any artifcats.
#' @param echo Print expressions within training script
#' @param view View the results of the run after training. The default "auto"
#'   will view the run when executing a top-level (printed) statement in an
#'   interactive session. Pass `TRUE` or `FALSE` to control whether the view is
#'   shown explictly. You can also pass "save" to save a copy of the
#'   run report at `tfruns.d/view.html`
#' @param envir The environment in which the script should be evaluated
#' @param encoding The encoding of the training script; see [file()].
#'
#' @return Single row data frame with run flags, metrics, etc.
#'
#' @details The training run will by default use a unique new run directory
#' within the "runs" sub-directory of the current working directory (or to the
#' value of the `tfruns.runs_dir` R option if specified).
#'
#' The directory name will be a timestamp (in GMT time). If a duplicate name is
#' generated then the function will wait long enough to return a unique one.
#'
#' If you want to use an alternate directory to store run data you can either
#' set the global `tfruns.runs_dir` R option, or you can pass a `run_dir`
#' explicitly to `training_run()`, optionally using the [unique_run_dir()]
#' function to generate a timestamp-based directory name.
#'
#' @export
training_run <- function(file = "train.R",
                         context = "local",
                         config = Sys.getenv("R_CONFIG_ACTIVE", unset = "default"),
                         flags = NULL,
                         properties = NULL,
                         run_dir = NULL,
                         artifacts_dir = getwd(),
                         echo = TRUE,
                         view = "auto",
                         envir = parent.frame(),
                         encoding = getOption("encoding")) {

  # if file is not specified then see if there is a single R source file
  # within the current working directory
  if (missing(file)) {
    all_r_scripts <- list.files(pattern = utils::glob2rx("*.r"), ignore.case = TRUE)
    if (length(all_r_scripts) == 1)
      file <- all_r_scripts
  }

  # verify that the file exists
  if (!file.exists(file))
    stop("The R script '", file, "' does not exist.")

  # setup run context
  run_dir <- initialize_run(
    file = file,
    type = "training",
    context = context,
    config = config,
    flags = flags,
    properties = properties,
    run_dir = run_dir
  )

  # execute the training run
  do_training_run(file, run_dir, artifacts_dir, echo = echo, envir = envir, encoding = encoding)

  # check for forced view
  force_view <- isTRUE(view)

  # result "auto" if necessary
  if (identical(view, "auto"))
    view <- interactive()

  # print completed message
  message('\nRun completed: ', run_dir, '\n')

  # prepare to return the run
  run_return <- return_runs(run_record(run_dir))

  # force_view means we do the view (i.e. we don't rely on printing)
  if (force_view) {

    view_run(run_dir)
    invisible(run_return)

  # regular view means give it a class that will result in a view
  # when executed as a top-level statement
  } else if (isTRUE(view)) {

    class(run_return) <- c("tfruns_viewed_run", class(run_return))
    run_return

  # save a copy of the run view
  } else if (identical(view, "save")) {

    save_run_view(run_dir, file.path(run_dir, "tfruns.d", "view.html"))
    invisible(run_return)

  # otherwise just return invisibly
  } else {

    invisible(run_return)

  }
}


#' Tune hyperparameters using training flags
#'
#' Run all combinations of the specifed training flags. The number of
#' combinations can be reduced by specifying the `sample` parameter, which
#' will result in a random sample of the flag combinations being run.
#'
#' @inheritParams training_run
#' @inheritParams ls_runs
#'
#' @param flags Either a named list with flag values (multiple values can be
#'   provided for each flag) or a data frame that contains pre-generated
#'   combinations of flags (e.g. via [base::expand.grid()]). The latter can
#'   be useful for subsetting combinations. See 'Examples'.
#' @param sample Sampling rate for flag combinations (defaults to
#'   running all combinations).
#' @param confirm Confirm before executing tuning run.
#'
#' @return Data frame with summary of all training runs performed
#'   during tuning.
#'
#' @examples
#' \dontrun{
#' library(tfruns)
#'
#' # using a list as input to the flags argument
#' runs <- tuning_run(
#'   system.file("examples/mnist_mlp/mnist_mlp.R", package = "tfruns"),
#'   flags = list(
#'     dropout1 = c(0.2, 0.3, 0.4),
#'     dropout2 = c(0.2, 0.3, 0.4)
#'   )
#' )
#' runs[order(runs$eval_acc, decreasing = TRUE), ]
#'
#' # using a data frame as input to the flags argument
#' # resulting in the same combinations above, but remove those
#' # where the combined dropout rate exceeds 1
#' grid <- expand.grid(
#'   dropout1 = c(0.2, 0.3, 0.4),
#'   dropout2 = c(0.2, 0.3, 0.4)
#' )
#' grid$combined_droput <- grid$dropout1 + grid$dropout2
#' grid <- grid[grid$combined_droput <= 1, ]
#' runs <- tuning_run(
#'   system.file("examples/mnist_mlp/mnist_mlp.R", package = "tfruns"),
#'   flags = grid[, c("dropout1", "dropout2")]
#' )
#' }
#' @export
tuning_run <- function(file = "train.R",
                       context = "local",
                       config = Sys.getenv("R_CONFIG_ACTIVE", unset = "default"),
                       flags = NULL,
                       sample = NULL,
                       properties = NULL,
                       runs_dir = getOption("tfruns.runs_dir", "runs"),
                       artifacts_dir = getwd(),
                       echo = TRUE,
                       confirm = interactive(),
                       envir = parent.frame(),
                       encoding = getOption("encoding")) {

  if (!is.list(flags) || is.null(names(flags))) {
    stop("flags must be specified as a named list or a data frame")
  }


   # set runs dir if specified
   old_runs_dir <- getOption("tfruns.runs_dir")
   options(tfruns.runs_dir = runs_dir)
   if (!is.null(old_runs_dir))
     on.exit(options(tfruns.runs_dir = old_runs_dir), add = TRUE)

   # calculate the flag grid
   if (!is.data.frame(flags)) {
     flag_grid <- do.call(function(...) expand.grid(..., stringsAsFactors = FALSE), flags)
     message(prettyNum(nrow(flag_grid), big.mark = ","), " total combinations of flags ")
   } else {
     flag_grid <- flags
   }

   # sample if requested
   if (!is.null(sample)) {
     if (sample > 1)
       stop("sample must be a floating point value less or equal to 1")
     if (sample <= 0)
       stop("sample must be a floating point value greater than 0")
     indices <- sample.int(nrow(flag_grid), size = ceiling(sample * nrow(flag_grid)))
     flag_grid <- flag_grid[indices, , drop = FALSE]
     message("(sampled to ", prettyNum(nrow(flag_grid), big.mark = ","), " combinations)\n")
   } else {
     message("(use sample parameter to run a random subset)")
   }

   if (confirm) {
     prompt <- readline("Proceed with tuning run? [Y/n]: ")
     if (nzchar(prompt) && tolower(prompt) != 'y')
       return(invisible(NULL))
   }

   # execute tuning
   for (i in 1:nrow(flag_grid)) {

     # flags
     flags = as.list(flag_grid[i,, drop = FALSE])
     message(sprintf(
       "Training run %d/%d (flags = %s) ",
       i,
       nrow(flag_grid),
       deparse(flags, control = c("keepInteger", "keepNA"))
     ))

     # execute run
     training_run(
       file = file,
       config = config,
       flags = flags,
       properties = properties,
       run_dir = NULL,
       artifacts_dir = artifacts_dir,
       echo = echo,
       view = FALSE,
       envir = new.env(parent = envir),
       encoding = encoding
     )
   }

   # return data frame with all runs
   invisible(ls_runs(latest_n = nrow(flag_grid), runs_dir = runs_dir))
}


#' @export
print.tfruns_viewed_run <- function(x, ...) {
  view_run(x)
}


do_training_run <- function(file, run_dir, artifacts_dir, echo, envir, encoding) {

  with_changed_file_copy(artifacts_dir, run_dir, {

    # write script
    write_run_property("script", basename(file))

    # write begin and end times
    write_run_property("start", as.double(Sys.time()))
    on.exit(write_run_property("end", as.double(Sys.time())), add = TRUE)

    # clear run on exit
    on.exit(clear_run(), add = TRUE)

    # clear TF session on exist
    on.exit(reset_tf_graph(), add = TRUE)

    # set width for run
    old_width <- getOption("width")
    options(width = min(100, old_width))
    on.exit(options(width = old_width), add = TRUE)

    # sink output
    properties_dir <- file.path(meta_dir(run_dir), "properties")
    output_file <- file(file.path(properties_dir, "output"), open = "wt", encoding = "UTF-8")
    sink(file = output_file, type = "output", split = TRUE)
    on.exit({ sink(type = "output"); close(output_file); }, add = TRUE)

    # sink plots
    plots_dir <- file.path(run_dir, "plots")
    if (!utils::file_test("-d", plots_dir))
      dir.create(plots_dir, recursive = TRUE)
    png_args <- list(
      filename = file.path(plots_dir, "Rplot%03d.png"),
      width=1200, height=715, res = 192 # ~ golden ratio @ highdpi
    )
    if (is_windows() && capabilities("cairo"))  # required to prevent empty plot
      png_args$type <- "cairo"                  # emitted when type = "windows"
    do.call(grDevices::png, png_args)
    dev_number <- grDevices::dev.cur()
    on.exit(grDevices::dev.off(dev_number), add = TRUE)

    # notify user of run dir
    message("Using run directory ", run_dir)

    # perform the run
    write_run_property("completed", FALSE)
    withCallingHandlers({
      source(file = file, local = envir, echo = echo, encoding = encoding)
      write_run_property("completed", TRUE)
    },
    error = function(e) {

      # write error
      write_run_metadata("error", list(
        message = e$message,
        traceback = capture_stacktrace(sys.calls())
      ))

      # forward error
      stop(e)
    })
  })
}


initialize_run <- function(file,
                           type = "training",
                           context = "local",
                           config = Sys.getenv("R_CONFIG_ACTIVE", unset = "default"),
                           flags = NULL,
                           properties = NULL,
                           run_dir = NULL) {

  # clear any existing run
  clear_run()

  # generate the run_dir
  if (is.null(run_dir))
    run_dir <- unique_run_dir()

  # create the directory if necessary
  if (!utils::file_test("-d", run_dir))
    if (!dir.create(run_dir, recursive = TRUE))
      stop("Unable to create run directory at ", run_dir)

  # if flags is a YAML file then read the flags from the file
  if (is.character(flags) && length(flags) == 1 &&
      is.null(names(flags)) && file.exists(flags)) {
    flags_file <- flags
    flags <- NULL
  } else {
    flags_file <- NULL
  }

  # this is new definition for the run_dir, save it
  .globals$run_dir$path <- run_dir

  # save config and flags (they'll get processed later in flags())
  .globals$run_dir$config <- config
  .globals$run_dir$flags <- flags
  .globals$run_dir$flags_file <- flags_file

  # write type and context
  write_run_metadata("properties", list(
    type = type,
    context = context
  ))

  # write properties
  write_run_metadata("properties", properties)

  # write source files
  write_run_metadata("source", dirname(file))

  # execute any pending writes
  for (name in ls(.globals$run_dir$pending_writes))
    .globals$run_dir$pending_writes[[name]](meta_dir(run_dir))

  # return invisibly
  invisible(run_dir)
}

clear_run <- function() {
  .globals$run_dir$path <- NULL
  .globals$run_dir$config <- NULL
  .globals$run_dir$flags <- NULL
  .globals$run_dir$flags_file <- NULL
  .globals$run_dir$pending_writes <- new.env(parent = emptyenv())
}

reset_tf_graph <- function() {
  tryCatch({
    if (reticulate::py_module_available("tensorflow")) {
      tf <- reticulate::import("tensorflow")
      if (tf_version(tf) >= "1.13" && !tf$executing_eagerly())
        if(tf_version(tf) >= "2.0") {
          tf$compat$v1$reset_default_graph()
        } else {
          tf$reset_default_graph()
        }
      if (reticulate::py_has_attr(tf, "keras"))
        tf$keras$backend$clear_session()
      else if (reticulate::py_has_attr(tf$contrib, "keras"))
        tf$contrib$keras$backend$clear_session()
    }
    if (reticulate::py_module_available("keras")) {
      keras <- reticulate::import("keras")
      if (reticulate::py_has_attr(keras$backend, "clear_session"))
        keras$backend$clear_session()
    }
  }, error = function(e) {
    warning("Error occurred resetting tf graph: ", e$message)
  })
}

capture_stacktrace <- function(stack) {
  stack <- stack[-(2:7)]
  stack <- utils::head(stack, -2)
  stack <- vapply(stack, function(frame) {
    frame <- deparse(frame)
    frame <- paste(frame, collapse = "\n")
    frame
  }, FUN.VALUE = "frame")
  rev(stack)
}

with_changed_file_copy <- function(artifacts_dir, run_dir, expr) {

  if (!is.null(artifacts_dir)) {
    # snapshot the files in the training script directory before training
    files_before <- utils::fileSnapshot(path = artifacts_dir, recursive = TRUE)

    # copy changed files on exit
    on.exit({
      # snapshot the files in the run_dir after training then compute changed files
      files_after <- utils::fileSnapshot(path = artifacts_dir, recursive = TRUE)
      changed_files <- utils::changedFiles(files_before, files_after)

      # filter out files within the run_dir and packrat/gs files (cloudml)
      changed_files <- c(changed_files$changed, changed_files$added)
      changed_files <- changed_files[!grepl(basename(run_dir), changed_files)]
      changed_files <- changed_files[!grepl("^packrat[/\\]", changed_files)]
      changed_files <- changed_files[!grepl("^gs[/\\]", changed_files)]

      # copy the changed files to the run_dir
      for (file in changed_files) {
        dir <- dirname(file)
        target_dir <- file.path(run_dir, dir)
        if (!utils::file_test("-d", target_dir))
          dir.create(target_dir, recursive = TRUE)
        file.copy(from = file.path(artifacts_dir, file), to = target_dir)
      }
    }, add = TRUE)
  }

  # execute the expression
  force(expr)
}


#' Save a run view as HTML
#'
#' The saved view includes summary information (flags, metrics, model
#' attributes, etc.), plot and console output, and the code used for the run.
#'
#' @inheritParams run_info
#' @param filename Path to save the HTML to. If no `filename` is specified
#'   then a temporary file is used (the path to the file is returned invisibly).
#'
#' @seealso [ls_runs()], [run_info()], [view_run()]
#'
#' @import base64enc
#'
#' @export
save_run_view <- function(run_dir = latest_run(), filename = "auto") {

  # verify run_dir
  if (is.null(run_dir))
    stop("No runs available in the current directory")

  # get run view data
  run <- run_info(run_dir)
  data <- run_view_data(run)

  # generate filename if needed
  if (identical(filename, "auto"))
    filename <- viewer_temp_file(paste0("run-", basename(run$run_dir)))

  # save the report
  save_page("view_run", data = data, filename)

  # return the path saved to
  invisible(filename)
}



#' View a training run
#'
#' View metrics and other attributes of a training run.
#'
#' @inheritParams run_info
#' @param viewer Viewer to display training run information within
#'   (default to an internal page viewer if available, otherwise
#'   to the R session default web browser).
#'
#' @seealso [ls_runs()], [run_info()]
#'
#' @import base64enc
#'
#' @export
view_run <- function(run_dir = latest_run(), viewer = getOption("tfruns.viewer")) {

  # verify run_dir
  if (is.null(run_dir))
    stop("No runs available in the current directory")

  # generate run report and view it
  view_page(save_run_view(run_dir), viewer)
}

#' Save a run comparison as HTML
#'
#' @inheritParams save_run_view
#'
#' @param runs Character vector of 2 training run directories or
#'   data frame returned from [ls_runs()] with at least 2 elements.
#'
#' @export
save_run_comparison <- function(runs = ls_runs(latest_n = 2), filename = "auto") {

  # cast to run_info
  runs <- run_info(runs)

  # verify exactly 2 runs provided
  if (length(runs) != 2)
    stop("You must pass at least 2 run directories to compare_runs")

  # generate filename if needed
  if (identical(filename, "auto")) {
    filename <- viewer_temp_file(paste("compare-runs",
                                       basename(runs[[1]]$run_dir),
                                       basename(runs[[2]]$run_dir),
                                       sep = "-"))
  }

  # runs to compare (order least to most recent)
  if (runs[[1]]$start > runs[[2]]$start) {
    run_a <- runs[[2]]
    run_b <- runs[[1]]
  } else {
    run_a <- runs[[1]]
    run_b <- runs[[2]]
  }

  # get view_data
  run_a <- run_view_data(run_a)
  run_b <- run_view_data(run_b)

  # generate diffs
  diffs <- run_diffs(run_a, run_b)

  # data for view
  data <- list(run_a = run_a, run_b = run_b, diffs = diffs)

  # save the report
  save_page("compare_runs", data = data, filename)

  # return the path saved to
  invisible(filename)
}

#' Compare training runs
#'
#' Render a visual comparison of two training runs. The runs are
#' displayed with the most recent run on the right and the
#' earlier run on the left.
#'
#' @inheritParams view_run
#' @inheritParams save_run_comparison
#'
#' @export
compare_runs <- function(runs = ls_runs(latest_n = 2),
                         viewer = getOption("tfruns.viewer")) {

  # verify runs
  if (is.null(runs))
    stop("No runs available in the current directory")

  # save and view
  view_page(save_run_comparison(runs), viewer)
}

run_view_data <- function(run) {

  # helper to extract prefaced properties
  with_preface <- function(preface, strip_preface = TRUE) {
    preface_pattern <- paste0("^", preface, "_")
    prefaced <- run[grepl(preface_pattern, names(run))]
    if (length(prefaced) > 0) {
      if (strip_preface)
        names(prefaced) <- sub(preface_pattern, "", names(prefaced))
      prefaced
    } else {
      NULL
    }
  }

  # helpers for formatting numbers
  format_integer <- function(x) {
    if (!is.null(x))
      prettyNum(x, mode = "integer", big.mark = ",")
    else
      NULL
  }
  format_numeric <- function(x, digits = 4) {
    if (!is.null(x))
      formatC(x, format='f', digits= digits)
    else
      NULL
  }

  # default some potentially empty sections to null
  data <- list(
    cloudml = NULL,
    history = NULL,
    model = NULL,
    metrics = NULL,
    evaluation = NULL,
    optimization = NULL,
    training = NULL,
    flags = NULL,
    files = NULL,
    plots = NULL,
    output = NULL,
    error = NULL
  )

  # run_dir
  data$run_dir <- basename(run$run_dir)

  # attributes
  script <- basename(run$script)
  data$attributes <- list(
    context = run$context,
    script = script,
    started = paste(as.POSIXct(run$start, origin="1970-01-01", tz = "GMT"),
                    "GMT"),
    time = format(as.POSIXct(as.character(Sys.Date()), tz = "GMT") +
                    run$end - run$start,
                  "%H:%M:%S")
  )

  # cloudml attributes
  if (identical(run$context, "cloudml")) {
    cloudml <- with_preface("cloudml")
    if (!is.null(cloudml)) {
      data$cloudml <- list()
      data$cloudml$job <- list(
        href = cloudml$console_url,
        text = cloudml$job
      )
      data$cloudml$logs <- list(
        href = cloudml$log_url,
        text = "View logs"
      )
      data$cloudml$master_type <- cloudml$master_type
      data$cloudml$status <- cloudml$state
      data$cloudml$created <- paste(
        as.POSIXct(as.numeric(cloudml$created),
                   origin="1970-01-01", tz = "GMT"),
        "GMT"
      )
      data$cloudml$time <- format(as.POSIXct(as.character(Sys.Date()), tz = "GMT") +
                     as.numeric(cloudml$end) - as.numeric(cloudml$created),
                     "%H:%M:%S")
      data$cloudml$ml_units <- cloudml$ml_units
    }
  }

  # metrics
  metrics <- with_preface("metric")
  if (!is.null(metrics))
    data$metrics <- lapply(metrics, format_numeric)

  # evaluation
  evaluation <- with_preface("eval", strip_preface = FALSE)
  if (!is.null(evaluation))
    data$evaluation <- evaluation

  # flags
  flags <- with_preface("flag")
  if (!is.null(flags))
    data$flags <- flags

  # optimization
  optimization <- list()
  optimization$loss <- run$loss_function
  optimization$optimizer <- run$optimizer
  optimization$lr <- run$learning_rate
  if (length(optimization) > 0)
    data$optimization <- optimization

  # training

  # determine the step units
  steps_unit <- get_steps_unit(run)
  steps_completed_unit <- get_steps_completed_unit(steps_unit)

  # format the steps
  if (!is.null(run[[steps_unit]]) && !is.null(run[[steps_completed_unit]])) {
    if (run[[steps_unit]] > run[[steps_completed_unit]])
      steps <- paste(format_integer(run[[steps_completed_unit]]),
                     format_integer(run[[steps_unit]]),
                     sep = "/")
    else
      steps <- format_integer(run[[steps_unit]])
  } else {
    steps <- NULL
  }
  training <- list()
  training$samples <- format_integer(run$samples)
  training$validation_samples <- format_integer(run$validation_samples)
  training[[steps_unit]] <- steps
  training$batch_size <- format_integer(run$batch_size)
  if (length(training) > 0)
    data$training <- training

  # error
  if (!is.null(run$error_message)) {
    data$error <- list(
      message = run$error_message,
      traceback = run$error_traceback
    )
  }

  # metrics history
  if (!is.null(run$metrics))
    data$history <- run$metrics

  # model summary
  if (!is.null(run$model)) {
    data$model <- sub("^Model\n", "", run$model)
    data$model <- sub("^_+\n", "", data$model)
  }

  # initialize tabs
  data$tabs <- list(
    list(href = "#summary", title = "Summary"),
    list(href = "#output", title = "Output"),
    list(href = "#code", title = "Code")
  )

  # determine if we have an output tab (remove it if we don't)
  data$output_tab <- !is.null(data$error) ||
    !is.null(data$history) ||
    !is.null(data$model)
  if (!data$output_tab)
    data$tabs[[2]] <- NULL

  # source code
  data$source_code <- run_source_code(script, run$run_dir)

  # files
  files <- list.files(run$run_dir, recursive = FALSE)
  files <- gsub("\\\\", "/", files)
  files <- files[!files %in% c("tfruns.d", "plots", "logs")]
  if (length(files) > 0) {
    data$files <- lapply(files, function(file) {
      list(
        name = file,
        directory = utils::file_test("-d", file.path(run$run_dir, file))
      )
    })
  }

  # plots
  plots <- list.files(file.path(run$run_dir, "plots"),
                      pattern = utils::glob2rx("*.png"),
                      full.names = TRUE)
  if (length(plots) > 0) {
    data$plots <- lapply(plots, function(plot) {
      base64enc::dataURI(file = plot,
                         mime = "image/png",
                         encoding = "base64")
    })
  }

  # output
  if (!is.null(run$output))
    data$output <- run$output

  # return data
  data
}


run_diffs <- function(run_a, run_b) {

  diffs <- list()

  # FLAGS
  run_flags <- function(run) ifelse(is.null(run$flags), "", yaml::as.yaml(run$flags))
  run_a_flags <- run_flags(run_a)
  run_b_flags <- run_flags(run_b)
  if (!identical(run_a_flags, run_b_flags)) {
    diffs[["FLAGS"]] <- list(
      run_a = run_a_flags,
      run_b = run_b_flags
    )
  }

  # source code: changes and deletions
  for (source_file in names(run_a$source_code)) {
    run_a_source_file <- run_a$source_code[[source_file]]
    run_b_source_file <- run_b$source_code[[source_file]]
    if (is.null(run_b_source_file))
      run_b_source_file <- ""
    if (!identical(run_a_source_file, run_b_source_file)) {
      diffs[[source_file]] <- list(
        run_a = run_a_source_file,
        run_b = run_b_source_file
      )
    }
  }
  # source code: additions
  new_source_files <- setdiff(names(run_b$source_code), names(run_a$source_code))
  for (source_file in new_source_files) {
    diffs[[source_file]] <- list(
      run_a = "",
      run_b = run_b$source_code[[source_file]]
    )
  }

  # return diffs
  diffs
}



