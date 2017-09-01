

.globals <- new.env(parent = emptyenv())

# run dir
.globals$run_dir <- new.env(parent = emptyenv())
.globals$run_dir$path <- NULL
.globals$run_dir$flags <- NULL
.globals$run_dir$flags_file <- NULL
.globals$run_dir$pending_writes <- new.env(parent = emptyenv())

# view components
.globals$view_components <- new.env(parent = emptyenv())
