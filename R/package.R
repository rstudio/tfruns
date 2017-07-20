

.globals <- new.env(parent = emptyenv())
.globals$run_dir <- new.env(parent = emptyenv())
.globals$run_dir$path <- NULL
.globals$run_dir$pending_writes <- new.env(parent = emptyenv())
