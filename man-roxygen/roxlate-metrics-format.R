#' @md
#'
#' @section Metrics data format:
#'
#'   Metrics should be passed as a data frame with one column for each metric.
#'   If the metrics are not yet complete (e.g. only metrics for the
#'   first several epochs are provided) then metrics in yet to be completed
#'   epochs should use `NA` as their values. For example:
#'
#'   ```
#'   data.frame':	30 obs. of  4 variables:
#'   $ loss    : num  0.423 0.201 NA NA NA ...
#'   $ acc     : num  0.873 0.942 NA NA NA ...
#'   $ val_loss: num  0.174 0.121 NA NA NA ...
#'   $ val_acc : num  0.949 0.964 NA NA NA ...
#'   ```
#'
