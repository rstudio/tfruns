library(testthat)
library(tfruns)

clean_runs(confirm = FALSE)
purge_runs(confirm = FALSE)

test_check("tfruns")
