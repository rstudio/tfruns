
library(tfruns)

FLAGS <- flags(
  flag_numeric('learning_rate', 0.01, 'Initial learning rate.'),
  flag_integer('max_steps', 5000, 'Number of steps to run trainer.')
)

metrics <- readRDS("metrics.rds")
write_run_metadata("metrics", metrics)

write_run_metadata("properties", list(foo = "bar", index = 42))

write_run_metadata("foo", function(data_dir) {
  file.create(file.path(data_dir, "foo"))
})

writeLines(c("1", "2", "3"), 'extra.dat')
writeLines(c("1", "2", "3"), file.path('subdir', 'extra.dat'))
