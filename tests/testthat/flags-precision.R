
library(tfruns)

FLAGS <- flags(
  flag_numeric('learning_rate', 2e-5, 'Initial learning rate.'),
  flag_numeric('max_steps', 1e-6, 'Number of steps to run trainer.')
)

