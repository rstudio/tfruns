
# empty train script callbed yby training_run in tests


library(tfruns)

FLAGS <- flags(
  flag_float("learning_rate", 0.01, "Learning rate")
)
