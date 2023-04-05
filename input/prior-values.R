

# Homework 06: question 1 -------------------------------------------------

values_priors <- tibble(
  model_name = c("prior_1", "prior_0.1", "prior_10", "prior_100"),
  model_name_sym = lapply(model_name, as.symbol),
  prior_level = as.integer(c(1, 0.1, 10, 100)))
