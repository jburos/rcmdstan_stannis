testthat::context('rcmdstan')
library(rcmdstan)

eightschools_stan <- "
data {
  int<lower=0> J;         // number of schools
  real y[J];              // estimated treatment effects
  real<lower=0> sigma[J]; // standard error of effect estimates
}
parameters {
  real mu;                // population treatment effect
  real<lower=0> tau;      // standard deviation in treatment effects
  vector[J] eta;          // unscaled deviation from mu by school
}
transformed parameters {
  vector[J] theta = mu + tau * eta;        // school treatment effects
}
model {
  target += normal_lpdf(eta | 0, 1);       // prior log-density
  target += normal_lpdf(y | theta, sigma); // log-likelihood
}
"

eightschools_dat <- list(J = 8,
                         y = c(28,  8, -3,  7, -1,  1, 18, 12),
                         sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

test_dir <- file.path(testthat::test_path(), 'stannis_workingdir')

testthat::test_that('rcmdstan runs cmdstan with in-memory inputs & 1 chain', {
  fit <- rcmdstan::cmdstan(model_code = eightschools_stan, data = eightschools_dat, model_name = 'eightschools', working_dir = test_dir, chains = 1)
  # test that we have one chain
  testthat::expect_equal(length(dimnames(rstan::summary(fit)$c_summary)$chains), 1)
  fit
})

testthat::test_that('rcmdstan runs cmdstan with in-memory inputs & 4 chains', {
  fit <- rcmdstan::cmdstan(model_code = eightschools_stan, data = eightschools_dat, model_name = 'eightschools', working_dir = test_dir, chains = 4)
  # test that we have 4 chains
  testthat::expect_equal(length(dimnames(rstan::summary(fit)$c_summary)$chains), 4)
  # confirm that the 4 chains have different seeds
  testthat::expect_equal(length(unique(fit@stan_args %>% purrr::map(~ .$seed))), 4)
})
