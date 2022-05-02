# Libraries ----
library(tidyverse)
library(rstan)
library(loo)

# Load stan_fit objects ----
load('results/vonbert_rpsm.rda')

# Get loo for each of the stan_fit objects ----
rpsm_loo <- loo(fit)
