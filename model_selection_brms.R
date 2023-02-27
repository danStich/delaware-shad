# Libraries ----
library(tidyverse)
library(brms)
library(loo)

# Model files ----
load("results/base_fit.rda")
load("results/rps_fit.rda")
load("results/rpsm_fit.rda")
load("results/sex_fit.rda")
load("results/r_mile_fit.rda")
load("results/site_fit.rda")
load("results/live_dead_fit.rda")


# Loo objections ----
base_loo <- loo(base_fit)
rps_loo <- loo(rps_fit)
rpsm_loo <- loo(rpsm_fit)
sex_loo <- loo(sex_fit)
r_mile_loo <- loo(r_mile_fit)
site_loo <- loo(site_fit)
live_dead_loo <- loo(live_dead_fit)


# Model comparison ----
loo_compare(base_loo, rps_loo, rpsm_loo, sex_loo, r_mile_loo, site_loo,
            live_dead_loo)

# Model       elpd_diff   se_diff
# sex_fit          0.0       0.0 # Clear best model here
# r_mile_fit     -79.3      13.4 # Distance upstream slightly better than just lower, middle, upper
# site_fit       -88.5      13.3 # This is better than the null model, but not by much
# rps_fit        -90.4      12.1 # This is better than the null model, but not by much
# rpsm_fit       -94.1      11.7 # This is better than the null model, but not by much
# live_dead_fit  -96.6      11.9 # This is better than the null model, but not by much
# base_fit      -104.5      11.7 # This is the null model

# Combined sex and r_mile model ----
# I fit this model after doing model selection to get the most important 
# variables
load("results/bests_fit.rda") 
bests_loo <- loo(bests_fit)
loo_compare(sex_loo, bests_loo)

