# Front-end needs ----
# . Load necessary packages ----
library(rstan)
library(tidyverse)
library(tidyr)
library(reshape2)

# . Set options ----
options(mc.cores = parallel::detectCores())  
rstan_options(auto_write = TRUE)

# . Data Read ----
fish <- read.csv("data/SHADMASTER.csv", stringsAsFactors = FALSE)
  
# . Data manipulation ----
fish <- fish %>% 
  filter(!is.na(final_age) & !is.na(fork) & !is.na(rpsm) & !is.na(cohort))

# Run model ----
# Package the data for stan
  vb_data = list(
    length = fish$fork,
    age = fish$final_age,
    obs = seq(1, nrow(fish), 1),
    nobs = nrow(fish),
    group = as.numeric(as.factor(fish$sex)),
    ngroup = length(unique(fish$sex)),
    x = as.vector(scale(fish$rpsm)), # Live/dead, rmile, repeats
    p_linf = 0,
    p_linf_sd = 1,
    p_k = 0,
    p_k_sd = 1,  
    p_t0 = 0,
    p_t0_sd = 1,      
    p_sigma = 10,
    p_tau = 1,
    p_omega = 4,
    p_b_linf_sd = 1,
    p_b_k_sd = 1,
    p_b_t0_sd = 1
  )


# Fit the model with stan
rpsm_fit <- stan(file = 'models/vonbert_hmv_cov.stan',
            data = vb_data,
            chains = 3,
            iter = 200,
            warmup = 150,
            control = list(
              adapt_delta = 0.80,
              max_treedepth = 10
            )
            )

# Print model summary
print(rpsm_fit, digits=3)

# Save result to a file
save(rpsm_fit, file='results/vonbert_rpsm.rda')

# Results ----
# . Load result ----
# load("vonbert_covariate.rda")

# . Get parameter estimates ----
pars <- rstan::extract(rpsm_fit)  

# . Covariate significance and predictions ----
# .. Significance of x variable (latitude)
# Here are 90% CRIs on our coefficients, with median (0.50)
quantile(pars$ba_linf, c(0.05, 0.50, 0.95))
quantile(pars$ba_k, c(0.05, 0.50, 0.95))
quantile(pars$ba_t0, c(0.05, 0.50, 0.95))

# Here are some quick histograms
hist(pars$ba_linf)
hist(pars$ba_k)
hist(pars$ba_t0)

# .. Predictions from covariates
# First, get unique values to make a predictive line
new_rpsm <- unique(fish$rpsm)
new_rpsm_scaled <- as.vector(unique(scale(fish$rpsm)))

# Second, make predictions
#  .. Across both sexes ----
linf_mat <- matrix(0, nrow = length(pars$b0_linf), ncol = length(new_rpsm))
k_mat <- matrix(0, nrow = length(pars$b0_k), ncol = length(new_rpsm))
t0_mat <- matrix(0, nrow = length(pars$b0_t0), ncol = length(new_rpsm))

for(i in 1:nrow(linf_mat)){
  for(t in 1:ncol(linf_mat)){
    
    linf_mat[i, t] <- exp(
      mean(pars$Gamma[i, 1, ]) + 
        pars$b0_linf[i] + pars$ba_linf[i] * new_rpsm_scaled[t]
      )
    
    k_mat[i, t] <- exp(
      mean(pars$Gamma[i, 2, ]) + 
        pars$b0_k[i] + pars$ba_k[i] * new_rpsm_scaled[t]
      )
    
    t0_mat[i, t] <- exp(
      mean(pars$Gamma[i, 3, ]) + 
        pars$b0_t0[i] + pars$ba_t0[i] * new_rpsm_scaled[t]
      ) - 10
    
  }
}

# Summarize predictions
# L-infinity
linf_mean <- apply(linf_mat, MARGIN = 2, mean)
linf_lwr <- apply(linf_mat, MARGIN = 2, quantile, 0.05)
linf_upr <- apply(linf_mat, MARGIN = 2, quantile, 0.95)

linf_preds <- data.frame(
  RPSM = new_rpsm, 
  fit = linf_mean, 
  lwr = linf_lwr, 
  upr = linf_upr,
  param = "linf"
)

# K
k_mean <- apply(k_mat, MARGIN = 2, mean)
k_lwr <- apply(k_mat, MARGIN = 2, quantile, 0.05)
k_upr <- apply(k_mat, MARGIN = 2, quantile, 0.95)

k_preds <- data.frame(
  RPSM = new_rpsm, 
  fit = k_mean, 
  lwr = k_lwr, 
  upr = k_upr,
  param = "k"
)

# t0
t0_mean <- apply(t0_mat, MARGIN = 2, mean)
t0_lwr <- apply(t0_mat, MARGIN = 2, quantile, 0.05)
t0_upr <- apply(t0_mat, MARGIN = 2, quantile, 0.95)

t0_preds <- data.frame(
  RPSM = new_rpsm, 
  fit = t0_mean, 
  lwr = t0_lwr, 
  upr = t0_upr,
  param = "t0"
)

# Smoosh them all together
temp <- rbind(linf_preds, k_preds)
preds <- rbind(temp, t0_preds)

# Give the parameters cool names we can plot
preds$param <- factor(
  preds$param,
  levels = c("linf", "k", "t0"),
  labels = c(
    linf = expression(italic(L[infinity])), 
    k = 'italic(K)',
    t0 = 'italic(t[0])'
  ))


# Now, we can get individual Linf, K, t0 from model
# to plot against our predictions
Linf_mean <- apply(pars$Linf, 2, mean)
K_mean <- apply(pars$K, 2, mean)
T0_mean <- apply(pars$t0, 2, mean)

# Smoosh those back in OG data
fish_preds <- 
  data.frame(
    fish, linf = Linf_mean, k = K_mean, t0 = T0_mean
  )

# Stack the data on parameter
fish_long <- fish_preds %>% 
  tidyr::pivot_longer(
    cols = linf:t0,
    names_to = "param",
    values_to = "Estimate"
  )

# Group by latitude and parameter to get summaries
fish_summary <- fish_long %>% 
  group_by(rpsm, param) %>% 
  summarize(
    fit = mean(Estimate),
    lwr = quantile(Estimate, 0.05),
    upr = quantile(Estimate, 0.95),
  )

# Give the parameters cool names we can plot
fish_summary$param <- factor(
  fish_summary$param,
  levels = c("linf", "k", "t0"),
  labels = c(
    linf = expression(italic(L[infinity])), 
    k = 'italic(K)',
    t0 = 'italic(t[0])'
  ))

# Plot the predictions
ggplot(preds, aes(x = RPSM, y = fit)) +
  geom_line() +
  geom_point(data = fish_summary, aes(x = rpsm, y = fit)) +
  geom_segment(data = fish_summary, 
               aes(x = rpsm, xend = rpsm, y = lwr, yend = upr)) +
  geom_ribbon(
    aes(xmax = RPSM, ymin = lwr, ymax = upr, color = NULL),
    alpha = 0.10) +
  facet_wrap(~param, scales = "free_y", labeller = label_parsed) +
  ylab("Parameter value") +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 3)
  )









# NOT SURE WHAT'S BELOW THIS? ----

# First, get unique values to make a predictive line
new_lat_scaled <- sample(scale(fish$lat), length(pars$b0_k), replace = TRUE)

# . Von Bert Curves the hard way ----
#  .. Males ----

Age <- seq(min(fish$first_age), max(fish$first_age), 1)

linf_male <- exp(
  pars$Gamma[, 1, 1] + pars$b0_linf[i] + pars$ba_linf[i] * new_lat_scaled[i])
k_male <- exp(
  pars$Gamma[, 2, 1] + pars$b0_k[i] + pars$ba_k[i] * new_lat_scaled[i])
t0_male <- exp(
  pars$Gamma[, 3, 1] + pars$b0_t0[i] + pars$ba_t0[i] * new_lat_scaled[i]) - 10

vb_male <- matrix(NA, nrow = length(linf_male), ncol = length(Age))
for(i in 1:length(linf_male)){
  for(t in 1:length(Age)){
    vb_male[i, t] <- linf_male[i] * (1 - exp(-k_male[i] * (Age[t] - t0_male[i])))
  }
}

apply(vb_male, 2, mean)


