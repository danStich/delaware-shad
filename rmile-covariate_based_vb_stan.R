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
  filter(!is.na(final_age) & !is.na(fork) & !is.na(r_mile) & !is.na(cohort))

plot(fish$final_age, fish$fork)

fish <- fish %>% 
  filter(!(final_age == 1 & fork > 200) &
         !(final_age == 5 & fork < 200))

plot(fish$final_age, fish$fork)


# Run model ----
# Package the data for stan
  vb_data = list(
    length = fish$fork,
    age = fish$final_age,
    obs = seq(1, nrow(fish), 1),
    nobs = nrow(fish),
    group = rep(1, nrow(fish)),
    ngroup = 1,
    x = as.vector(scale(fish$r_mile)), 
    p_linf = 0,
    p_linf_sd = 1,
    p_k = 0,
    p_k_sd = 1,  
    p_t0 = 0,
    p_t0_sd = 1,      
    p_sigma = 50,
    p_tau = 2.5,
    p_omega = 4,
    p_b_linf_sd = 1,
    p_b_k_sd = 1,
    p_b_t0_sd = 1,
    nu_shape = 6,
    nu_scale = 0.1      
  )


# Fit the model with stan
r_mile_fit <- stan(file = 'models/vonbert_hmv_cov.stan',
            data = vb_data,
            chains = 3,
            iter = 200,
            warmup = 100,
            control = list(
              adapt_delta = 0.80,
              max_treedepth = 10
            )
            )

# Print model summary
print(r_mile_fit, digits=3)

# Save result to a file
save(r_mile_fit, file='results/vonbert_r_mile.rda')

# Results ----
# . Load result ----
load("vonbert_r_mile.rda")

# . Get parameter estimates ----
pars <- rstan::extract(r_mile_fit)  

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
new_r_mile <- unique(fish$r_mile)
new_r_mile_scaled <- as.vector(unique(scale(fish$r_mile)))

# Second, make predictions
#  .. Across both sexes ----
linf_mat <- matrix(0, nrow = length(pars$b0_linf), ncol = length(new_r_mile))
k_mat <- matrix(0, nrow = length(pars$b0_k), ncol = length(new_r_mile))
t0_mat <- matrix(0, nrow = length(pars$b0_t0), ncol = length(new_r_mile))

for(i in 1:nrow(linf_mat)){
  for(t in 1:ncol(linf_mat)){
    
    linf_mat[i, t] <- exp(
      mean(pars$Gamma[i, 1, ]) + 
        pars$b0_linf[i] + pars$ba_linf[i] * new_r_mile_scaled[t]
      )
    
    k_mat[i, t] <- exp(
      mean(pars$Gamma[i, 2, ]) + 
        pars$b0_k[i] + pars$ba_k[i] * new_r_mile_scaled[t]
      )
    
    t0_mat[i, t] <- exp(
      mean(pars$Gamma[i, 3, ]) + 
        pars$b0_t0[i] + pars$ba_t0[i] * new_r_mile_scaled[t]
      ) - 10
    
  }
}

# Summarize predictions
# L-infinity
linf_mean <- apply(linf_mat, MARGIN = 2, mean)
linf_lwr <- apply(linf_mat, MARGIN = 2, quantile, 0.05)
linf_upr <- apply(linf_mat, MARGIN = 2, quantile, 0.95)

linf_preds <- data.frame(
  RMILE = new_r_mile, 
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
  RMILE = new_r_mile, 
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
  RMILE = new_r_mile, 
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
  group_by(r_mile, param) %>% 
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
ggplot(preds, aes(x = RMILE, y = fit)) +
  geom_line() +
  geom_point(data = fish_summary, aes(x = r_mile, y = fit)) +
  geom_segment(data = fish_summary, 
               aes(x = r_mile, xend = r_mile, y = lwr, yend = upr)) +
  geom_ribbon(
    aes(xmax = RMILE, ymin = lwr, ymax = upr, color = NULL),
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


