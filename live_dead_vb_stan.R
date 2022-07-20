# Front-end needs ----
# . Load necessary packages ----
library(rstan)
library(tidyverse)
library(tidyr)
library(reshape2)

# . Set options ----
options(mc.cores = parallel::detectCores(4))  
rstan_options(auto_write = TRUE)

# . Data Read ----
fish <- read.csv("data/SHADMASTER.csv", stringsAsFactors = FALSE)

# . Data manipulation ----
fish <- fish %>% 
  filter(!is.na(final_age) & !is.na(fork) & !is.na(live_dead) & !is.na(cohort))

# Run model ----
# Package the data for stan
vb_data = list(
  length = fish$fork,
  age = fish$final_age,
  nobs = nrow(fish),
  group = as.numeric(as.factor(fish$sex)),
  ngroups = length(unique(fish$sex)),
  hp_tau = 1,
  hp_sigma = 10,
  hp_omega = 4,
  p_b = 0,
  p_b_sd = 1
)


# Fit the model with stan
live_dead_fit <- stan(file = 'models/vonbert_group.stan',
                 data = vb_data,
                 chains = 3,
                 iter = 500,
                 warmup = 300,
                 control = list(
                   adapt_delta = 0.80,
                   max_treedepth = 10
                 )
)

# Print model summary
print(live_dead_fit, digits=3)

# Save result to a file
save(live_dead_fit, file='results/vonbert_live_dead.rda')

# Results ----
# . Load result ----
# load("vonbert_covariate.rda")

# . Get parameter estimates ----
pars <- rstan::extract(live_dead_fit)  

# - Results ----
# . Load result ----
# load("live_dead_fit.rda")

# . Get parameter estimates ----
pars <- rstan::extract(live_dead_fit)  

linf_mat <- reshape2::melt(pars$Linf)  
names(linf_mat) <- c("iteration", "live_dead", "linf")  

k_mat <- reshape2::melt(pars$K)  
names(k_mat) <- c("iteration", "live_dead", "k")  

t0_mat <- reshape2::melt(pars$t0)  
names(t0_mat) <- c("iteration", "live_dead", "t0")  

ests <- data.frame(linf_mat, k_mat$k, t0_mat$t0)
names(ests)[3:5] <- c("linf", "k", "t0")

ests <- ests %>% 
  filter(live_dead != 3)

# . Summarize parameter estimates ----
par_ests <- tidyr::pivot_longer(
  data = ests,
  cols = 3:5,
  names_to = "parameter",
  values_to = "estimate"
)

par_summary <- par_ests %>% 
  group_by(live_dead, parameter) %>% 
  summarize(fit = mean(estimate),
            lwr = quantile(estimate, 0.025),
            upr = quantile(estimate, 0.975)
  ) %>% 
  data.frame()


# Make some violin plot
ggplot(par_ests, aes(x=factor(live_dead), y=estimate), 
       color=factor(sex), fill=factor(live_dead))+
  geom_violin(alpha=.10)+
  facet_wrap(~parameter, scales = "free")

#Make a scatter plot
ggplot(par_summary,aes(x=factor(live_dead), y=fit), 
       color=factor(sex), fill=factor(sex))+
  geom_point()+
  geom_segment(aes(xend=factor(live_dead), y=lwr, yend=upr))+
  facet_wrap(~parameter, scales = "free")


# . Check model fit against observations ----

Age <-  seq(0, max(fish$final_age))

vb_pred <- function(age, linf, k, t0) {
  length <- linf * (1 - exp(-k*(age - t0)))
  return(length)
}
out_list <- vector(mode = "list", length = length(Age))
for(i in 1:length(unique(Age))){
  out_list[[i]] <- ests %>% 
    group_by(iteration, live_dead) %>% 
    summarise(pred = vb_pred(Age[i], linf, k, t0))
  out_list[[i]]$Age = Age[i]
}

preds <- do.call(rbind, out_list)

#Summarize preds

fish_preds <- preds %>% 
  group_by(live_dead, Age) %>% 
  summarise(fit=mean(pred),
            lwr=quantile(pred, 0.025),
            upr=quantile(pred, 0.975))

fish <- fish %>% 
  filter(live_dead != 3)

# Plot predictions
ggplot(fish_preds, aes(x = Age, y = fit)) +
  geom_line(aes(y = fit)) +
  facet_wrap(~live_dead) +
  geom_ribbon(aes(xmax=Age, ymin=lwr, ymax=upr), alpha =.1) +
  geom_point(data = fish, aes(x=final_age, y = fork), alpha = 0.05) +
  theme_bw() +
  theme(
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 3)
  ) +
  scale_y_continuous(limits = c(-1, 600)) #+
  # scale_x_continuous(limits = c(1, 10))
  