# Front-end needs ----
# . Load necessary packages ----
library(rstan)
library(tidyverse)
library(tidyr)
library(reshape2)
library(loo)

# . Set options ----
options(mc.cores = parallel::detectCores(4))  
rstan_options(auto_write = TRUE)

# . Data Read ----
fish <- read.csv("data/SHADMASTER_subsampjuv.csv", stringsAsFactors = FALSE)

# . Data manipulation ----
fish <- fish %>% 
  filter(!is.na(final_age) & !is.na(fork) & fork > 100 & !is.na(cohort) 
         & sex != "unknown" & final_age > 1 & rps != "unknown")
# Run model ----
# Package the data for stan
vb_data = list(
  length = fish$fork,
  age = fish$final_age,
  nobs = nrow(fish),
  group = as.numeric(as.factor(fish$rps)),
  ngroups = length(unique(fish$rps)),
  hp_tau = 2.5,
  hp_sigma = 50,
  hp_omega = 4,
  p_b = 0,
  p_b_sd = 1,
  nu_shape = 6,
  nu_scale = 0.1  
)


# Fit the model with stan
rps_fit <- stan(file = 'models/vonbert_group.stan',
                      data = vb_data,
                      chains = 3,
                      iter = 10000,
                      warmup = 9000,
                      control = list(
                        adapt_delta = .99,
                        max_treedepth = 15
                      ),
                      refresh = 10
)

rps_loo <- loo(rps_fit)
rps_loo

# Print model summary
print(rps_fit, digits=3)

# Save result to a file
save(rps_fit, file='results/vonbert_rps.rda')

# Results ----
# . Load result ----
load("results/vonbert_rps.rda")

# . Get parameter estimates ----
pars <- rstan::extract(rps_fit)  

linf_mat <- reshape2::melt(pars$Linf)  
names(linf_mat) <- c("iteration", "rps", "linf")  

k_mat <- reshape2::melt(pars$K)  
names(k_mat) <- c("iteration", "rps", "k")  

t0_mat <- reshape2::melt(pars$t0)  
names(t0_mat) <- c("iteration", "rps", "t0")  

ests <- data.frame(linf_mat, k_mat$k, t0_mat$t0)
names(ests)[3:5] <- c("linf", "k", "t0")

ests <- ests %>% 
  filter(rps != 3)

# . Summarize parameter estimates ----
par_ests <- tidyr::pivot_longer(
  data = ests,
  cols = 3:5,
  names_to = "parameter",
  values_to = "estimate"
)

par_summary <- par_ests %>% 
  group_by(rps, parameter) %>% 
  summarize(fit = mean(estimate),
            lwr = quantile(estimate, 0.025),
            upr = quantile(estimate, 0.975)
  ) %>% 
  data.frame()


# Make some violin plots
ggplot(par_ests, aes(x=factor(rps), y=estimate), 
       color=factor(sex), fill=factor(rps))+
  geom_violin(alpha=.10)+
  facet_wrap(~parameter, scales = "free")

#Make a scatter plot
ggplot(par_summary,aes(x=factor(rps), y=fit), 
       color=factor(sex), fill=factor(sex))+
  geom_point()+
  geom_segment(aes(xend=factor(rps), y=lwr, yend=upr))+
  facet_wrap(~parameter, scales = "free")


# . Check model fit against observations ----

Age <-  seq(0, max(fish$final_age), 0.1)

vb_pred <- function(age, linf, k, t0) {
  length <- linf * (1 - exp(-k*(age - t0)))
  return(length)
}
out_list <- vector(mode = "list", length = length(Age))
for(i in 1:length(unique(Age))){
  out_list[[i]] <- ests %>% 
    group_by(iteration, rps) %>% 
    summarise(pred = vb_pred(Age[i], linf, k, t0), .groups = "keep")
  out_list[[i]]$Age = Age[i]
}

preds <- do.call(rbind, out_list)

#Summarize preds

fish_preds <- preds %>% 
  group_by(rps, Age) %>% 
  summarise(fit=mean(pred),
            lwr=quantile(pred, 0.025),
            upr=quantile(pred, 0.975))

fish_preds$lwr[fish_preds$lwr <0] <- 0

fish$rps <- as.numeric(as.factor(fish$rps))

fish <- fish %>% 
  filter(rps != 3)

rps_labels <- c(
  "1" = "Yes", 
  "2" = "No")

# Plot predictions
  
ggplot(fish_preds, aes(x = Age, y = fit)) +
  geom_line(aes(y = fit)) +
  facet_wrap(~rps, labeller = labeller(rps = rps_labels)) +
  geom_ribbon(aes(xmax=Age, ymin=lwr, ymax=upr), alpha =.1) +
  geom_point(data = fish, aes(x=final_age, y = fork), alpha = 0.05) +
  theme_bw() +
  theme(
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 3),
    strip.background = element_blank()) +
  scale_y_continuous(limits = c(-1, 600)) +
  scale_x_continuous(limits = c(1, 10))+
  ylab("Length (mm)")
