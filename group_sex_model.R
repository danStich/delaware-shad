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
shadmaster <- read.csv("data/SHADMASTER.csv", stringsAsFactors = FALSE)

# Data Manipulation
fish <- shadmaster %>% 
  filter(!is.na(final_age) & 
           !is.na(fork) & 
           !is.na(sex) & fork < 900)

fish <- fish %>% 
  filter(!(fish$final_age == 5 & fish$fork < 50))

#### Run model location/age based model ####
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
sex_fit <- stan(file = 'models/vonbert_group.stan',
            data = vb_data,
            chains = 3,
            iter = 500,
            warmup = 400,
            control = list(
              adapt_delta = 0.80,
              max_treedepth = 10
            )
)

# Print model summary
print(sex_fit, digits=3)

# Save result to a file
save(sex_fit, file='vonbert_sex_group.rda')

# - Results ----
# . Load result ----
# load("vonbert_sex_group.rda")

# . Get parameter estimates ----
pars <- rstan::extract(sex_fit)  

linf_mat <- reshape2::melt(pars$Linf)  
names(linf_mat) <- c("iteration", "sex", "linf")  

k_mat <- reshape2::melt(pars$K)  
names(k_mat) <- c("iteration", "sex", "k")  

t0_mat <- reshape2::melt(pars$t0)  
names(t0_mat) <- c("iteration", "sex", "t0")  

ests <- data.frame(linf_mat, k_mat$k, t0_mat$t0)
names(ests)[3:5] <- c("linf", "k", "t0")

# . Summarize parameter estimates ----
par_ests <- tidyr::pivot_longer(
  data = ests,
  cols = 3:5,
  names_to = "parameter",
  values_to = "estimate"
)

par_summary <- par_ests %>% 
  group_by(sex, parameter) %>% 
  summarize(fit = mean(estimate),
            lwr = quantile(estimate, 0.025),
            upr = quantile(estimate, 0.975)
  ) %>% 
  data.frame()


# Make some violin plot
ggplot(par_ests, aes(x=factor(sex), y=estimate), color=factor(sex), fill=factor(sex))+
  geom_violin(alpha=.10)+
  facet_wrap(~parameter, scales = "free")

#Make a scatter plot
ggplot(par_summary,aes(x=factor(sex), y=fit), color=factor(sex), fill=factor(sex))+
  geom_point()+
  geom_segment(aes(xend=factor(sex), y=lwr, yend=upr))+
  facet_wrap(~parameter, scales = "free")

# This is from the first time I ran it. Will change a little each time
#   parameter         fit         lwr         upr
# 1         k   0.2375733   0.1564696   0.3317956
# 2      linf 402.1970692 298.8405118 543.8759847
# 3        t0  -0.1433281  -0.6718556   0.2140471

# . Check model fit against observations ----

Age <-  seq(0, max(fish$final_age))

vb_pred <- function(age, linf, k, t0) {
  length <- linf * (1 - exp(-k*(age - t0)))
  return(length)
}
out_list <- vector(mode = "list", length = length(Age))
for(i in 1:length(unique(Age))){
  out_list[[i]] <- ests %>% 
    group_by(iteration, sex) %>% 
    summarise(pred = vb_pred(Age[i], linf, k, t0))
  out_list[[i]]$Age = Age[i]
}

preds <- do.call(rbind, out_list)

#Summarize preds

fish_preds <- preds %>% 
  group_by(sex, Age) %>% 
  summarise(fit=mean(pred),
            lwr=quantile(pred, 0.025),
            upr=quantile(pred, 0.975))

fish$sex <- as.numeric(as.factor(fish$sex))

# Plot predictions
ggplot(fish_preds, aes(x = Age, y = fit)) +
  geom_line(aes(y = fit)) +
  facet_wrap(~sex) +
  geom_ribbon(aes(xmax=Age, ymin=lwr, ymax=upr), alpha =.1) +
  geom_point(data = fish, aes(x=final_age, y = fork)) +
  theme_bw() +
  theme(
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 3)
  ) +
  scale_y_continuous(limits = c(0, 600)) #+
  # scale_x_continuous(limits = c(1, 10))
  



