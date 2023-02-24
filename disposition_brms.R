# Front-end needs ----
# . Load necessary packages ----
library(brms)
library(tidyverse)
library(tidyr)
library(reshape2)

# . Data Read ----
fish <- read.csv("data/SHADMASTER.csv", stringsAsFactors = FALSE)

# . Data manipulation ----
fish <- fish %>% 
  filter(!is.na(final_age) & !is.na(fork) & !is.na(rps) & !is.na(cohort)
         & sex != "unknown")

plot(fish$final_age, fish$fork)

fish <- fish %>% 
  filter(!(final_age == 1 & fork > 200) &
         !(final_age == 5 & fork < 200))

plot(fish$final_age, fish$fork)

# Run model ----
# Specify log-scale global priors
prior1 <- prior(normal(0, 1), nlpar = "linf") +
  prior(normal(0, 1), nlpar = "k") +
  prior(normal(0, 1), nlpar = "t0")

# Compile the model. We will fit it below
live_dead_mod <- brm(bf(fork ~ exp(linf) * (1 - exp(-exp(k)*(final_age-t0))),
                   linf ~ 1 + live_dead,
                   k ~ 1 + live_dead,
                   t0 ~ 1 + live_dead,
                   nl = TRUE),
                data = fish,
                prior = prior1,
                iter = 1,
                chains = 3,
                cores = 3)

# Now fit the model. Re-fit with increased number of samples or other
# stan settings as needed
live_dead_fit <- update(live_dead_mod, iter = 3000, chains = 3, cores = 3)

# Print a summary
summary(live_dead_fit)

# Save the result
save(live_dead_fit, file = "results/live_dead_fit.rda")



