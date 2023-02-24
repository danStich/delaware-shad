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
fish$site <- "Middle"
fish$site[fish$location %in% c("Upper Hancock", "Lordville", "Long Eddy")] <- "Upper"
fish$site[fish$location %in% c("Milford", "Smithfield Beach")] <- "Lower"

fish$r_mile_s <- as.vector(scale(fish$r_mile))

# Specify log-scale global priors
prior1 <- prior(normal(0, 1), nlpar = "linf") +
  prior(normal(0, 1), nlpar = "k") +
  prior(normal(0, 1), nlpar = "t0")

# Compile the model. We will fit it below
bests_mod <- brm(bf(fork ~ exp(linf) * (1 - exp(-exp(k)*(final_age-t0))),
                   linf ~ 1 + r_mile_s + sex,
                   k ~ 1 + r_mile_s + sex,
                   t0 ~ 1 + r_mile_s + sex,
                   nl = TRUE),
                data = fish,
                prior = prior1,
                iter = 1,
                chains = 3,
                cores = 3)

# Now fit the model. Re-fit with increased number of samples or other
# stan settings as needed
bests_fit <- update(bests_mod, iter = 3000, chains = 3, cores = 3)

# Print a summary
summary(bests_fit)

# Save the result
save(bests_fit, file = "results/bests_fit.rda")



