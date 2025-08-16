# Updates to repeat spawning probability analysis TAFS-2025-0055-R1 ----
# Libraries ----
library(tidyverse)
library(AICcmodavg)
library(car)   
library(boot)
library(RColorBrewer)

# Data Read ----
shadmaster <- read.csv("data/SHADMASTER.csv", stringsAsFactors = FALSE)

# Data Manipulation ----
delawareshad <- shadmaster %>% 
  filter(shadmaster$river == "Delaware River",
         shadmaster$sex != "unknown",
         !is.na(final_age) & final_age > 1) %>% 
  mutate(sex = str_to_title(sex),
         live_dead = str_to_title(live_dead),
         rps = as.numeric(as.factor(rps))-1)

# Add day of year (ordinal date)
delawareshad$date <- as.Date(delawareshad$date, format = "%m/%d/%y")
delawareshad$doy <- lubridate::yday(delawareshad$date)

# Summarize to get sex ratios?
delawareshad <- delawareshad %>% 
  group_by(r_km, r_mile, live_dead, doy, year, sex) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = sex, values_from = n)

delawareshad$Female[is.na(delawareshad$Female)] <- 0
delawareshad$Male[is.na(delawareshad$Male)] <- 0

delawareshad$total = delawareshad$Female + delawareshad$Male

# Model selection ----
# Create a list with models in it. We quote the names of the models here because
# they are going to be the names of the list element
model_list <- list(
  # Original model set
  'null_mod' = glm(cbind(Female, Male) ~ 1, data = delawareshad, family = binomial),
  'disposition_mod' = glm(cbind(Female, Male) ~ live_dead, data = delawareshad, family = binomial),
  'rmile_mod' = glm(cbind(Female, Male) ~ r_mile, data = delawareshad, family = binomial),
  'disposition_rmile_mod' = glm(cbind(Female, Male) ~ live_dead + r_mile, data = delawareshad, family = binomial),
  'disposition_int_rmile_mod' = glm(cbind(Female, Male) ~ live_dead * r_mile, data = delawareshad, family = binomial),
  # New models for TAFS-2025-0055-R1
  'doy_mod' = glm(cbind(Female, Male) ~ doy, data = delawareshad, family = binomial),
  'doy_disposition_mod' = glm(cbind(Female, Male) ~ doy + live_dead, data = delawareshad, family = binomial),
  'doy_rmile_mod' = glm(cbind(Female, Male) ~ doy + r_mile, data = delawareshad, family = binomial),
  'doy_disposition_rmile_mod' = glm(cbind(Female, Male) ~ doy + live_dead + r_mile, data = delawareshad, family = binomial),
  'doy_disposition_int_rmile_mod' = glm(cbind(Female, Male) ~ doy + live_dead * r_mile, data = delawareshad, family = binomial)
  )

# We will assign the model names from our model list above to a new character
# vector that we can use for model selection in the aictab() function
model_names <- names(model_list)

# Do model selection on all the models in our list using the names we made
aictab(model_list, model_names)

# Model analysis (new best model for TAFS-2025-0055-R1) ----
# . Fit the best model ----
sexratio_mod <- model_list$disposition_rmile_mod

# . Statistical significance (residual checks in original code) ----
Anova(sexratio_mod, type = "III") 
summary(sexratio_mod) 

# . Model predictions ----
# Make predictions on the link scale
logit_preds <- data.frame(predict(
  sexratio_mod, 
  type = "link",
  se.fit = TRUE))

logit_preds$lwr <- logit_preds$fit + qnorm(0.025)*logit_preds$se.fit 
logit_preds$upr <- logit_preds$fit + qnorm(0.975)*logit_preds$se.fit 

# Back-transform to probability scale
real_preds <- apply(logit_preds, 2, boot::inv.logit)

# Combine with original data and summarize for plotting
sexratio_preds <- data.frame(delawareshad, real_preds)

# . Plotting ----
myPalette <- colorRampPalette((brewer.pal(10, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(10), limits=c(0, 1))

sexratio_plot <- ggplot(sexratio_preds, 
                        aes(x = r_km, 
                            y = Female/total, 
                            group = live_dead,
                            size = total)) +
  geom_jitter(width = 0.05, height = 0.02, alpha = 0.5) +
  geom_line(aes(y = fit), size = .5) +
  geom_ribbon(aes(xmax = r_km, ymin = lwr, ymax = upr), 
              size = NULL, color = NA, alpha = 0.2) +
  facet_wrap(~live_dead, axes = "all") +
  ylab("Proportion female") +
  xlab("River kilometer") +
  labs(size = "Sample size") +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "top",
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3),
        text = element_text(size = 14, color = "black"))

sexratio_plot
