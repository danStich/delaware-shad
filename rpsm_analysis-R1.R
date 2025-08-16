# Updated repeat spawning mark analysis for TAFS-2025-0055-R1 ----
# Libraries ----
library(tidyverse)
library(AICcmodavg)
library(car)   
library(boot)
library(MASS)
library(RColorBrewer)

# Data Read ----
shadmaster <- read.csv("data/SHADMASTER.csv", stringsAsFactors = FALSE)

# Data Manipulation ----
delawareshad <- shadmaster %>% 
  filter(shadmaster$river == "Delaware River",
         shadmaster$sex != "unknown",
         !is.na(final_age) & final_age > 1,
         rpsm > 0) %>% 
  mutate(sex = str_to_title(sex),
         live_dead = str_to_title(live_dead))

# Add day of year (ordinal date)
delawareshad$date <- as.Date(delawareshad$date, format = "%m/%d/%y")
delawareshad$doy <- lubridate::yday(delawareshad$date)

# Model selection ----
# Get a starting guess for theta (size) to use in negative binomial regression
fitdistr(delawareshad$rpsm, densfun = "negative binomial")
theta <- 266


# Create a list with models in it. We quote the names of the models here because
# they are going to be the names of the list element
model_list <- list(
  # Original models
  'null_mod' = glm(rpsm ~ 1, data = delawareshad, family = negative.binomial(theta = 266)),
  'sex_mod' = glm(rpsm ~ sex, data = delawareshad, family = negative.binomial(theta = 266)),
  'disposition_mod' = glm(rpsm ~ live_dead, data = delawareshad, family = negative.binomial(theta = 266)),
  'rmile_mod' = glm(rpsm ~ r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  'sex_disposition_mod' = glm(rpsm ~ sex + live_dead, data = delawareshad, family = negative.binomial(theta = 266)),
  'sex_rmile_mod' = glm(rpsm ~ sex + r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  'sex_int_rmile_mod' = glm(rpsm ~ sex * r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  'sex_disposition_rmile_mod' = glm(rpsm ~ sex + live_dead + r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  'disposition_rmile_mod' = glm(rpsm ~ live_dead + r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  'sex_int_disposition_mod' = glm(rpsm ~ sex * live_dead, data = delawareshad, family = negative.binomial(theta = 266)),
  'sex_int_disposition_rmile_mod' = glm(rpsm ~ sex * live_dead + r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  'sex_int_disposition_int_rmile_mod' = glm(rpsm ~ sex * live_dead * r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  'sex_disposition_int_rmile_mod' = glm(rpsm ~ sex + live_dead * r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  'sex_int_rmile_disposition_mod' = glm(rpsm ~ sex + live_dead + r_mile + sex:r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  'disposition_int_rmile_mod' = glm(rpsm ~ live_dead * r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  # New models for TAFS-2025-0055-R1
  'doy_mod' = glm(rpsm ~ doy, data = delawareshad, family = negative.binomial(theta = 266)),
  'doy_sex_mod' = glm(rpsm ~ doy + sex, data = delawareshad, family = negative.binomial(theta = 266)),
  'doy_disposition_mod' = glm(rpsm ~ doy + live_dead, data = delawareshad, family = negative.binomial(theta = 266)),
  'doy_rmile_mod' = glm(rpsm ~ doy + r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  'doy_sex_disposition_mod' = glm(rpsm ~ doy + sex + live_dead, data = delawareshad, family = negative.binomial(theta = 266)),
  'doy_sex_rmile_mod' = glm(rpsm ~ doy + sex + r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  'doy_sex_int_rmile_mod' = glm(rpsm ~ doy + sex * r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  'doy_sex_disposition_rmile_mod' = glm(rpsm ~ doy + sex + live_dead + r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  'doy_disposition_rmile_mod' = glm(rpsm ~ doy + live_dead + r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  'doy_sex_int_disposition_mod' = glm(rpsm ~ doy + sex * live_dead, data = delawareshad, family = negative.binomial(theta = 266)),
  'doy_sex_int_disposition_rmile_mod' = glm(rpsm ~ doy + sex * live_dead + r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  'doy_sex_int_disposition_int_rmile_mod' = glm(rpsm ~ doy + sex * live_dead * r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  'doy_int_sex_int_disposition_int_rmile_mod' = glm(rpsm ~ doy * sex * live_dead * r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  'doy_sex_disposition_int_rmile_mod' = glm(rpsm ~ doy + sex + live_dead * r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  'doy_sex_int_rmile_disposition_mod' = glm(rpsm ~ doy + sex + live_dead + r_mile + sex:r_mile, data = delawareshad, family = negative.binomial(theta = 266)),
  'doy_disposition_int_rmile_mod' = glm(rpsm ~ doy + live_dead * r_mile, data = delawareshad, family = negative.binomial(theta = 266))
  )

# We will assign the model names from our model list above to a new character
# vector that we can use for model selection in the aictab() function
model_names <- names(model_list)

# Do model selection on all the models in our list using the names we made
aictab(model_list, model_names)

# Model analysis (new best model for TAFS-2025-0055-R1) ----
# . Fit the best model ----
rpsm_mod <- model_list$doy_disposition_int_rmile_mod

# . Statistical significance (residual checks in original code) ----
Anova(rpsm_mod, type = "III")
summary(rpsm_mod)

# . Model predictions ----
# Make predictions on the link scale
log_preds <- data.frame(predict(
  rpsm_mod, 
  type = "link",
  se.fit = TRUE))

# Back-transform to the real scale (counts)
real_preds <- apply(log_preds, 2, exp)

# Combine predictions with original data and summarize
rpsm_preds <- data.frame(delawareshad, real_preds)%>% 
  group_by(live_dead, doy, r_km) %>%
  summarize(fit = mean(fit),
            n = n())

# . Plotting ----
myPalette <- colorRampPalette((brewer.pal(10, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(10), limits=c(1, 4))

rpsm_plot <- ggplot(rpsm_preds, aes(x = doy, y = r_km, 
                       color = fit, size = n)) +
  geom_jitter(width = 2, height = 2, alpha = 0.75) +
  labs(color = "Repeat spawning marks", 
       size = "Sample size") +
  xlab("Ordinal day of year") +
  ylab("River kilometer") +
  facet_wrap(~live_dead, axes = "all") +
  sc +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "top",
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3),
        text = element_text(size = 14, color = "black"))

rpsm_plot

