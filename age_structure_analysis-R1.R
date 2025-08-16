# Updated age-structure analysis for TAFS-2025-0055-R1
# Libraries ----
library(tidyverse)
library(AICcmodavg)
library(car)     
library(RColorBrewer)

# Data Read ----
shadmaster <- read.csv("data/SHADMASTER.csv", stringsAsFactors = FALSE)

# Data Manipulation ----
delawareshad <- shadmaster %>% 
  filter(shadmaster$river == "Delaware River",
         shadmaster$sex != "unknown",
         !is.na(final_age) & final_age > 1) %>% 
  mutate(sex = str_to_title(sex))

# Add day of year (ordinal date)
delawareshad$date <- as.Date(delawareshad$date, format = "%m/%d/%y")
delawareshad$doy <- lubridate::yday(delawareshad$date)


# Model selection ----
# Create a list with models in it. We quote the names of the models here because
# they are going to be the names of the list element
model_list <- list(
  # Original model set
  'null_mod' = lm(final_age ~ 1, data = delawareshad),
  'sex_mod' = lm(final_age ~ sex, data = delawareshad),
  'disposition_mod' = lm(final_age ~ live_dead, data = delawareshad),
  'rmile_mod' = lm(final_age ~ r_mile, data = delawareshad),
  'sex_disposition_mod' = lm(final_age ~ sex + live_dead, data = delawareshad),
  'sex_rmile_mod' = lm(final_age ~ sex + r_mile, data = delawareshad),
  'sex_int_rmile_mod' = lm(final_age ~ sex * r_mile, data = delawareshad),
  'sex_disposition_rmile_mod' = lm(final_age ~ sex + live_dead + r_mile, data = delawareshad),
  'disposition_rmile_mod' = lm(final_age ~ live_dead + r_mile, data = delawareshad),
  'sex_int_disposition_mod' = lm(final_age ~ sex * live_dead, data = delawareshad),
  'sex_int_disposition_rmile_mod' = lm(final_age ~ sex * live_dead + r_mile, data = delawareshad),
  'sex_int_disposition_int_rmile_mod' = lm(final_age ~ sex * live_dead * r_mile, data = delawareshad),
  'sex_disposition_int_rmile_mod' = lm(final_age ~ sex + live_dead * r_mile, data = delawareshad),
  'sex_int_rmile_disposition_mod' = lm(final_age ~ sex + live_dead + r_mile + sex:r_mile, data = delawareshad),
  'disposition_int_rmile_mod' = lm(final_age ~ live_dead * r_mile, data = delawareshad),
  # New models for TAFS-2025-0055-R1
  'doy_mod' = lm(final_age ~ doy + 1, data = delawareshad),
  'doy_sex_mod' = lm(final_age ~ doy + sex, data = delawareshad),
  'doy_disposition_mod' = lm(final_age ~ doy + live_dead, data = delawareshad),
  'doy_rmile_mod' = lm(final_age ~ doy + r_mile, data = delawareshad),
  'doy_sex_disposition_mod' = lm(final_age ~ doy + sex + live_dead, data = delawareshad),
  'doy_sex_rmile_mod' = lm(final_age ~ doy + sex + r_mile, data = delawareshad),
  'doy_sex_int_rmile_mod' = lm(final_age ~ doy + sex * r_mile, data = delawareshad),
  'doy_sex_disposition_rmile_mod' = lm(final_age ~ doy + sex + live_dead + r_mile, data = delawareshad),
  'doy_disposition_rmile_mod' = lm(final_age ~ doy + live_dead + r_mile, data = delawareshad),
  'doy_sex_int_disposition_mod' = lm(final_age ~ doy + sex * live_dead, data = delawareshad),
  'doy_sex_int_disposition_rmile_mod' = lm(final_age ~ doy + sex * live_dead + r_mile, data = delawareshad),
  'doy_sex_int_disposition_int_rmile_mod' = lm(final_age ~ doy + sex * live_dead * r_mile, data = delawareshad),
  'doy_sex_int_disposition_int_rmile_mod' = lm(final_age ~ doy * sex * live_dead * r_mile, data = delawareshad),
  'doy_int_sex_disposition_int_rmile_mod' = lm(final_age ~ doy + sex + live_dead * r_mile, data = delawareshad),
  'doy_sex_int_rmile_disposition_mod' = lm(final_age ~ doy + sex + live_dead + r_mile + sex:r_mile, data = delawareshad),
  'doy_disposition_int_rmile_mod' = lm(final_age ~ doy + live_dead * r_mile, data = delawareshad)
  
  )

# We will assign the model names from our model list above to a new character
# vector that we can use for model selection in the aictab() function
model_names <- names(model_list)

# Do model selection on all the models in our list using the names we made
aictab(model_list, model_names)

# Model analysis (new best model for TAFS-2025-0055-R1) ----
# . Fit the best model ----
age_mod <- model_list$doy_sex_int_disposition_int_rmile_mod

# . Statistical significance (residual checks in old code) ----
Anova(age_mod, Type = "III")
summary(age_mod)

# . Model predictions ----
predictions <- predict(age_mod, 
                       interval = "prediction")

# Combine predictions with original data
age_preds <- data.frame(delawareshad, predictions) %>% 
  group_by(live_dead, sex, doy, r_km) %>%
  summarize(fit = mean(fit),
            n = n())

# . Plotting ----
myPalette <- colorRampPalette((brewer.pal(10, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(10), limits=c(4, 8))

age_plot <- ggplot(age_preds, aes(x = doy, y = r_km, 
                       color = fit, size = n)) +
  geom_jitter(width = 2, height = 2, alpha = .75) +
  facet_grid(live_dead~sex) +
  labs(color = "Age (years)", 
       size = "Sample size") +
  xlab("Ordinal day of year") +
  ylab("River kilometer") +
  sc +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = "top",
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3),
        text = element_text(size = 14, color = "black"))

age_plot
