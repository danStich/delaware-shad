# Updated size structure analysis for TAFS-2025-0055-R1 ----
# Libraries ----
library(tidyverse)
library(AICcmodavg)
library(car)     
library(RColorBrewer)

# Data Read ----
shadmaster <- read.csv("data/SHADMASTER.csv", stringsAsFactors = FALSE)

# Data Manipulation ----
shadmaster$gear <- factor(shadmaster$gear,
                          levels = c(1, 2, 3, 4, 5),
                          labels = c("Gill net", "Electrofishing",
                                     "Angling", "Scavenge", "YOY seine"))

delawareshad <- shadmaster %>% 
  filter(shadmaster$river == "Delaware River",
         shadmaster$sex != "unknown",
         !is.na(fork) & fork > 100) %>% 
         # live_dead == "live") %>% 
  mutate(sex = str_to_title(sex))

# Add day of year (ordinal date)
delawareshad$date <- as.Date(delawareshad$date, format = "%m/%d/%y")
delawareshad$doy <- lubridate::yday(delawareshad$date)


# Test to see if Smithfield influences result 
# delawareshad <- shadmaster %>% 
#   filter(shadmaster$river == "Delaware River",
#          shadmaster$sex != "unknown",
#          !is.na(fork) & fork > 100,
#          # live_dead == "live",
#          year == 2021,
#          r_km > 351) %>% 
#   mutate(sex = str_to_title(sex))


# Model selection ----
# Create a list with models in it. We quote the names of the models here because
# they are going to be the names of the list element
# Replace r_mile with gear to test whether gear effect is stronger
model_list <- list(
  # Original model set
  'null_mod' = lm(fork ~ 1, data = delawareshad),
  'sex_mod' = lm(fork ~ sex, data = delawareshad),
  'disposition_mod' = lm(fork ~ live_dead, data = delawareshad),
  'rmile_mod' = lm(fork ~ r_mile, data = delawareshad),
  'sex_disposition_mod' = lm(fork ~ sex + live_dead, data = delawareshad),
  'sex_rmile_mod' = lm(fork ~ sex + r_mile, data = delawareshad),
  'sex_int_rmile_mod' = lm(fork ~ sex * r_mile, data = delawareshad),
  'sex_disposition_rmile_mod' = lm(fork ~ sex + live_dead + r_mile, data = delawareshad),
  'disposition_rmile_mod' = lm(fork ~ live_dead + r_mile, data = delawareshad),
  'sex_int_disposition_mod' = lm(fork ~ sex * live_dead, data = delawareshad),
  'sex_int_disposition_rmile_mod' = lm(fork ~ sex * live_dead + r_mile, data = delawareshad),
  'sex_int_disposition_int_rmile_mod' = lm(fork ~ sex * live_dead * r_mile, data = delawareshad),
  'sex_disposition_int_rmile_mod' = lm(fork ~ sex + live_dead * r_mile, data = delawareshad),
  'sex_int_rmile_disposition_mod' = lm(fork ~ sex + live_dead + r_mile + sex:r_mile, data = delawareshad),
  'disposition_int_rmile_mod' = lm(fork ~ live_dead * r_mile, data = delawareshad),
  # New models for TAFS-2025-055
  'doy_sex_mod' = lm(fork ~ doy + sex, data = delawareshad),
  'doy_disposition_mod' = lm(fork ~ doy + live_dead, data = delawareshad),
  'doy_rmile_mod' = lm(fork ~ doy + r_mile, data = delawareshad),
  'doy_sex_disposition_mod' = lm(fork ~ doy + sex + live_dead, data = delawareshad),
  'doy_sex_rmile_mod' = lm(fork ~ doy + sex + r_mile, data = delawareshad),
  'doy_sex_int_rmile_mod' = lm(fork ~ doy + sex * r_mile, data = delawareshad),
  'doy_sex_disposition_rmile_mod' = lm(fork ~ doy + sex + live_dead + r_mile, data = delawareshad),
  'doy_disposition_rmile_mod' = lm(fork ~ doy + live_dead + r_mile, data = delawareshad),
  'doy_sex_int_disposition_mod' = lm(fork ~ doy + sex * live_dead, data = delawareshad),
  'doy_sex_int_disposition_rmile_mod' = lm(fork ~ doy + sex * live_dead + r_mile, data = delawareshad),
  'doy_sex_int_disposition_int_rmile_mod' = lm(fork ~ doy + sex * live_dead * r_mile, data = delawareshad),
  'doy_sex_int_disposition_int_rmile_mod' = lm(fork ~ doy + sex * live_dead * r_mile, data = delawareshad),
  'doy_int_sex_disposition_int_rmile_mod' = lm(fork ~ doy * sex + live_dead * r_mile, data = delawareshad),
  'doy_sex_int_rmile_disposition_mod' = lm(fork ~ doy + sex + live_dead + r_mile + sex:r_mile, data = delawareshad),
  'doy_disposition_int_rmile_mod' = lm(fork ~ doy + live_dead * r_mile, data = delawareshad)#,
  )

# . Check for confounding with gear size-selectivity ----
# Uncomment the code below (lines 80-96) to run gear check
# since best submodel did not include gear, we excluded this from
# the analysis above
# delawareshad <- shadmaster %>%
#   filter(shadmaster$river == "Delaware River",
#          shadmaster$sex != "unknown",
#          !is.na(fork) & fork > 100,
#          live_dead == "live") %>%
#   mutate(sex = str_to_title(sex))
# 
# model_list <- list(
#   'null_mod' = lm(fork ~ 1, data = delawareshad),
#   'sex_mod' = lm(fork ~ sex, data = delawareshad),
#   'rmile_mod' = lm(fork ~ r_mile, data = delawareshad),
#   'sex_rmile_mod' = lm(fork ~ sex + r_mile, data = delawareshad),
#   'sex_int_rmile_mod' = lm(fork ~ sex * r_mile, data = delawareshad),
#   'gear_mod' = lm(fork ~ gear, data = delawareshad),
#   'sex_gear_mod' = lm(fork ~ sex + gear, data = delawareshad),
#   'sex_int_gear_mod' = lm(fork ~ sex * gear, data = delawareshad)
# )

# We will assign the model names from our model list above to a new character
# vector that we can use for model selection in the aictab() function
model_names <- names(model_list)

# Do model selection on all the models in our list using the names we made
aictab(model_list, model_names)

# Model analysis (new best model for TAFS-2025-0055-R1) ----
# . Fit the best model ----
doy_sex_int_rmile_mod = lm(fork ~ doy + sex * r_mile, data = delawareshad)

# . Statistical significance (residual checks in original code) ----
Anova(doy_sex_int_rmile_mod, type = "III") 
summary(doy_sex_int_rmile_mod) 

# . Model Predictions ----
predictions <- predict(doy_sex_int_rmile_mod, 
                       interval = "prediction")

# Combine with original data and summarize for plotting
size_preds <- data.frame(delawareshad, predictions) %>% 
  group_by(sex, doy, r_km) %>%
  summarize(fit = mean(fit),
            n = n())

# . Plotting ----
myPalette <- colorRampPalette(rev(brewer.pal(10, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(10), limits=c(350, 525))

size_plot <- ggplot(size_preds, aes(x = doy, y = r_km, 
                       color = fit, size = n)) +
  geom_jitter(width = 2, height = 2) +
  facet_wrap(~sex, axes = "all") +
  labs(color = "", 
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

size_plot