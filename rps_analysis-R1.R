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

# Model selection ----
# Create a list with models in it. We quote the names of the models here because
# they are going to be the names of the list element
model_list <- list(
  # Original model set
  'null_mod' = glm(rps ~ 1, data = delawareshad, family = binomial),
  'sex_mod' = glm(rps ~ sex, data = delawareshad, family = binomial),
  'disposition_mod' = glm(rps ~ live_dead, data = delawareshad, family = binomial),
  'rmile_mod' = glm(rps ~ r_mile, data = delawareshad, family = binomial),
  'sex_disposition_mod' = glm(rps ~ sex + live_dead, data = delawareshad, family = binomial),
  'sex_rmile_mod' = glm(rps ~ sex + r_mile, data = delawareshad, family = binomial),
  'sex_int_rmile_mod' = glm(rps ~ sex * r_mile, data = delawareshad, family = binomial),
  'sex_disposition_rmile_mod' = glm(rps ~ sex + live_dead + r_mile, data = delawareshad, family = binomial),
  'disposition_rmile_mod' = glm(rps ~ live_dead + r_mile, data = delawareshad, family = binomial),
  'sex_int_disposition_mod' = glm(rps ~ sex * live_dead, data = delawareshad, family = binomial),
  'sex_int_disposition_rmile_mod' = glm(rps ~ sex * live_dead + r_mile, data = delawareshad, family = binomial),
  'sex_int_disposition_int_rmile_mod' = glm(rps ~ sex * live_dead * r_mile, data = delawareshad, family = binomial),
  'sex_disposition_int_rmile_mod' = glm(rps ~ sex + live_dead * r_mile, data = delawareshad, family = binomial),
  'sex_int_rmile_disposition_mod' = glm(rps ~ sex + live_dead + r_mile + sex:r_mile, data = delawareshad, family = binomial),
  'disposition_int_rmile_mod' = glm(rps ~ live_dead * r_mile, data = delawareshad, family = binomial),
  # New models for TAFS-2025-0055-R1
  'doy_mod' = glm(rps ~ doy + doy, data = delawareshad, family = binomial),
  'doy_sex_mod' = glm(rps ~ doy + doy + sex, data = delawareshad, family = binomial),
  'doy_disposition_mod' = glm(rps ~ doy + live_dead, data = delawareshad, family = binomial),
  'doy_rmile_mod' = glm(rps ~ doy + r_mile, data = delawareshad, family = binomial),
  'doy_sex_disposition_mod' = glm(rps ~ doy + sex + live_dead, data = delawareshad, family = binomial),
  'doy_sex_rmile_mod' = glm(rps ~ doy + sex + r_mile, data = delawareshad, family = binomial),
  'doy_sex_int_rmile_mod' = glm(rps ~ doy + sex * r_mile, data = delawareshad, family = binomial),
  'doy_sex_disposition_rmile_mod' = glm(rps ~ doy + sex + live_dead + r_mile, data = delawareshad, family = binomial),
  'doy_disposition_rmile_mod' = glm(rps ~ doy + live_dead + r_mile, data = delawareshad, family = binomial),
  'doy_sex_int_disposition_mod' = glm(rps ~ doy + sex * live_dead, data = delawareshad, family = binomial),
  'doy_sex_int_disposition_rmile_mod' = glm(rps ~ doy + sex * live_dead + r_mile, data = delawareshad, family = binomial),
  'doy_sex_int_disposition_int_rmile_mod' = glm(rps ~ doy + sex * live_dead * r_mile, data = delawareshad, family = binomial),
  'doy_int_sex_int_disposition_int_rmile_mod' = glm(rps ~ doy * sex * live_dead * r_mile, data = delawareshad, family = binomial),
  'doy_sex_disposition_int_rmile_mod' = glm(rps ~ doy + sex + live_dead * r_mile, data = delawareshad, family = binomial),
  'doy_sex_int_rmile_disposition_mod' = glm(rps ~ doy + sex + live_dead + r_mile + sex:r_mile, data = delawareshad, family = binomial),
  'doy_disposition_int_rmile_mod' = glm(rps ~ doy + live_dead * r_mile, data = delawareshad, family = binomial)
  )

# We will assign the model names from our model list above to a new character
# vector that we can use for model selection in the aictab() function
model_names <- names(model_list)

# Do model selection on all the models in our list using the names we made
aictab(model_list, model_names)

# Table S1 for Sample sizes, means, etc ----
delawareshad$rpsm2 <- delawareshad$rpsm
delawareshad$rpsm2[delawareshad$rpsm == 0] <- NA

output <- delawareshad %>% 
  group_by(sex, live_dead, r_km) %>% 
  summarize(Age = round(mean(final_age, na.rm = TRUE), 1),
            FL = round(mean(fork, na.rm = TRUE), 0),
            rps = round(mean(rps, na.rm = TRUE), 1),
            rpsm = round(mean(rpsm, na.rm = TRUE), 1),
            n = n())

write.table(output, "results/TableS1.csv")



# Model analysis (new best model for TAFS-2025-0055-R1) ----
# . Fit the best model ----
rps_mod <- model_list$doy_int_sex_int_disposition_int_rmile_mod

# . Statistical significance (residual checks in original code) ----
Anova(rps_mod, type = "III") 
summary(rps_mod) 

# . Model predictions ----
# Make predictions on the link scale
logit_preds <- data.frame(predict(
  rps_mod, 
  type = "link",
  se.fit = TRUE))

# Back-transform to probability scale
real_preds <- apply(logit_preds, 2, boot::inv.logit)

# Combine with original data and summarize for plotting
rps_preds <- data.frame(delawareshad, real_preds) %>% 
  group_by(live_dead, sex, doy, r_km) %>%
  summarize(fit = mean(fit),
            n = n())

# . Plotting ----
myPalette <- colorRampPalette((brewer.pal(10, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(10), limits=c(0, 1))

rps_plot <- ggplot(rps_preds, aes(x = doy, y = r_km, 
                       color = fit, size = n)) +
  geom_jitter(width = 2, alpha = 0.75) +
  facet_grid(live_dead~sex, axes = "all") +
  labs(color = "Repeat spawning probability", 
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

rps_plot
