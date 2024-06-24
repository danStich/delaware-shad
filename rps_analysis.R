#Shad Workbook
#### Loading data and tidyverse ####
library(tidyverse)
library(AICcmodavg)
library(car)   
library(boot)

#### Data Read ####
shadmaster <- read.csv("data/SHADMASTER.csv", stringsAsFactors = FALSE)

#### Data Manipulation ####
delawareshad <- shadmaster %>% 
  filter(shadmaster$river == "Delaware River",
         shadmaster$sex != "unknown",
         !is.na(final_age) & final_age > 1) %>% 
  mutate(sex = str_to_title(sex),
         live_dead = str_to_title(live_dead),
         rps = as.numeric(as.factor(rps))-1)

#### Model selection ####
# Create a list with models in it. We quote the names of the models here because
# they are going to be the names of the list element
model_list <- list(
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
  'disposition_int_rmile_mod' = glm(rps ~ live_dead * r_mile, data = delawareshad, family = binomial)
)

# We will assign the model names from our model list above to a new character
# vector that we can use for model selection in the aictab() function
model_names <- names(model_list)

# Do model selection on all the models in our list using the names we made
aictab(model_list, model_names)

#### Residual diagnostics ####
# Get the residuals from the most complex model
delawareshad$residuals <- model_list$sex_int_disposition_int_rmile_mod$residuals

# Check the mean: it is about zero
mean(delawareshad$residuals)

# Check the overall distribution: looks pretty symmetrical
ggplot(delawareshad, aes(residuals)) +
  geom_histogram()


#### Statistical significance ####
summary(model_list$sex_disposition_int_rmile_mod)
Anova(model_list$sex_disposition_int_rmile_mod, type = "III")

#### Predictions ####
# Calculate means and 95% confidence intervals for observed data
observed_means <- delawareshad %>% 
  group_by(sex, r_mile, r_km, live_dead) %>% 
  summarize(obs_mean = mean(rps),
            obs_lwr = mean(rps) - sd(rps),
            obs_upr = mean(rps) + sd(rps),
            sample_size = n())

# Make predictions from the model with confidence intervals
# on the logit scale
logit_predictions <- data.frame(
  predict(model_list$sex_disposition_int_rmile_mod, 
  newdata = observed_means,
  se.fit = TRUE))
logit_predictions$lwr <- logit_predictions$fit - logit_predictions$se.fit*1.96
logit_predictions$upr <- logit_predictions$fit + logit_predictions$se.fit*1.96

# Convert predictions to the real scale
real_predictions <- apply(logit_predictions, 2, inv.logit) 

# Combine the observed means and CIs with the predictions
rps_preds <- data.frame(observed_means, real_predictions)

# Figure 4 ----
# Plot the predictions against the observed means and CIs
rps_plot <- ggplot(rps_preds, aes(x = r_km, y = obs_mean)) +
  geom_point(position = position_dodge(width = 3), size = 3) +
  # geom_errorbar(aes(x = r_km, ymin = obs_lwr, ymax = obs_upr), width = 0,
  #               position = position_dodge(width = 3), lwd = 1) +
  geom_line(aes(y = fit), position = position_dodge(width = 3)) +
  geom_ribbon(aes(xmax = r_km, ymin = lwr, ymax = upr, color = NULL), 
              alpha = 0.10, position = position_dodge(width = 3)) +
  xlab("River kilometer") +
  ylab("Proportion of repeat spawners") +
  facet_grid(live_dead~sex) +
  theme(
    strip.background = element_blank(),
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 3),
    text = element_text(size = 14, color = "black")
  )

# For viewing in Rstudio
rps_plot

# Figure for manuscript
jpeg(filename = "results/Figure4.jpg",
     res = 300,
     width = 2400, 
     height = 1600)
rps_plot
dev.off()

#### Additional summary stats ####
# Need some additional summary stats for live fish since they 
# didn't really change
newd <- data.frame(
  sex = c("Male", "Female"),
  live_dead = "Live",
  r_mile = mean(unique(delawareshad$r_mile))
  )

lsex_preds <- data.frame(predict(model_list$sex_disposition_int_rmile_mod, 
        newdata = newd, se.fit = TRUE))
lsex_preds$lwr <- lsex_preds$fit - lsex_preds$se.fit*1.96
lsex_preds$upr <- lsex_preds$fit + lsex_preds$se.fit*1.96

rsex_preds <- apply(lsex_preds, 2, inv.logit)

sex_preds_live <- data.frame(newd, rsex_preds)
sex_preds_live

