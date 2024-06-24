#Shad Workbook
#### Loading data and tidyverse ####
library(tidyverse)
library(AICcmodavg)
library(car)     

#### Data Read ####
shadmaster <- read.csv("data/SHADMASTER.csv", stringsAsFactors = FALSE)

#### Data Manipulation ####
delawareshad <- shadmaster %>% 
  filter(shadmaster$river == "Delaware River",
         shadmaster$sex != "unknown",
         !is.na(final_age) & final_age > 1) %>% 
  mutate(sex = str_to_title(sex))

#### Model selection ####
# Create a list with models in it. We quote the names of the models here because
# they are going to be the names of the list element
model_list <- list(
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
  'disposition_int_rmile_mod' = lm(final_age ~ live_dead * r_mile, data = delawareshad)
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

# Plot the residuals against r_mile by sex and disposition to triple check
# Also looks good!
ggplot(delawareshad, aes(x = r_mile, y = residuals, color = sex, fill = sex)) +
  geom_jitter(alpha = 0.5) +
  geom_hline(yintercept = 0) +
  facet_wrap(~live_dead)

#### Statistical significance ####
summary(model_list$sex_int_rmile_mod)
Anova(model_list$sex_int_rmile_mod, type = "III")

#### Predictions ####
# Calculate means and 95% confidence intervals for observed data
observed_means <- delawareshad %>% 
  group_by(sex, r_mile, r_km) %>% 
  summarize(obs_mean = mean(final_age),
            obs_lwr = quantile(final_age, 0.025),
            obs_upr = quantile(final_age, 0.975),
            sample_size = n())

# Make predictions from the model with confidence intervals
model_predictions <- predict(model_list$sex_int_rmile_mod, 
                             newdata = observed_means,
                             interval = "confidence")

# Combine the observed means and CIs with the predictions
age_preds <- data.frame(observed_means, model_predictions)

# Figure 3 ----
# Plot the predictions against the observed means and CIs
age_pred_plot <- ggplot(age_preds, aes(x = r_km, y = obs_mean)) +
  geom_point(position = position_dodge(width = 3), size = 3) +
  geom_errorbar(aes(x = r_km, ymin = obs_lwr, ymax = obs_upr), width = 0,
                position = position_dodge(width = 3), lwd = 1)+
  geom_line(aes(y = fit), position = position_dodge(width = 3)) +
  geom_ribbon(aes(xmax = r_km, ymin = lwr, ymax = upr, color = NULL), 
              alpha = 0.10, position = position_dodge(width = 3)) +
  xlab("River kilometer") +
  ylab("Age (years)") +
  facet_wrap(~sex) +
  theme(
    strip.background = element_blank(),
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 3),
    text = element_text(size = 14, color = "black")
  )

# For viewing in Rstudio
age_pred_plot

# Figure for manuscript
jpeg(filename = "results/Figure3.jpg",
     res = 300,
     width = 2400, 
     height = 1600)
age_pred_plot
dev.off()


