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
         !is.na(fork) & fork > 100) %>% 
  mutate(sex = str_to_title(sex))

#### Model selection ####
# Create a list with models in it. We quote the names of the models here because
# they are going to be the names of the list element
model_list <- list(
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
  'disposition_int_rmile_mod' = lm(fork ~ live_dead * r_mile, data = delawareshad)
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
# ggplot(delawareshad, aes(residuals)) +
#   geom_histogram()

# Plot the residuals against r_mile by sex and disposition to triple check
# Also looks good!
# ggplot(delawareshad, aes(x = r_mile, y = residuals, color = sex, fill = sex)) +
#   geom_jitter(alpha = 0.5) +
#   geom_hline(yintercept = 0) +
#   facet_wrap(~live_dead)

#### Statistical significance ####
summary(model_list$sex_int_rmile_mod)
Anova(model_list$sex_int_rmile_mod, type = "III")

#### Predictions ####
# Calculate means and 95% confidence intervals for observed data
observed_means <- delawareshad %>% 
  group_by(sex, r_mile) %>% 
  summarize(obs_mean = mean(fork),
            obs_lwr = quantile(fork, 0.025),
            obs_upr = quantile(fork, 0.975),
            sample_size = n())

# Make predictions from the model with confidence intervals
model_predictions <- predict(model_list$sex_int_rmile_mod, 
                             newdata = observed_means,
                             interval = "confidence")

# Combine the observed means and CIs with the predictions
length_preds <- data.frame(observed_means, model_predictions)

# Plot the predictions against the observed means and CIs
ggplot(length_preds, aes(x = r_mile, y = obs_mean)) +
  geom_point(position = position_dodge(width = 3), size = 3) +
  geom_errorbar(aes(x = r_mile, ymin = obs_lwr, ymax = obs_upr), width = 0,
                position = position_dodge(width = 3), lwd = 1)+
  geom_line(aes(y = fit), position = position_dodge(width = 3)) +
  geom_ribbon(aes(xmax = r_mile, ymin = lwr, ymax = upr, color = NULL), 
              alpha = 0.10, position = position_dodge(width = 3)) +
  xlab("River mile") +
  ylab("Fork length (mm)") +
  facet_wrap(~sex) +
  theme(
    strip.background = element_blank(),
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 3)
  )
