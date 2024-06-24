#Shad Workbook
#### Loading data and tidyverse ####
library(tidyverse)
library(AICcmodavg)
library(car)   
library(boot)
library(MASS)

#### Data Read ####
shadmaster <- read.csv("data/SHADMASTER.csv", stringsAsFactors = FALSE)

#### Data Manipulation ####
delawareshad <- shadmaster %>% 
  filter(shadmaster$river == "Delaware River",
         shadmaster$sex != "unknown",
         !is.na(final_age) & final_age > 1,
         rpsm > 0) %>% 
  mutate(sex = str_to_title(sex),
         live_dead = str_to_title(live_dead))

#### Model selection ####
# Get a starting guess for theta (size) to use in negative binomial regression
fitdistr(delawareshad$rpsm, densfun = "negative binomial")

# Create a list with models in it. We quote the names of the models here because
# they are going to be the names of the list element
model_list <- list(
  'null_mod' = glm(rpsm ~ 1, data = delawareshad, family = negative.binomial(theta = .01)),
  'sex_mod' = glm(rpsm ~ sex, data = delawareshad, family = negative.binomial(theta = .01)),
  'disposition_mod' = glm(rpsm ~ live_dead, data = delawareshad, family = negative.binomial(theta = .01)),
  'rmile_mod' = glm(rpsm ~ r_mile, data = delawareshad, family = negative.binomial(theta = .01)),
  'sex_disposition_mod' = glm(rpsm ~ sex + live_dead, data = delawareshad, family = negative.binomial(theta = .01)),
  'sex_rmile_mod' = glm(rpsm ~ sex + r_mile, data = delawareshad, family = negative.binomial(theta = .01)),
  'sex_int_rmile_mod' = glm(rpsm ~ sex * r_mile, data = delawareshad, family = negative.binomial(theta = .01)),
  'sex_disposition_rmile_mod' = glm(rpsm ~ sex + live_dead + r_mile, data = delawareshad, family = negative.binomial(theta = .01)),
  'disposition_rmile_mod' = glm(rpsm ~ live_dead + r_mile, data = delawareshad, family = negative.binomial(theta = .01)),
  'sex_int_disposition_mod' = glm(rpsm ~ sex * live_dead, data = delawareshad, family = negative.binomial(theta = .01)),
  'sex_int_disposition_rmile_mod' = glm(rpsm ~ sex * live_dead + r_mile, data = delawareshad, family = negative.binomial(theta = .01)),
  'sex_int_disposition_int_rmile_mod' = glm(rpsm ~ sex * live_dead * r_mile, data = delawareshad, family = negative.binomial(theta = .01)),
  'sex_disposition_int_rmile_mod' = glm(rpsm ~ sex + live_dead * r_mile, data = delawareshad, family = negative.binomial(theta = .01)),
  'sex_int_rmile_disposition_mod' = glm(rpsm ~ sex + live_dead + r_mile + sex:r_mile, data = delawareshad, family = negative.binomial(theta = .01)),
  'disposition_int_rmile_mod' = glm(rpsm ~ live_dead * r_mile, data = delawareshad, family = negative.binomial(theta = .01))
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
summary(model_list$disposition_int_rmile_mod)
Anova(model_list$disposition_int_rmile_mod, type = "III")

#### Predictions ####
# Calculate means and 95% confidence intervals for observed data
observed_means <- delawareshad %>% 
  group_by(r_mile, r_km, live_dead) %>% 
  summarize(obs_mean = mean(rpsm),
            obs_lwr = quantile(rpsm, 0.025),
            obs_upr = quantile(rpsm, 0.975),
            sample_size = n())

# Make predictions from the model with confidence intervals
# on the logit scale
log_predictions <- data.frame(
  predict(model_list$disposition_int_rmile_mod, 
  newdata = observed_means,
  se.fit = TRUE))
log_predictions$lwr <- log_predictions$fit - log_predictions$se.fit*1.96
log_predictions$upr <- log_predictions$fit + log_predictions$se.fit*1.96

# Convert predictions to the real scale
real_predictions <- apply(log_predictions, 2, exp) 

# Combine the observed means and CIs with the predictions
rpsm_preds <- data.frame(observed_means, real_predictions)

# Figure 5 ----
# Plot the predictions against the observed means and CIs
rpsm_plot <- ggplot(rpsm_preds, aes(x = r_km, y = obs_mean)) +
  geom_point(position = position_dodge(width = 3), size = 3) +
  geom_errorbar(aes(x = r_km, ymin = obs_lwr, ymax = obs_upr), width = 0,
                position = position_dodge(width = 3), lwd = 1)+
  geom_line(aes(y = fit), position = position_dodge(width = 3)) +
  geom_ribbon(aes(xmax = r_km, ymin = lwr, ymax = upr, color = NULL), 
              alpha = 0.10, position = position_dodge(width = 3)) +
  xlab("River mile") +
  ylab("Number of spawning marks") +
  theme(
    strip.background = element_blank(),
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 3),
    text = element_text(size = 14, color = "black")
  ) +
  facet_wrap(~live_dead)

# For viewing in Rstudio
rpsm_plot

# Figure for manuscript
jpeg(filename = "results/Figure5.jpg",
     res = 300,
     width = 2400, 
     height = 1600)
rpsm_plot
dev.off()




#### Additional summary stats ####
# Predictions across range of river miles for live and dead fish
newd <- data.frame(
  live_dead = sort(rep(c("Live", "Dead"), length(unique(delawareshad$r_mile)))),
  r_mile = rep(unique(delawareshad$r_mile), 2)
)

lpreds <- data.frame(predict(model_list$disposition_int_rmile_mod, 
                                 newdata = newd, se.fit = TRUE))
lpreds$lwr <- lpreds$fit - lpreds$se.fit*1.96
lpreds$upr <- lpreds$fit + lpreds$se.fit*1.96

rpreds <- apply(lpreds, 2, exp)

preds <- data.frame(newd, lpreds)

preds[with(preds, order(live_dead, r_mile)), ]
     
# Summary of mean number of spawning marks by live/dead
newd <- data.frame(
  live_dead = c("Live", "Dead"),
  r_mile = mean(unique(delawareshad$r_mile))
)

lpreds <- data.frame(predict(model_list$disposition_int_rmile_mod, 
                             newdata = newd, se.fit = TRUE))
lpreds$lwr <- lpreds$fit - lpreds$se.fit*1.96
lpreds$upr <- lpreds$fit + lpreds$se.fit*1.96

rpreds <- apply(lpreds, 2, exp)

preds <- data.frame(newd, lpreds)

preds


