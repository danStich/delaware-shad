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
  filter(!is.na(final_age) & !is.na(fork) & !is.na(rps) & !is.na(cohort))

# Randomly assigning sex to YOY
for(i in 1:nrow(fish)) {
  if(fish$comments[i] == "YOY") {
    fish$sex[i] <- sample(c("male", "female"), 1, replace = TRUE)
  }
}

fish <- fish %>% 
  filter(sex != "unknown")

# Assign mean of river miles to YOY because we don't know where they originated
# from:
fish$river_mile[fish$comments == "YOY"] <- NA
fish$river_mile[fish$comments == "YOY"] <- mean(fish$river_mile)

# Remove biologically unreasonable measurements
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
predictions_mod <- brm(bf(fork ~ exp(linf) * (1 - exp(-exp(k)*(final_age-t0))),
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
predictions_fit <- update(predictions_mod, iter = 3000, chains = 3, cores = 3)

# Print a summary
summary(predictions_fit)

# Save the result
save(predictions_fit, file = "results/predictions_fit.rda")

# Coefficient estimates for explanatory variables ----
# Get coefficient estimates
ests <- data.frame(fixef(predictions_fit))
glimpse(ests)
names(ests)[3:4] <- c("lwr", "upr")
ests$Parameter <- row.names(ests)

ests <- ests[-grep("Intercept", ests$Parameter), ]

ests$X = c("River Mile", "Sex", "River Mile", "Sex","River Mile", "Sex")
ests$Parameter <- c("Linf", "Linf", "K", "K", "t0", "t0")
ests$Parameter <- factor(ests$Parameter, levels = c("t0", "K", "Linf"))

ggplot(ests, aes(x = Parameter, y = Estimate)) +
  geom_point() +
  geom_hline(yintercept = 0.0, linetype = 2) +
  scale_y_continuous(limits = c(-.2, .2)) +
  geom_errorbar(aes(x = Parameter, ymin = lwr, ymax = upr), width = 0) +
  coord_flip() +
  facet_wrap(~X) +
  ylab("Coefficient Estimate")


# VBGF Parameter predictions ----
# Get unique values of standardized and unstandardized river miles,
# repeat them twice so there is one observation of river mile for each
# sex
river_mile = rep(sort(unique(fish$r_mile)), 2)
river_miles = rep(sort(unique(fish$r_mile_s)), 2)

# Repeat combinations of sex for each set of river miles
sex = c(rep(1, length(unique(fish$r_mile))),
        rep(0, length(unique(fish$r_mile))))

# Make a dataframe out of the fixed-effects coefficients from 
# the prediction model object
params <- data.frame(fixef(predictions_fit))

# Create a dataframe from the unique river miles and sex that can
# be combined with the predictions
new_d <- data.frame(
  river_mile,
  river_miles,
  sex
)

# Make a copy of the new_d data frame to hold predictions for effects of 
# sex and river mile on L-infinity. Then use the fixed effects coefficients
# stored in params to predict the VBGF parameter as a function of the 
# explanatory variables as y = mx + b
new_d1 <- new_d
new_d1$fit <- exp(
  params[1,1] + params[2,1] * new_d$river_miles + params[3, 1] * new_d$sex)
new_d1$lwr <- exp(
  params[1,3] + params[2,3] * new_d$river_miles + params[3, 3] * new_d$sex)
new_d1$upr <- exp(
  params[1,4] + params[2,4] * new_d$river_miles + params[3, 4] * new_d$sex)
new_d1$Parameter <- "Linf"

# Make a copy of the new_d data frame to hold predictions for effects of 
# sex and river mile on K. Then use the fixed effects coefficients
# stored in params to predict the VBGF parameter as a function of the 
# explanatory variables as y = mx + b
new_d2 <- new_d
new_d2$fit <- exp(
  params[4,1] + params[5,1] * new_d$river_miles + params[6, 1] * new_d$sex)
new_d2$lwr <- exp(
  params[4,3] + params[5,3] * new_d$river_miles + params[6, 3] * new_d$sex)
new_d2$upr <- exp(
  params[4,4] + params[5,4] * new_d$river_miles + params[6, 4] * new_d$sex)
new_d2$Parameter <- "K"

# Make a copy of the new_d data frame to hold predictions for effects of 
# sex and river mile on t0. Then use the fixed effects coefficients
# stored in params to predict the VBGF parameter as a function of the 
# explanatory variables as y = mx + b
new_d3 <- new_d
new_d3$fit <- 
  params[7,1] + params[8,1] * new_d$river_miles + params[9, 1] * new_d$sex
new_d3$lwr <- 
  params[7,3] + params[8,3] * new_d$river_miles + params[9, 3] * new_d$sex
new_d3$upr <- 
  params[7,4] + params[8,4] * new_d$river_miles + params[9, 4] * new_d$sex
new_d3$Parameter <- "t0"

# Combine the predictions for each parameter (Linf, K, t0) into
# a single data frame that we can use for plotting
temp <- rbind(new_d1, new_d2)
stacked <- rbind(temp, new_d3)

# Clean up the sex variable to make plotting easier
stacked$Sex = as.character(stacked$sex)
stacked$Sex[stacked$sex == 0] <- "Female"
stacked$Sex[stacked$sex == 1] <- "Male"

# Graph the predictions for Linf, K, and t0 using as a function of 
# explanatory variables (river mile and sex)
ggplot(stacked, aes(x = river_mile, y = fit, color = Sex, fill = Sex)) +
  geom_line() +
  geom_ribbon(aes(xmax = river_mile, ymin = lwr, ymax = upr, color = NULL),
              alpha = 0.10) +
  facet_wrap(~Parameter, scale = "free_y")


# Model predictions ----
# Get unique combinations of sex, standardized river mile, and ages,
# and gather them into a dataframe with names matching original data
newd <- data.frame(expand.grid(
  unique(fish$sex),
  c(mean(fish$r_mile_s), min(fish$r_mile_s), max(fish$r_mile_s)),
  unique(fish$final_age)
))
names(newd) <- c("sex", "r_mile_s", "final_age")

# Make predictions from the fitted model object using newd defined above
preds <- predict(predictions_fit, newdata = newd)
head(preds)

# Combine the predictions with the newd dataframe
preds <- cbind(newd, preds)

# Add a column for un-standardized river mile that we can use for plotting
preds$r_mile <- sort(rep(  
  c(round(mean(fish$r_mile), 0), min(fish$r_mile), max(fish$r_mile)), 2))

# Format columns for plotting
preds$r_mile_s <- as.factor(preds$r_mile_s)
preds$r_mile <- as.factor(preds$r_mile)
preds$sex <- str_to_sentence(preds$sex)

# Make a graph of the predictions
ggplot(preds, aes(x = final_age, y = Estimate, color = r_mile, fill = r_mile)) +
  geom_line(data = preds, aes(x = final_age, y = Estimate)) +
  geom_ribbon(aes(xmax = final_age, ymin = Q2.5, ymax = Q97.5, color = NULL), 
              alpha = 0.1) +
  labs(color = "River mile", fill = "River mile") +
  xlab("Age (years)") +
  ylab("Fork length (mm)") +
  # geom_point(data = fish, aes(x = final_age, y= fork), color = NULL) +
  facet_wrap(~sex)

