#Shad Workbook
#### Loading data and tidyverse ####
library(tidyverse)
library(MASS)     
library(car)     
library(rstanarm)
library(loo)
shadmaster <- read.csv("data/SHADMASTER.csv", stringsAsFactors = FALSE)

#### Data Manipulation ####
delawareshad <- shadmaster %>% filter(shadmaster$river == "Delaware River",
                                      shadmaster$sex != "unknown")

as.numeric(delawareshad$fork)
as.numeric((delawareshad$year))

#### Significance Testing ####

# Significance of fork length by male/female
sex_length_mod <- lm(fork~sex, data = delawareshad)
summary(sex_length_mod)
anova(sex_length_mod)

# Significance of fork length by live/dead
mortality_length_mod <- lm(fork~live_dead, data = delawareshad)
summary(mortality_length_mod)
anova(mortality_length_mod)

# Significance of fork length by river mile of capture
rmile_length_mod <- lm(fork~r_mile, data = delawareshad)
summary(rmile_length_mod)
anova(rmile_length_mod)

# Significance of age by male/female
age_sex_mod <- lm(final_age~sex, data = delawareshad)
summary(age_sex_mod)
anova(age_sex_mod)

# Significance of age by live/dead
age_mortality_mod <- lm(final_age~live_dead, data = delawareshad)
summary(age_mortality_mod)
anova(age_mortality_mod)

# Significance of age by river mile of capture
age_rmile_mod <- lm(final_age~r_mile, data = delawareshad)
summary(age_rmile_mod)
anova(age_rmile_mod)

# Significance of y/n_rps by river mile of capture
rps_rmile_mod <- lm(r_mile~rps, data = delawareshad)
summary(rps_rmile_mod)
anova(rps_rmile_mod)

#Significance of sex on river mile of capture
r_mile_sex_mod <- lm(r_mile~sex, data = delawareshad)
summary(r_mile_sex_mod)
anova(r_mile_sex_mod)

# Significance of rpsm by male/female
rpsm_sex_mod <- lm(rpsm~sex, data = delawareshad)
summary(rpsm_sex_mod)
anova(rpsm_sex_mod)

# Significance of rpsm  by live/dead
rpsm_mortality_mod <- lm(rpsm~live_dead, data = delawareshad)
summary(rpsm_mortality_mod)
anova(rpsm_mortality_mod)