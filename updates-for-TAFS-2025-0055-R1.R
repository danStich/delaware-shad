# Code checks for confounding factors identified by reviewer 2 ----
# Libraries ----
library(tidyverse)

# Data read ----
shadmaster <- read.csv("data/SHADMASTER.csv", stringsAsFactors = FALSE)

# Data Manipulation ----
# Reducing the data set to only include adult, sexed fish from
# the Delaware river with assigned ages and fork lengths
delawareshad <- shadmaster %>% 
  filter(shadmaster$river == "Delaware River",
         shadmaster$sex != "unknown",
         !is.na(fork) & fork > 100,
         shadmaster$live_dead != "unknown") %>% 
  mutate(sex = str_to_title(sex))

# Year effects ----
# . Size Distributions ----
ggplot(delawareshad, aes(fork)) +
  geom_histogram() +
  facet_wrap(~year, ncol = 1)

t.test(fork ~ year, data = delawareshad)
# .. No significant differences among years ----

# Age distributions
ggplot(delawareshad, aes(final_age)) +
  geom_histogram() +
  facet_wrap(~year, ncol = 1)

t.test(final_age ~ year, data = delawareshad)
# .. Significantly younger mean age in 2020 (Smithfield) ----

ggplot(delawareshad, aes(x = r_km, y = final_age)) +
  geom_point() +
  facet_wrap(~year, ncol = 1)

with(delawareshad, table(year, r_km))

#### Potential Gear effects ####
# Gear Type	
# 1	gill net
# 2	electrofishing
# 3	conventional angling
# 4	scavange survey for dead shad
# 5	beach seine (YOY survey)
delawareshad$gear <- factor(delawareshad$gear,
                            levels = c(1, 2, 3, 4, 5),
                            labels = c("Gill net", "Electrofishing",
                                       "Angling", "Scavenge", "YOY seine"))

# Sex-specific differences among gears for FL
anova(lm(fork ~ gear, data = delawareshad[delawareshad$sex == "Female",]))
TukeyHSD(aov(lm(fork ~ gear, data = delawareshad[delawareshad$sex == "Female",])))

# Sex-specific differences among gears for age
anova(lm(final_age ~ gear, data = delawareshad[delawareshad$sex == "Female",]))
TukeyHSD(aov(lm(final_age ~ gear, data = delawareshad[delawareshad$sex == "Female",])))

#### Date effects ####
delawareshad$date <- as.Date(delawareshad$date, format = "%m/%d/%y")
delawareshad$doy <- lubridate::yday(delawareshad$date)

anova(lm(final_age ~ date, data = delawareshad[delawareshad$sex == "Female",]))
anova(lm(final_age ~ date, data = delawareshad[delawareshad$sex == "Male",]))

with(delawareshad, table(doy, r_km))

