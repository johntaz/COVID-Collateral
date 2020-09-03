#---------------------------------------------------------------------------------------
# Project: Covid-Collateral
# Program Name: 101_an_overallPopulation
# Author: John Tazare
# Date Created: 16/06/2020
# Notes: Trend of diabetes recording over time in whole population
#---------------------------------------------------------------------------------------

# Flag missing libraries and install those required
list <-  c("tidyverse", "lubridate")
new.packages <- list[!(list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(lubridate)

#---------------------------------------------------------------------------------------
#  Import data
#---------------------------------------------------------------------------------------
setwd("J:/EHR-Working/Sinead_Covid_Collaterol/datafiles")

diabetesOverall <- read_csv("J:/EHR-Working/Sinead_Covid_Collaterol/datafiles/2020_08/diabetes/an_diabetes.csv") %>%
  mutate_at(vars(weekDate), dmy)  %>% 
  filter(numOutcome > 23)
#---------------------------------------------------------------------------------------
# Analysis ignoring seasonal trend
#--------------------------------------------------------------------------------------

outcome <-  with(diabetesOverall, cbind(numOutcome, numEligible))
model1 <- glm(outcome ~ time * lockdown, data = diabetesOverall, family = binomial)
summary(model1)

lockdownWeek <- diabetesOverall %>%
  filter(weekDate == date("2020-03-22")) # week 167 is lockdown

# figure
figure1 <- ggplot(diabetesOverall, aes(x = time, y = model1$y)) +
  geom_line(color = "steelblue") +
  geom_point(size = 1) +
  xlab("Time") +
  ylab("Proportion Overall") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_x_continuous(
    breaks = c(0, 52, 104, 156, 168, 208),
    labels = c("2017", "2018", "2019", "2020", "Lockdown", "2021")
  ) +
  geom_vline(xintercept = 167, linetype = "dashed")

figure1

#ggsave("J:/EHR-Working/Sinead_Covid_Collaterol/graphfiles/diabetes/diabetesOverall_noTrend.png" , device = "png")

#---------------------------------------------------------------------------------------
# Analysis including seasonal trend
#---------------------------------------------------------------------------------------

model2 <- glm(outcome ~ sin(0.125 * time) * lockdown,
      data = diabetesOverall,
      family = binomial)
summary(model2)

# Week
figure2 <- ggplot(diabetesOverall, aes(x = time, y = model2$y)) +
  geom_line(color = "steelblue") +
  geom_point(size = 1) +
  xlab("Time") +
  ylab("Proportion Overall") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_x_continuous(
    breaks = c(0, 52, 104, 156, 167, 208),
    labels = c("2017", "2018", "2019", "2020", "Lockdown", "2021")
  ) +
  geom_vline(xintercept = 167, linetype = "dashed")

figure2

figure3 <- ggplot(diabetesOverall, aes(x = time, y = model2$y)) +
  geom_line(color = "steelblue") +
  xlab("Time") +
  ylab("Proportion Overall") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_x_continuous(
    breaks = c(0, 52, 104, 156, 167, 208),
    labels = c("2017", "2018", "2019", "2020", "Lockdown", "2021")
  ) +
  geom_vline(xintercept = 167, linetype = "dashed") +
  geom_line(aes(x = time, y = model2$fitted.values), colour = "black")

figure3

#ggsave("J:/EHR-Working/Sinead_Covid_Collaterol/graphfiles/diabetes/diabetesOverall_Trend.png" , device = "png")

