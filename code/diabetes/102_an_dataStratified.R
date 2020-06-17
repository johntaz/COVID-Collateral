#---------------------------------------------------------------------------------------
# Project: Covid-Collateral
# Program Name: 102_an_stratifiedDiabetes
# Author: John Tazare
# Date Created: 16/06/2020
# Notes: Trend of diabetes recording over time in whole population by gender & region
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
setwd("J:/EHR-Working/Sinead_Covid_Collaterol/datafiles/diabetes/")

# Gender
diabetesGender <- read_csv("an_diabetesGender.csv") %>% 
  mutate_at(vars(weekDate), dmy) # convert to date 

# Region
diabetesRegion <- read_csv("an_diabetesRegion.csv") %>% 
  mutate_at(vars(weekDate), dmy) # convert to date 

#---------------------------------------------------------------------------------------
# Analysis ignoring seasonal trend
#---------------------------------------------------------------------------------------

# Ignoring seasonal trend
outcomeGender <-  with(diabetesGender, cbind(numOutcome, numEligible))
modelGender <- glm(outcomeGender ~ gender*time*lockdown, data = diabetesGender, family = binomial)
summary(modelGender)

lockdownWeek <- diabetesGender %>% 
  filter(weekDate == date("2020-03-15")) # 167

# figure
figureGender <- diabetesGender %>%
  mutate(labels = ifelse(gender == 1, "Female", "Male")) %>% 
  ggplot(aes(x=time, y=modelGender$y)) +
  geom_line(aes(color = labels)) +   
  #geom_point( size = 1) +
  xlab("Time") +
  ylab("Proportion Overall") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_continuous(breaks = c(0,52, 104, 156, 167, 208), labels = c("2017","2018","2019","2020","Lockdown","2021")) +
  geom_vline(xintercept = 167, linetype = "dashed") + 
  theme(legend.title=element_blank())

figureGender
ggsave("J:/EHR-Working/Sinead_Covid_Collaterol/graphfiles/diabetes/diabetesGender_noTrend.png" , device = "png")

# Ignoring seasonal trend
outcomeRegion <-  with(diabetesRegion, cbind(numOutcome, numEligible))
modelRegion <- glm(outcomeRegion ~ region*time*lockdown, data = diabetesRegion, family = binomial)
summary(modelRegion)

lockdownWeek <- diabetesRegion %>% 
  filter(weekDate == date("2020-03-15")) # 167

# figure
figureRegion <-  diabetesRegion %>% 
  mutate(region = as.factor(region)) %>%
  mutate(labels = 
           case_when( region == 1 ~ "North East",
                      region == 2 ~ "North West",
                      region == 3 ~ "Yorkshire",
                      region == 4 ~ "East Midlands",
                      region == 5 ~ "West Midlands",
                      region == 6 ~ "East of England",
                      region == 7 ~ "South West",
                      region == 8 ~ "South Central",
                      region == 9 ~ "London",
                      region == 10 ~ "South East Coast",
                      region == 11 ~ "Northern Ireland" ) 
    
  ) %>%
  ggplot(aes(x=time, y=modelRegion$y)) +
  geom_line(aes(color = labels)) +   
  xlab("Time") +
  ylab("Proportion Overall") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_continuous(breaks = c(0,52, 104, 156, 167, 208), labels = c("2017","2018","2019","2020","Lockdown","2021")) +
  geom_vline(xintercept = 167, linetype = "dashed") + 
  theme(legend.title=element_blank())

figureRegion
ggsave("J:/EHR-Working/Sinead_Covid_Collaterol/graphfiles/diabetes/diabetesRegion_noTrend.png" , device = "png")


#---------------------------------------------------------------------------------------
# Analysis including seasonal trend
#---------------------------------------------------------------------------------------


