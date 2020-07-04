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

# dm population 
diabetesPopn <- read_csv("an_diabetesdmtype.csv") %>% 
  mutate_at(vars(weekDate), dmy) # convert to date 

#---------------------------------------------------------------------------------------
# Analysis ignoring seasonal trend
#---------------------------------------------------------------------------------------

# Ignoring seasonal trend
outcomePopn <-  with(diabetesPopn, cbind(numOutcome, numEligible))
modelDM <- glm(outcomePopn ~ dm_type*time*lockdown, data = diabetesPopn, family = binomial)
summary(modelDM)

lockdownWeek <- diabetesPopn %>% 
  filter(weekDate == date("2020-03-15")) # 167

# figure
figureDM <- diabetesPopn %>%
  mutate(labels = ifelse(dm_type == 1, "Type 1 Diabetes", "Type 2 Diabetes")) %>% 
  ggplot(aes(x=time, y=modelDM$y)) +
  geom_line(aes(color = labels)) +   
  #geom_point( size = 1) +
  xlab("Time") +
  ylab("Proportion Overall") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_continuous(breaks = c(0,52, 104, 156, 167, 208), labels = c("2017","2018","2019","2020","Lockdown","2021")) +
  geom_vline(xintercept = 167, linetype = "dashed") + 
  theme(legend.title=element_blank())

figureDM
ggsave("J:/EHR-Working/Sinead_Covid_Collaterol/graphfiles/diabetes/diabetesDM_noTrend.png" , device = "png")


#---------------------------------------------------------------------------------------
# Analysis including seasonal trend
#---------------------------------------------------------------------------------------


