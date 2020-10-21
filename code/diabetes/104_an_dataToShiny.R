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
setwd("/Users/lsh1510922/Documents/COVID-Collateral")

# Gender
diabetesGender <- read_csv("data/an_diabetesGender.csv") %>% 
  mutate_at(vars(weekDate), dmy) # convert to date 
# Gender
diabetesRegion <- read_csv("data/an_diabetesRegion.csv") %>% 
  mutate_at(vars(weekDate), dmy) # convert to date 

#---------------------------------------------------------------------------------------
# Analysis ignoring seasonal trend
#---------------------------------------------------------------------------------------

# Ignoring seasonal trend
outcomeGender <-  with(diabetesGender, cbind(numOutcome, numEligible))
modelGender <- glm(outcomeGender ~ gender*time*lockdown, data = diabetesGender, family = binomial)
summary(modelGender)

lockdownWeek <- diabetesGender %>% 
  filter(weekDate == date("2020-03-16")) # 167

# figure
figureGender <- diabetesGender %>%
  mutate(labels = ifelse(gender == 1, "Female", "Male")) %>%
	mutate(model_out = modelGender$y) %>%
	select(weekDate, labels, model_out)

ggplot(figureGender, aes(x = weekDate, y = model_out, color = labels)) +
  geom_line() +   
  #geom_point( size = 1) +
  xlab("Time") +
  ylab("Proportion Overall") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  #scale_x_continuous(breaks = c(0,52, 104, 156, 167, 208), labels = c("2017","2018","2019","2020","Lockdown","2021")) +
  geom_vline(xintercept = 167, linetype = "dashed") + 
  theme(legend.title=element_blank())


#ggsave("J:/EHR-Working/Sinead_Covid_Collaterol/graphfiles/diabetes/diabetesGender_noTrend.png" , device = "png")


# make the big “gender” file for shiny  -----------------------------------
figureGender_shiny <- figureGender %>%  
	mutate(outcome = "Diabetes") %>%
	bind_rows(mutate(figureGender, outcome = "CMI")) %>%
	bind_rows(mutate(figureGender, outcome = "SMI")) %>%
	bind_rows(mutate(figureGender, outcome = "Asthma")) %>%
	bind_rows(mutate(figureGender, outcome = "COPD")) %>%
	bind_rows(mutate(figureGender, outcome = "COPD")) %>%
	bind_rows(mutate(figureGender, outcome = "Cardiac failure")) %>%
	bind_rows(mutate(figureGender, outcome = "MI")) %>%
	bind_rows(mutate(figureGender, outcome = "Angina")) %>%
	bind_rows(mutate(figureGender, outcome = "Alcohol"))
save(figureGender_shiny, file = paste0(getwd(),"/code/shiny_app/covid_collateral_shiny/data/figureGender_shiny.RData"))
figureAge_shiny <- figureGender_shiny %>%
	mutate_at("labels", ~ifelse(.=="Female", ">=50", "<50")) %>%
	mutate_at("model_out", ~ifelse(labels==">=50", 0.004, 0.003))
save(figureAge_shiny, file = paste0(getwd(),"/code/shiny_app/covid_collateral_shiny/data/figureAge_shiny.RData"))

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
	mutate(model_out = modelRegion$y) %>%
	select(weekDate, labels, model_out)

ggplot(figureRegion, aes(x=weekDate, y=model_out)) +
  geom_line(aes(color = labels)) +   
  xlab("Time") +
  ylab("Proportion Overall") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  #scale_x_continuous(breaks = c(0,52, 104, 156, 167, 208), labels = c("2017","2018","2019","2020","Lockdown","2021")) +
  geom_vline(xintercept = 167, linetype = "dashed") + 
  theme(legend.title=element_blank())

figureRegion_shiny <- figureRegion %>%  
	mutate(outcome = "Diabetes") %>%
	bind_rows(mutate(figureRegion, outcome = "CMI")) %>%
	bind_rows(mutate(figureRegion, outcome = "SMI")) %>%
	bind_rows(mutate(figureRegion, outcome = "Asthma")) %>%
	bind_rows(mutate(figureRegion, outcome = "COPD")) %>%
	bind_rows(mutate(figureRegion, outcome = "COPD")) %>%
	bind_rows(mutate(figureRegion, outcome = "Cardiac failure")) %>%
	bind_rows(mutate(figureRegion, outcome = "MI")) %>%
	bind_rows(mutate(figureRegion, outcome = "Angina")) %>%
	bind_rows(mutate(figureRegion, outcome = "Alcohol"))

#ggsave("J:/EHR-Working/Sinead_Covid_Collaterol/graphfiles/diabetes/diabetesRegion_noTrend.png" , device = "png")

save(figureRegion_shiny, file = paste0(getwd(),"/code/shiny_app/covid_collateral_shiny/data/figureRegion_shiny.RData"))




