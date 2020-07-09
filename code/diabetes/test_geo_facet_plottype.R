#---------------------------------------------------------------------------------------
# Project: Covid-Collateral
# Program Name: 101_an_overallPopulation
# Author: John Tazare
# Date Created: 16/06/2020
# Notes: Trend of diabetes recording over time in whole population
#---------------------------------------------------------------------------------------

# Flag missing libraries and install those required
list <-  c("tidyverse", "lubridate", "geofacet")
new.packages <- list[!(list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(lubridate)
library(geofacet)

#---------------------------------------------------------------------------------------
#  Import data
#---------------------------------------------------------------------------------------
setwd("~/Documents/COVID-collateral/")

diabetesOverall <- read_csv("data/an_diabetesOverall.csv") %>%
	mutate_at(vars(weekDate), dmy) # convert to date
diabetesRegion <- read_csv("data/an_diabetesRegion.csv") %>%
	mutate_at(vars(weekDate), dmy) # convert to date

outcome <-  with(diabetesOverall, cbind(numOutcome, numEligible))
model1 <- glm(outcome ~ time * lockdown, data = diabetesOverall, family = binomial)
summary(model1)

diabetesOverall <- diabetesOverall %>%
	mutate(model_out = model1$y)

# figure
figure1 <- ggplot(diabetesOverall, aes(x = time, y = model_out)) +
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
figure1

# diabetesRegion <- diabetesOverall %>%
# 	mutate(region = 99)
# for(ii in 1:11){
# 	random_denom <- round(runif(1, 8, 12))
# 	region_temp <- tibble(
# 			weekDate = seq.Date(as.Date("2017-01-01"), as.Date("2020-04-2020"), by = "7 days"),
# 			numEligible = c(round(runif(166, 1e7/random_denom, 1.05e7/random_denom)), round(7592029/random_denom), round(runif(6, 5e6/random_denom, 6e6/random_denom))),
# 			numOutcome = c(round(runif(165, 6e4/random_denom, 75e3/random_denom)), round(c(44846/random_denom, 22000/random_denom)), round(runif(6, 8000/random_denom, 12000/random_denom)))
# 		) %>%
# 		mutate(time = 1:n(),
# 					 lockdown = 0,
# 					 region = ii) %>%
# 		mutate_at("lockdown", ~ifelse(weekDate>as.Date("2020-03-28"), 1, 0)) 
# 	
# 	diabetesRegion <- diabetesRegion %>%
# 		bind_rows(region_temp)
# }
# 
# diabetesRegion <- diabetesRegion %>%
# 	filter(region != 99)

# Ignoring seasonal trend
outcomeRegion <-  with(diabetesRegion, cbind(numOutcome, numEligible))
modelRegion <- glm(outcomeRegion ~ region*time*lockdown, data = diabetesRegion, family = binomial)
summary(modelRegion)

# figure
df_figureRegion <-  diabetesRegion %>% 
	# merge on model results
	mutate(model_out = modelRegion$y) %>% 
	mutate(region = as.factor(region)) %>%
	mutate(labels = 
				 	case_when( region == 1 ~ "North East",
				 						 region == 2 ~ "North West",
				 						 region == 3 ~ "Yorkshire & the Humber",
				 						 region == 4 ~ "East Midlands",
				 						 region == 5 ~ "West Midlands",
				 						 region == 6 ~ "Eastern",
				 						 region == 7 ~ "South West",
				 						 region == 8 ~ "South Central",
				 						 region == 9 ~ "London",
				 						 region == 10 ~ "South East",
				 						 region == 11 ~ "Northern Ireland" ) 
				 
	) %>%
	rename(name = labels)

figureRegion <- ggplot(filter(df_figureRegion, weekDate >= as.Date("2019-01-01")), aes(x=time, y=model_out)) +
	geom_line(aes(color = name)) +   
	xlab("Time") +
	ylab("Proportion Overall") +
	theme_classic() +
	theme(axis.text.x=element_text(angle=60, hjust=1)) +
	scale_x_continuous(breaks = c(0,52, 104, 156, 167, 208), labels = c("2017","2018","2019","2020","Lockdown","2021")) +
	geom_vline(xintercept = 167, linetype = "dashed") + 
	theme(legend.title=element_blank())


namestoinclude <- c("North East","North West","Yorkshire & the Humber","East Midlands","West Midlands","Eastern","South West","South Central","London","South East","Northern Ireland")
southcentral <- bind_cols("row" = 7, "col" = 3, "name" = "South Central", "code" = "UKK")
my_grid <- geofacet::uk_regions1 %>%
	filter(name %in% namestoinclude) %>%
	bind_rows(southcentral)

figureRegion_facet <- ggplot(filter(df_figureRegion, weekDate >= as.Date("2019-01-01")), aes(x=time, y=model_out, group = name, colour = name)) +
	geom_line() +   
	xlab("Time") +
	ylab("Proportion Overall") +
	theme_classic() +
	theme(axis.text.x=element_text(angle=60, hjust=1)) +
	scale_x_continuous(breaks = c(0,52, 104, 156, 167, 208), labels = c("2017","2018","2019","2020","Lockdown","2021")) +
	geom_vline(xintercept = 167, linetype = "dashed") + 
	theme(legend.position = "none") + 	
	#organise by state name in grid file
	geofacet::facet_geo(~name, grid = my_grid) 

#ggsave("~lsh1510922/Documents/COVID-collateral/graphfiles/test_UKfacet.pdf", width = 8, height = 8)


figureRegion
figureRegion_facet
