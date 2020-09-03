#---------------------------------------------------------------------------------------
# Project: Covid-Collateral stratifiers plots (one outcome at a time)
# Program Name: outcomesStratifiers
# Author: Alasdair Henderson 
# Date Created: 16/06/2020
# Notes: Stratified trends in recording over time 
#---------------------------------------------------------------------------------------

# Flag missing libraries and install those required
list <-  c("tidyverse", "lubridate", "cowplot")
new.packages <- list[!(list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(lubridate)
library(cowplot)

setwd("J:/EHR-Working/Sinead_Covid_Collaterol/datafiles/2020_08/graphdata")

# Import data -------------------------------------------------------------
outcome_of_interest <- sort(c("alcohol","anxiety", "cba", "depression", "diabetes", "feedingdisorders", "hf", "mi", "ocd", "selfharm","smi", "tia", "ua", "vte"))

files_to_import <- list.files(pattern = paste0("an_", outcome_of_interest, collapse = "|"))

for(i in 1:length(files_to_import)){
  load_file <- read_csv(paste0(files_to_import[i])) %>%
    mutate_at(vars(weekDate), dmy) %>%
    mutate(proportion = numOutcome/numEligible) %>%
    mutate(category_cat = category) %>%
    mutate_at("category_cat" , ~ifelse(stratifier == "ethnicity", 
                                       recode(., `0` ='White', `1` ='South Asian', `2` ='Black', `3` ='Other', `4` ='Mixed', `5` ='Not Stated'),
                                       ifelse(stratifier == "gender", 
                                              recode(.,`1` = "Male", `2` = "Female"),
                                              ifelse(stratifier == "age", 
                                                     recode(.,`10` = "10-30", `20` = "10-30", `30` = "30-50", `40` = "30-50", `50` = "50-70", `60` = "50-70", `70` = "70+", `80` = "70+", `90` = "70+", `100` = "70+"),
                                                     ifelse(stratifier == "region", 
                                                            recode(.,`1` = "North East",`2` = "North West",`3` = "Yorkshire & the Humber",`4` = "East Midlands",`5` = "West Midlands",`6` = "Eastern",`7` = "South West",`8` = "South Central",`9` = "London",`10` = "South East",`11` = "Northern Ireland"),
                                                            .)
                                              )
                                       )
    )
    )
  # convert to date
  assign(paste0("outcome_", i), load_file)
}

numOutcomes <- length(files_to_import)

# Plot strata ----------------------------------------------------
plot_strata <- function(ii, strata_group = "age"){
  out_file <- get(paste0("outcome_",ii))	
	plot_strata <- out_file %>%
		rename("value" = proportion) %>%
		filter(stratifier == strata_group)
	
	Plot_fmt_strata <- plot_strata %>%
		group_by(weekDate, category_cat) %>%
		summarise(value = mean(value, na.rm = T)) %>%
		ungroup() %>%
		mutate(year = year(weekDate)) %>%
		mutate(week = week(weekDate)) 
	
	Plot_2020_strata <- Plot_fmt_strata %>%
		filter(year == 2020) %>% 
		select(week, "value_20" = value, category_cat)
	
	Plot_historical_strata <- Plot_fmt_strata %>%
		filter(year != 2020) %>%
		group_by(week, category_cat) %>%
		summarise(value = mean(value, na.rm = T)) %>% 
		rename("value_hist" = value)
	
	df_plot2_strata <- Plot_fmt_strata %>% 
		filter(year != 2020) %>%
		left_join(Plot_historical_strata, by = c("week", "category_cat")) %>%
		left_join(Plot_2020_strata, by = c("week", "category_cat")) %>%
		mutate(plotWeek = as.Date("1991-01-01")+(week*7)) 
	
	df_plot3_strata <- df_plot2_strata %>%
		mutate(month = month(weekDate))
	
	figure_1c_strata <- ggplot(df_plot3_strata, aes(x = as.Date("1991-01-01"), y = value, group = factor(category_cat), col = factor(category_cat))) +
		geom_boxplot(outlier.size=0, fill = "white", position="identity", alpha=.5) +
		geom_line(data = filter(df_plot3_strata, !is.na(value_20)), aes(x = plotWeek, y = value_20), lwd = 1.2) + 
		scale_x_date(date_labels = "%b") +
		geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
		labs(x = "Time", y = "Proportion Overall", title = str_to_title(outcome_of_interest[ii]), colour = strata_group) +
		theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
		theme_classic()
	
	figure_1c_strata
	return(figure_1c_strata)
}

for(j in 1:numOutcomes) {
plot_strata(ii= j, strata_group = "age")
ggsave(file = paste0("J:/EHR-Working/Sinead_Covid_Collaterol/graphfiles/",outcome_of_interest[j],"_age.pdf"), width = 8, height = 6)
plot_strata(ii= j, strata_group = "gender")
ggsave(file = paste0("J:/EHR-Working/Sinead_Covid_Collaterol/graphfiles/",outcome_of_interest[j],"_gender.pdf"), width = 8, height = 6)
plot_strata(ii= j, strata_group = "region")
ggsave(file = paste0("J:/EHR-Working/Sinead_Covid_Collaterol/graphfiles/",outcome_of_interest[j],"_region.pdf"), width = 8, height = 6)
plot_strata(ii= j, strata_group = "ethnicity")
ggsave(file = paste0("J:/EHR-Working/Sinead_Covid_Collaterol/graphfiles/",outcome_of_interest[j],"_ethnicity.pdf"), width = 8, height = 6)

}
