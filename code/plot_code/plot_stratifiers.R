#---------------------------------------------------------------------------------------
# Project: Covid-Collateral stratifiers plots (one outcome at a time)
# Program Name: 101_an_overallPopulation
# Author: Alasdair Henderson 
# Date Created: 16/06/2020
# Notes: Trend of diabetes recording over time in whole population
#---------------------------------------------------------------------------------------

# Flag missing libraries and install those required
list <-  c("tidyverse", "lubridate", "cowplot")
new.packages <- list[!(list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(lubridate)
library(cowplot)

#---------------------------------------------------------------------------------------
#  Import data
#---------------------------------------------------------------------------------------
setwd("~/Documents/COVID-collateral/")

outcome_of_interest <- sort(c("alcohol","anxiety","asthma","copd", "cba", "depression", "diabetes", "feedingdisorders", "hf", "mi", "ocd", "selfharm","smi", "tia", "ua", "vte"))

outcome_of_interest_namematch <- bind_cols("outcome" = outcome_of_interest, 
																					 "outcome_name" = sort(c("Alcohol", "Anxiety", "Asthma", "COPD", "Cerebrovascular Accident", "Depression", "Diabetes", "Feeding Disortders", "Heart Failure", "Myocardial Infarction", "OCD", "Self-harm", "Severe Mental Illness", "Transient Ischaemic Attacks", "Unstable Angina", "Venous Thromboembolism"))
)

files_to_import <- list.files("data/", pattern = paste0(outcome_of_interest, collapse = "|"))

for(i in 1:length(files_to_import)){
	load_file <- read_csv(paste0("data/", files_to_import[i])) %>%
		mutate_at(vars(weekDate), dmy) # convert to date
	assign(paste0("outcome_", i), load_file)
}
length(files_to_import)





# do plot 1b by strata ----------------------------------------------------
plot_strata_by_outcome <- function(run_no = NA,
											 strata_group = "age"){
	
		outcome_temp <- get(paste0("outcome_", run_no))
		## recode all the factor variables
		outcome_temp <- outcome_temp %>%
			mutate(category_cat = category) %>%
			mutate_at("category_cat" , ~ifelse(stratifier == "gender", 
																				 recode(.,`1` = "Female", `2` = "Male"),
																				 ifelse(stratifier == "age", 
																				 			 recode(.,`10` = "10-30", `20` = "10-30", `30` = "30-50", `40` = "30-50", `50` = "50-70", `60` = "50-70", `70` = "70+", `80` = "70+", `90` = "70+", `100` = "70+"),
																				 			 ifelse(stratifier == "region", 
																				 			 			 recode(.,`1` = "North East",`2` = "North West",`3` = "Yorkshire & the Humber",`4` = "East Midlands",`5` = "West Midlands",`6` = "Eastern",`7` = "South West",`8` = "South Central",`9` = "London",`10` = "South East",`11` = "Northern Ireland"),
																				 			 			 .)
																				 )
			)
		)
		## manual bodge
		if(strata_group == "age" & outcome_of_interest[run_no] == "copd"){
			outcome_temp <- outcome_temp %>%
				filter(category_cat != "30-50")
		}
		plot_strata <- outcome_temp %>%
			mutate(value = numOutcome/numEligible) %>%
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
		
		name_to_title <- outcome_of_interest_namematch %>%
			filter(outcome == outcome_of_interest[run_no]) %>%
			select(outcome_name) 
		
		figure_1c_strata <- ggplot(df_plot3_strata, aes(x = as.Date("1991-01-01"), y = value, group = factor(category_cat), col = factor(category_cat), fill = factor(category_cat))) +
			geom_boxplot(width=20, outlier.size=0, position="identity", alpha=.5) +
			geom_line(data = filter(df_plot3_strata, !is.na(value_20)), aes(x = plotWeek, y = value_20), lwd = 1.2) + 
			scale_x_date(date_labels = "%b") +
			geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
			labs(x = "Date", y = "Prop. of people consulting for outcome", title = name_to_title, colour = stringr::str_to_title(strata_group), fill = stringr::str_to_title(strata_group)) +
			theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
			theme_classic()
		
		figure_1c_strata
		return(figure_1c_strata)
}

pdf("~/Documents/COVID-Collateral/graphfiles/ageOutcomes.pdf", width = 14, height = 14)
cowplot::plot_grid(
	plot_strata_by_outcome(1),
	plot_strata_by_outcome(2),
	plot_strata_by_outcome(3),
	plot_strata_by_outcome(4),
	plot_strata_by_outcome(5),
	plot_strata_by_outcome(6),
	plot_strata_by_outcome(7),
	plot_strata_by_outcome(8),
	plot_strata_by_outcome(9),
	plot_strata_by_outcome(10),
	plot_strata_by_outcome(11),
	plot_strata_by_outcome(12),
	plot_strata_by_outcome(13),
	plot_strata_by_outcome(14),
	plot_strata_by_outcome(15),
	plot_strata_by_outcome(16),
	ncol = 4
)
dev.off()

pdf("~/Documents/COVID-Collateral/graphfiles/genderOutcomes.pdf", width = 14, height = 14)
cowplot::plot_grid(
	plot_strata_by_outcome(1, strata_group = "gender"),
	plot_strata_by_outcome(2, strata_group = "gender"),
	plot_strata_by_outcome(3, strata_group = "gender"),
	plot_strata_by_outcome(4, strata_group = "gender"),
	plot_strata_by_outcome(5, strata_group = "gender"),
	plot_strata_by_outcome(6, strata_group = "gender"),
	plot_strata_by_outcome(7, strata_group = "gender"),
	plot_strata_by_outcome(8, strata_group = "gender"),
	plot_strata_by_outcome(9, strata_group = "gender"),
	plot_strata_by_outcome(10, strata_group = "gender"),
	plot_strata_by_outcome(11, strata_group = "gender"),
	plot_strata_by_outcome(12, strata_group = "gender"),
	plot_strata_by_outcome(13, strata_group = "gender"),
	plot_strata_by_outcome(14, strata_group = "gender"),
	plot_strata_by_outcome(15, strata_group = "gender"),
	plot_strata_by_outcome(16, strata_group = "gender"),
	ncol = 4
)
dev.off()

# plot_strata(strata_group = "age")
# ggsave(file = paste0("graphfiles/figure1_strata_",outcome_of_interest,"_age.pdf"), width = 8, height = 6)
# plot_strata(strata_group = "gender")
# ggsave(file = paste0("graphfiles/figure1_strata_",outcome_of_interest,"_gender.pdf"), width = 8, height = 6)
# plot_strata(strata_group = "region")
# ggsave(file = paste0("graphfiles/figure1_strata_",outcome_of_interest,"_region.pdf"), width = 8, height = 6)
