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
																					 "outcome_name" = (c("Acute Alcohol Abuse", "Anxiety", "Asthma exacerbations",  "Cerebrovascular Accident", "COPD",
																					 										"Depression", "Diabetes emergencies", "Feeding Disorders", 
																					 										"Heart Failure", "Myocardial Infarction", "Obsessive Compulsive Disorder", "Self-harm", "Severe Mental Illness", "Transient Ischaemic Attacks", 
																					 										"Unstable Angina", "Venous Thromboembolism"))
)
plot_order <- c(7,1,2,6,8,11,12,13,4,9,10,14,15,16,3,5)

files_to_import <- list.files("data/", pattern = paste0(outcome_of_interest, collapse = "|"))

for(i in 1:length(files_to_import)){
	load_file <- read_csv(paste0("data/", files_to_import[i])) %>%
		mutate_at(vars(weekDate), dmy) # convert to date
	assign(paste0("outcome_", i), load_file)
}
length(files_to_import)

running_manually <- F

# do plot 1b by strata ----------------------------------------------------
plot_strata_by_outcome <- function(run_no = 4,
											 strata_group = "age"){
	
		outcome_temp <- get(paste0("outcome_", run_no))
		
		## recode all the factor variables
		outcome_temp <- outcome_temp %>%
			mutate(category_cat = category) %>%
			mutate_at("category_cat" , ~ifelse(stratifier == "gender", 
																				 recode(.,`1` = "Female", `2` = "Male"),
																				 ifelse(stratifier == "age", 
																				 			 recode(.,
																				 			 			 `10` = "0 - 10", 
																				 			 			 `20` = "11 - 20",
																				 			 			 `30` = "21 - 30",
																				 			 			 `40` = "31 - 40",
																				 			 			 `50` = "41 - 50",
																				 			 			 `60` = "51 - 60",
																				 			 			 `70` = "61 - 70",
																				 			 			 `80` = "71 - 80",
																				 			 			 `90` = "81 - 90",
																				 			 			 `100` = "91 - 100"
																				 			 			 ),
																				 			 ifelse(stratifier == "region", 
																				 			 			 recode(.,`1` = "North East",`2` = "North West",`3` = "Yorkshire & the Humber",`4` = "East Midlands",`5` = "West Midlands",`6` = "Eastern",`7` = "South West",`8` = "South Central",`9` = "London",`10` = "South East",`11` = "Northern Ireland"),
																				 			 			 .)
																				 )
			)
		)
		# label define ageLab 
		#		`10` = "0 - 10", 
		# 	`20` = "11 - 20",
		# 	`30` = "21 - 30",
		# 	`40` = "31 - 40",
		# 	`50` = "41 - 50",
		# 	`60` = "51 - 60",
		# 	`70` = "61 - 70",
		# 	`80` = "71 - 80",
		# 	`90` = "81 - 90",
		# 	`100` = "91 - 100"
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
			mutate_at("value", ~ifelse(.==0, NA, .)) %>%
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
		
		df_plot3_strata <- df_plot3_strata %>%
			mutate(plot_name = pull(name_to_title)) 
		##
		if(running_manually){
			ggplot(df_plot3_strata, aes(x = as.Date("1991-01-01"), y = value, group = factor(category_cat), col = factor(category_cat), fill = factor(category_cat))) +
				geom_boxplot(width=20, outlier.size=0, position="identity", alpha=.5) +
				geom_line(data = filter(strat_plot_data, !is.na(value_20)), aes(x = plotWeek, y = value_20), lwd = 1.2) + 
				scale_x_date(date_labels = "%b") +
				facet_wrap(~plot_name, scales = "free", ncol = 4) +
				geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
				labs(x = "Date", y = "Prop. of people consulting for outcome", title = "", colour = "Age", fill = "Age") +
				theme_classic()  +
				theme(axis.text.x = element_text(angle = 60, hjust = 1),
							strip.background = element_rect(fill = bkg_colour, colour =  NA),
							strip.text = element_text(hjust = 0)) 
			}
		return(df_plot3_strata)
}

pdf("~/Documents/COVID-Collateral/graphfiles/ageOutcomes_09-24.pdf", width = 14, height = 14)
	strat_plot_data <- NULL
	for(ii in plot_order){
		strat_plot_data <- strat_plot_data %>%
			bind_rows(
				plot_strata_by_outcome(ii, strata_group = "age")
			)
	}
	strat_plot_data$plot_name <- factor(strat_plot_data$plot_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])
	bkg_colour <- "white"
	figure_1c_strata <- ggplot(strat_plot_data, aes(x = as.Date("1991-01-01"), y = value, group = factor(category_cat), col = factor(category_cat), fill = factor(category_cat))) +
		geom_boxplot(width=20, outlier.size=0, position="identity", alpha=.5) +
		geom_line(data = filter(strat_plot_data, !is.na(value_20)), aes(x = plotWeek, y = value_20), lwd = 1.2) + 
		scale_x_date(date_labels = "%b") +
		facet_wrap(~plot_name, scales = "free", ncol = 4) +
		geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
		labs(x = "Date", y = "Prop. of people consulting for outcome", title = "", colour = "Age", fill = "Age") +
		theme_classic()  +
		theme(axis.text.x = element_text(angle = 60, hjust = 1),
					strip.background = element_rect(fill = bkg_colour, colour =  NA),
					strip.text = element_text(hjust = 0)) 
	figure_1c_strata
dev.off()




pdf("~/Documents/COVID-Collateral/graphfiles/ethnicityOutcomes.pdf", width = 14, height = 14)
	strat_plot_data <- NULL
	for(ii in plot_order){
		strat_plot_data <- strat_plot_data %>%
			bind_rows(
				plot_strata_by_outcome(ii, strata_group = "ethnicity")
			)
	}
	strat_plot_data$plot_name <- factor(strat_plot_data$plot_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])
	bkg_colour <- "white"
	figure_1c_strata <- ggplot(strat_plot_data, aes(x = as.Date("1991-01-01"), y = value, group = factor(category_cat), col = factor(category_cat), fill = factor(category_cat))) +
		geom_boxplot(width=20, outlier.size=0, position="identity", alpha=.5) +
		geom_line(data = filter(strat_plot_data, !is.na(value_20)), aes(x = plotWeek, y = value_20), lwd = 1.2) + 
		scale_x_date(date_labels = "%b") +
		facet_wrap(~plot_name, scales = "free", ncol = 4) +
		geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
		labs(x = "Date", y = "Prop. of people consulting for outcome", title = "", colour = "Ethnicity", fill = "Ethnicity") +
		theme_classic()  +
		theme(axis.text.x = element_text(angle = 60, hjust = 1),
					strip.background = element_rect(fill = bkg_colour, colour =  NA),
					strip.text = element_text(hjust = 0)) 
	figure_1c_strata
dev.off()

pdf("~/Documents/COVID-Collateral/graphfiles/genderOutcomes.pdf", width = 14, height = 14)
	strat_plot_data <- NULL
	for(ii in plot_order){
		strat_plot_data <- strat_plot_data %>%
			bind_rows(
				plot_strata_by_outcome(ii, strata_group = "gender")
			)
	}
	strat_plot_data$plot_name <- factor(strat_plot_data$plot_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])
	bkg_colour <- "white"
	figure_1c_strata <- ggplot(strat_plot_data, aes(x = as.Date("1991-01-01"), y = value, group = factor(category_cat), col = factor(category_cat), fill = factor(category_cat))) +
		geom_boxplot(width=20, outlier.size=0, position="identity", alpha=.5) +
		geom_line(data = filter(strat_plot_data, !is.na(value_20)), aes(x = plotWeek, y = value_20), lwd = 1.2) + 
		scale_x_date(date_labels = "%b") +
		facet_wrap(~plot_name, scales = "free", ncol = 4) +
		geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
		labs(x = "Date", y = "Prop. of people consulting for outcome", title = "", colour = "Gender", fill = "Gender") +
		theme_classic()  +
		theme(axis.text.x = element_text(angle = 60, hjust = 1),
					strip.background = element_rect(fill = bkg_colour, colour =  NA),
					strip.text = element_text(hjust = 0)) 
	figure_1c_strata
dev.off()


pdf("~/Documents/COVID-Collateral/graphfiles/regionOutcomes.pdf", width = 14, height = 14)
strat_plot_data <- NULL
for(ii in plot_order){
	strat_plot_data <- strat_plot_data %>%
		bind_rows(
			plot_strata_by_outcome(ii, strata_group = "region")
		)
}
strat_plot_data$plot_name <- factor(strat_plot_data$plot_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])
bkg_colour <- "white"
figure_1c_strata <- ggplot(strat_plot_data, aes(x = as.Date("1991-01-01"), y = value, group = factor(category_cat), col = factor(category_cat), fill = factor(category_cat))) +
	geom_boxplot(width=20, outlier.size=0, position="identity", alpha=.5) +
	geom_line(data = filter(strat_plot_data, !is.na(value_20)), aes(x = plotWeek, y = value_20), lwd = 1.2) + 
	scale_x_date(date_labels = "%b") +
	facet_wrap(~plot_name, scales = "free", ncol = 4) +
	geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
	labs(x = "Date", y = "Prop. of people consulting for outcome", title = "", colour = "Region", fill = "Region") +
	theme_classic()  +
	theme(axis.text.x = element_text(angle = 60, hjust = 1),
				axis.text = element_text(size = 10), 
				strip.background = element_rect(fill = bkg_colour, colour =  NA),
				strip.text = element_text(hjust = 0, size = 10)) 
figure_1c_strata
dev.off()
