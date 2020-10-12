#---------------------------------------------------------------------------------------
# Project: Covid-Collateral stratifiers plots (one outcome at a time)
# Program Name: 101_an_overallPopulation
# Author: Alasdair Henderson 
# Date Created: 16/06/2020
# Notes: all outcomes, by straifiers
# Ref: https://github.com/johntaz/COVID-Collateral 
#---------------------------------------------------------------------------------------

# Flag missing libraries and install those required
list <-  c("tidyverse", "lubridate", "cowplot")
new.packages <- list[!(list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(lubridate)

setwd("~/Documents/COVID-collateral/")

# import data -------------------------------------------------------------
outcome_of_interest <- sort(c("alcohol","anxiety","asthma","copd", "cba", "depression", "diabetes", "feedingdisorders", "hf", "mi", "ocd", "selfharm","smi", "tia", "ua", "vte"))

outcome_of_interest_namematch <- bind_cols("outcome" = outcome_of_interest, 
																					 "outcome_name" = (c("Acute Alcohol-Related Event", "Anxiety", "Asthma exacerbations",  "Cerebrovascular Accident", "COPD",
																					 										"Depression", "Diabetes Emergencies", "Eating Disorders", 
																					 										"Heart Failure", "Myocardial Infarction", "Obsessive Compulsive Disorder", "Self-harm", "Severe Mental Illness", "Transient Ischaemic Attacks", 
																					 										"Unstable Angina", "Venous Thromboembolism"))
)
outcome_of_interest_namematch_Age <- bind_cols("outcome" = outcome_of_interest, 
																					 "outcome_name" = (c("Acute Alcohol-Related Event (18+)", "Anxiety", "Asthma exacerbations",  "Cerebrovascular Accident (31+)", "COPD (41+)",
																					 										"Depression", "Diabetes Emergencies", "Eating Disorders", 
																					 										"Heart Failure (31+)", "Myocardial Infarction (31+)", "Obsessive Compulsive Disorder", "Self-harm", "Severe Mental Illness", "Transient Ischaemic Attacks (31+)", 
																					 										"Unstable Angina (31+)", "Venous Thromboembolism (31+)"))
)
plot_order <- c(7,1,2,6,8,11,12,13,4,9,10,14,15,16,3,5)

files_to_import <- list.files("data/", pattern = paste0(outcome_of_interest, collapse = "|"))

for(i in 1:length(files_to_import)){
	load_file <- read_csv(paste0("data/", files_to_import[i])) %>%
		mutate_at(vars(weekDate), dmy) # convert to date
	assign(paste0("outcome_", i), load_file)
}
length(files_to_import)

# do plot 1b by strata ----------------------------------------------------
plot_strata_by_outcome <- function(run_no = 7,strata_group = "age"){
	
		outcome_temp <- get(paste0("outcome_", run_no))
		
		## recode all the factor variables
		outcome_temp <- outcome_temp %>%
			mutate(category_cat = category) %>%
			mutate_at("category_cat" , ~ifelse(stratifier == "gender", 
																				 recode(.,`1` = "Female", `2` = "Male"),
																				 ifelse(stratifier == "age", 
																				 			 recode(.,
																				 			 			 `10` = "0 - 10", 
																				 			 			 `20` = "11 - 30",
																				 			 			 `30` = "11 - 30",
																				 			 			 `40` = "31 - 50",
																				 			 			 `50` = "31 - 50",
																				 			 			 `60` = "51 - 70",
																				 			 			 `70` = "51 - 70",
																				 			 			 `80` = "71+",
																				 			 			 `90` = "71+",
																				 			 			 `100` = "71+"
																				 			 			 ),
																				 			 ifelse(stratifier == "region", 
																				 			 			 recode(.,
																				 			 			 			 `1` = "North (NE, NW, Yorkshire, NI)",   #`1` = "North East",
																				 			 			 			 `2` = "North (NE, NW, Yorkshire, NI)",   #`2` = "North West",
																				 			 			 			 `3` = "North (NE, NW, Yorkshire, NI)",   #`3` = "Yorkshire & the Humber",
																				 			 			 			 `4` = "Midlands",   #`4` = "East Midlands",
																				 			 			 			 `5` = "Midlands",   #`5` = "West Midlands",
																				 			 			 			 `6` = "Midlands",   #`6` = "Eastern",
																				 			 			 			 `7` = "South (SW, SC, SE)",   #`7` = "South West",
																				 			 			 			 `8` = "South (SW, SC, SE)",   #`8` = "South Central",
																				 			 			 			 `9` = "London",   #`9` = "London",
																				 			 			 			 `10` = "South (SW, SC, SE)",   #`10` = "South East",
																				 			 			 			 `11` = "North (NE, NW, Yorkshire, NI)"   #`11` = "Northern Ireland"
																				 			 			 			 ),
																				 			 ifelse(stratifier == "ethnicity", 
																				 			 			 recode(.,
																				 			 			 			 `0` = "White",
																				 			 			 			 `1` = "South Asian",
																				 			 			 			 `2` = "Black",
																				 			 			 			 `3` = "Other/Mixed",
																				 			 			 			 `4` = "Other/Mixed",
																				 			 			 			 `5` = "Missing"),
																				 			 			 .)
																				 			 )
																				 )
			)
		)
		## calc proportion consulting overall
		plot_strata <- outcome_temp %>%
			#mutate_at("numOutcome", ~ifelse(. == 5, 0, .)) %>%
			#mutate(value = (numOutcome/numEligible)*100) %>%
			filter(stratifier == strata_group)
		
		## find categories that do not exceed at any point in 2020
		group_low_cat <- plot_strata %>%
			filter(weekDate >= as.Date("2020-01-01")) %>%
			group_by(category) %>%
			summarise(max_outcome= max(numOutcome, na.rm = T)) %>%
			ungroup()
		
		# get year and week of data
		Plot_fmt_strata <- plot_strata %>%
			left_join(group_low_cat, by = "category") %>%
			mutate_at("numOutcome", ~ifelse(max_outcome == 5 & weekDate >= as.Date("2020-01-01"), NA, .)) %>%
			group_by(weekDate, category_cat) %>%
			summarise(max_outcome = max(numOutcome, na.rm = T),
								sum_outcome = sum(numOutcome, na.rm = T),
								sum_denom = sum(numEligible, na.rm = T)) %>%
			ungroup() %>%
			mutate_at("sum_outcome", ~ifelse(max_outcome == -Inf, NA, .)) %>%
			mutate(value = (sum_outcome/sum_denom)*100) %>%
			mutate(year = year(weekDate)) %>%
			mutate(week = week(weekDate)) 
		
		# take data from 2020 and creat as a new variable
		Plot_2020_strata <- Plot_fmt_strata %>%
			filter(year == 2020) %>% 
			#mutate_at("value", ~ifelse(. %in% c(0), NA, .)) %>%
			select(week, "value_20" = value, category_cat)
		
		# take historic data (<2020) and calculate weekly mean
		Plot_historical_strata <- Plot_fmt_strata %>%
			filter(year != 2020) %>%
			group_by(week, category_cat) %>%
			summarise(value = mean(value, na.rm = T)) %>%
			rename("value_hist" = value)
		
		# merge historic data with historic average and 2020 data
		df_plot2_strata <- Plot_fmt_strata %>% 
			filter(year != 2020) %>%
			left_join(Plot_historical_strata, by = c("week", "category_cat")) %>%
			left_join(Plot_2020_strata, by = c("week", "category_cat")) %>%
			mutate(plotWeek = as.Date("1991-01-01")+(week*7)) 
		
		df_plot3_strata <- df_plot2_strata %>%
			mutate(month = month(weekDate)) 
		
		## change the names used for plots slightly if age 
		if(strata_group == "age"){
			name_to_title <- outcome_of_interest_namematch_Age %>%
				filter(outcome == outcome_of_interest[run_no]) %>%
				select(outcome_name) 
		}else{
			name_to_title <- outcome_of_interest_namematch %>%
				filter(outcome == outcome_of_interest[run_no]) %>%
				select(outcome_name) 
		}
		df_plot3_strata <- df_plot3_strata %>%
			mutate(plot_name = pull(name_to_title)) 
		##
		return(df_plot3_strata)
}


# plot by age -------------------------------------------------------------
pdf("~/Documents/COVID-Collateral/graphfiles/strat_ageOutcomes_v3.pdf", width = 14.5, height = 14.5)
	strat_plot_data <- NULL
	for(ii in plot_order){
		strat_plot_data <- strat_plot_data %>%
			bind_rows(
				plot_strata_by_outcome(ii, strata_group = "age")
			)
	}
	strat_plot_data$plot_name <- factor(strat_plot_data$plot_name, levels = outcome_of_interest_namematch_Age$outcome_name[plot_order])
	
	bkg_colour <- "white"
	
	figure_1c_strata <- ggplot(strat_plot_data, aes(x = as.Date("1991-01-01"), y = value, group = factor(category_cat), col = factor(category_cat), fill = factor(category_cat))) +
		geom_boxplot(width=20, outlier.size=0, position="identity", alpha=.5) +
		geom_line(data = filter(strat_plot_data, !is.na(value_20)), aes(x = plotWeek, y = value_20), lwd = 1.2) + 
		scale_x_date(date_labels = "%b", breaks = "1 month") +
		facet_wrap(~plot_name, scales = "free", ncol = 4) +
		geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
		labs(x = "Date (2020)", y = "% of people consulting for outcome", title = "", colour = "Age", fill = "Age") +
		#scale_x_date() + 
		theme_classic()  +
		theme(axis.title = element_text(size = 18),
					axis.text = element_text(size = 12),
					axis.text.x = element_text(angle = 60, hjust = 1),
					legend.text = element_text(size = 12),
					legend.title = element_text(size = 12),
					legend.position = "top",
					strip.background = element_rect(fill = bkg_colour, colour =  NA),
					strip.text = element_text(size = 12, hjust = 0)) 
	figure_1c_strata
dev.off()


# plot by ethnicity -------------------------------------------------------
pdf("~/Documents/COVID-Collateral/graphfiles/strat_ethnicityOutcomes.pdf", width = 14, height = 14)
	strat_plot_data <- NULL
	for(ii in plot_order){
		strat_plot_data <- strat_plot_data %>%
			bind_rows(
				plot_strata_by_outcome(ii, strata_group = "ethnicity")
			)
	}
	strat_plot_data$plot_name <- factor(strat_plot_data$plot_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])
	bkg_colour <- "white"
	strat_plot_data <- strat_plot_data %>%
		filter(category_cat != "Missing")
	figure_1c_strata <- ggplot(strat_plot_data, aes(x = as.Date("1991-01-01"), y = value, group = factor(category_cat), col = factor(category_cat), fill = factor(category_cat))) +
		geom_boxplot(width=20, outlier.size=0, position="identity", alpha=.5) +
		geom_line(data = filter(strat_plot_data, !is.na(value_20)), aes(x = plotWeek, y = value_20), lwd = 1.2) + 
		scale_x_date(date_labels = "%b", breaks = "1 month") +
		facet_wrap(~plot_name, scales = "free", ncol = 4) +
		geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
		labs(x = "Date (2020)", y = "% of people consulting for outcome", title = "", colour = "Ethnicity", fill = "Ethnicity") +
		theme_classic()  +
		theme(axis.title = element_text(size = 18),
					axis.text = element_text(size = 12),
					axis.text.x = element_text(angle = 60, hjust = 1),
					legend.text = element_text(size = 12),
					legend.title = element_text(size = 12),
					legend.position = "top",
					strip.background = element_rect(fill = bkg_colour, colour =  NA),
					strip.text = element_text(size = 12, hjust = 0)) 
figure_1c_strata
dev.off()


# plot by gender ----------------------------------------------------------
pdf("~/Documents/COVID-Collateral/graphfiles/strat_genderOutcomes.pdf", width = 14, height = 14)
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
		scale_x_date(date_labels = "%b", breaks = "1 month") +
		facet_wrap(~plot_name, scales = "free", ncol = 4) +
		geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
		labs(x = "Date (2020)", y = "% of people consulting for outcome", colour = "Gender", fill = "Gender") +
		theme_classic()  +
		theme(axis.title = element_text(size = 18),
					axis.text = element_text(size = 12),
					axis.text.x = element_text(angle = 60, hjust = 1),
					legend.text = element_text(size = 12),
					legend.title = element_text(size = 12),
					legend.position = "top",
					strip.background = element_rect(fill = bkg_colour, colour =  NA),
					strip.text = element_text(size = 12, hjust = 0)) 
	figure_1c_strata
dev.off()

# plot by region ----------------------------------------------------------
pdf("~/Documents/COVID-Collateral/graphfiles/strat_regionOutcomes.pdf", width = 14, height = 14)
strat_plot_data <- NULL
for(ii in plot_order){
	strat_plot_data <- strat_plot_data %>%
		bind_rows(
			plot_strata_by_outcome(ii, strata_group = "region")
		)
}
strat_plot_data$plot_name <- factor(strat_plot_data$plot_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])
bkg_colour <- "white"
strat_plot_data <- strat_plot_data %>%
	filter(!is.na(category_cat))

figure_1c_strata <- ggplot(strat_plot_data, aes(x = as.Date("1991-01-01"), y = value, group = factor(category_cat), col = factor(category_cat), fill = factor(category_cat))) +
	geom_boxplot(width=20, outlier.size=0, position="identity", alpha=.5) +
	geom_line(data = filter(strat_plot_data, !is.na(value_20)), aes(x = plotWeek, y = value_20), lwd = 1.2) + 
	scale_x_date(date_labels = "%b", breaks = "1 month") +
	facet_wrap(~plot_name, scales = "free", ncol = 4) +
	geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
	labs(x = "Date (2020)", y = "% of people consulting for outcome", title = "", colour = "Region", fill = "Region") +
	theme_classic()  +
	theme(axis.title = element_text(size = 18),
				axis.text = element_text(size = 12),
				axis.text.x = element_text(angle = 60, hjust = 1),
				legend.text = element_text(size = 12),
				legend.title = element_text(size = 12),
				legend.position = "top",
				strip.background = element_rect(fill = bkg_colour, colour =  NA),
				strip.text = element_text(size = 12, hjust = 0)) 
figure_1c_strata
dev.off()
