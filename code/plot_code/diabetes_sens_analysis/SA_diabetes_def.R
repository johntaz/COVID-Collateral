#---------------------------------------------------------------------------------------
# Project: Covid-Collateral
# Program Name: outcomesOverall
# Author: Alasdair SA_diabetes_def 
# Date Created: 07/10/2020
# Notes: How sensitive are results to the definition of "diabetes"
#---------------------------------------------------------------------------------------

# Flag missing libraries and install those required
list <-  c("tidyverse", "lubridate", "cowplot")
new.packages <- list[!(list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(lubridate)
library(cowplot)

# Import data -------------------------------------------------------------
outcome_of_interest <- c("diabetes", "diabetes_sa", "diabetes_s2")

outcome_of_interest_namematch <- bind_cols("outcome" = outcome_of_interest, 
																					 "outcome_name" = (c("A - Diabetic Emergencies", 
																					 										"B - Diabetic Emergencies (sensitivity analysis)",
																					 										"C - All diabetic (sensitivity analysis)")
																					 )
)
plot_order <- c(1,2,3) 

outcome_1 <- read_csv(here::here("data", "an_diabetes.csv")) %>%
	mutate_at(vars(weekDate), dmy) %>%
	mutate(proportion = (numOutcome/numEligible)*100) 
outcome_2 <- read_csv(here::here("data/Sens_analysis/an_diab_hyperglyc.csv")) %>%
	mutate_at(vars(weekDate), dmy) %>%
	mutate(proportion = (numOutcome/numEligible)*100) 
outcome_3 <- read_csv(here::here("data/Sens_analysis/an_diabetes_full.csv")) %>%
	mutate_at(vars(weekDate), dmy) %>%
	mutate(proportion = (numOutcome/numEligible)*100) 

plot_main <- function(ii){
	out_file <- get(paste0("outcome_",ii))
	## recode all the factor variables
	out_file <- out_file %>%
		mutate(category_cat = category) %>%
		mutate_at("category_cat" , ~ifelse(stratifier == "gender", 
																			 recode(.,`1` = "Male", `2` = "Female"),
																			 ifelse(stratifier == "age", 
																			 			 recode(.,`10` = "10-30", `20` = "10-30", `30` = "30-50", `40` = "30-50", `50` = "50-70", `60` = "50-70", `70` = "70+", `80` = "70+", `90` = "70+", `100` = "70+"),
																			 			 ifelse(stratifier == "region", 
																			 			 			 recode(.,`1` = "North East",`2` = "North West",`3` = "Yorkshire & the Humber",`4` = "East Midlands",`5` = "West Midlands",`6` = "Eastern",`7` = "South West",`8` = "South Central",`9` = "London",`10` = "South East",`11` = "Northern Ireland"),
																			 			 			 .)
																			 )
		)
		)
	
	df_plot_1 <- out_file %>%
		rename("value" = proportion) %>%
		filter(stratifier == "overall") 
	
	# Week
	figure1 <- ggplot(df_plot_1, aes(x = time, y = value)) +
		geom_line() +
		geom_point() +
		xlab("Date") +
		ylab("Proportion Overall") +
		ggtitle(stringr::str_to_title(outcome_of_interest[ii])) +
		theme_classic() +
		theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
		scale_x_continuous(
			breaks = c(0, 52, 104, 156, 167, 208),
			labels = c("2017", "2018", "2019", "2020", "Lockdown", "2021")
		) +
		geom_vline(xintercept = 167, linetype = "dashed")
	
	# Plot style 2 - relative change from baseline ----------------------------
	Plot_fmt <- df_plot_1 %>%
		mutate(year = year(weekDate)) %>%
		mutate(week = week(weekDate)) 
	
	Plot_2020 <- Plot_fmt %>%
		filter(year == 2020) %>% 
		select(week, "value_20" = value) 
	
	Plot_historical <- Plot_fmt %>%
		filter(year != 2020) %>%
		group_by(week) %>%
		summarise(value = mean(value)) %>% 
		rename("value_hist" = value)
	
	df_plot2 <- Plot_fmt %>% 
		filter(year != 2020) %>%
		left_join(Plot_historical, by = "week") %>%
		left_join(Plot_2020, by = "week") %>%
		mutate(plotWeek = as.Date("1991-01-01")+(week*7)) %>%
		mutate(value_20_low = ifelse(value_20 < value_hist, value_20, value_hist),
					 value_20_hi = ifelse(value_20 > value_hist, value_20, value_hist))
	
	
	name_to_plot_title <- outcome_of_interest_namematch %>%
		filter(outcome == outcome_of_interest[ii]) %>% 
		select(outcome_name)
	
	df_plot2 <- df_plot2 %>%
		mutate(plot_name = pull(name_to_plot_title),
					 fill_group = "lo")

	## bodging so that the lines sync up despite the colour switch
	higher_pts <- which(df_plot2$value_20 > df_plot2$value_hist)
	for(xx in higher_pts){
		dup_row <- df_plot2 %>%
			slice(xx) %>%
			mutate(fill_group = "hi")
		df_plot2 <- df_plot2 %>%
			slice(-xx) %>%
			add_row(dup_row) %>%
			add_row(dup_row)
	}
	
	df_plot2
}

pdf("~/Documents/COVID-Collateral/graphfiles/SA_diabetes.pdf", width = 13, height = 5)

plot_full <- NULL
for(ii in plot_order){
	print(ii)
	plot_full <- plot_full %>%
		bind_rows(
			plot_main(ii)
		)
}
plot_full$plot_name <- factor(plot_full$plot_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])

bkg_colour <- "white"
colors <- c("2020" = "red", "2017-2019 average" = "black")
figure_1b <- ggplot(plot_full, aes(x = plotWeek, y = value, group = year)) +
	geom_line(data = filter(plot_full, year == 2017), alpha = 0.2) +  #aes(col = "2017"), 
	geom_line(data = filter(plot_full, year == 2018), alpha = 0.2) +  #aes(col = "2018"), 
	geom_line(data = filter(plot_full, year == 2019), alpha = 0.2) +  #aes(col = "2019"), 
	geom_line(aes(y = value_hist, col = "2017-2019 average"), lwd = 1.2) +
	geom_line(aes(y = value_20, col = "2020"), lwd = 1.2) +
	geom_ribbon(aes(ymin = value_20, ymax = value_hist), fill = alpha(2, 0.2), lty = 0) +
	scale_x_date(date_labels = "%b", breaks = "2 months") +
	facet_wrap(~plot_name, scales = "free", ncol = 4) +
	geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
	labs(x = "Date", y = "% of people consulting for outcome") +
	theme_classic() +
	theme(axis.title = element_text(size = 16),
				axis.text.y = element_text(size = 12),
				axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
				legend.position = "top",
				plot.background = element_rect(fill = bkg_colour, colour =  NA),
				panel.background = element_rect(fill = bkg_colour, colour =  NA),
				legend.text = element_text(size = 12),
				legend.title = element_text(size = 12),
				legend.background = element_rect(fill = bkg_colour, colour = NA),
				strip.background = element_rect(fill = bkg_colour, colour =  NA),
				strip.text = element_text(size = 12, hjust = 0),
				panel.grid.major = element_blank(),
				panel.grid.minor.x = element_blank(),
				panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
				panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3))) +
	scale_color_manual(name = "",
										 breaks = c("2017-2019 average", "2020"),
										 labels = c("2017-2019 average", "2020"),
										 values = colors)
figure_1b
dev.off()
#ggsave(file = "~/Documents/COVID-Collateral/graphfiles/overallOutcomes.pdf", width = 12, height = 12)


