#---------------------------------------------------------------------------------------
# Project: Covid-Collateral
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

outcome_of_interest <- "schizo"

files_to_import <- list.files("data/", pattern = outcome_of_interest)

for(i in 1:length(files_to_import)){
	load_file <- read_csv(paste0("data/", files_to_import[i])) %>%
		mutate_at(vars(weekDate), dmy) # convert to date
	assign(paste0("outcome_", i), load_file)
}
length(files_to_import)

#---------------------------------------------------------------------------------------
# Analysis ignoring seasonal trend
#--------------------------------------------------------------------------------------
if(outcome_of_interest == "diabetes"){
	outcome_1 <- outcome_1 %>%
		mutate(proportion = numOutcome/numEligible) 
}
## recode all the factor variables
outcome_1 <- outcome_1 %>%
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

df_plot_1 <- outcome_1 %>%
	rename("value" = proportion) %>%
	filter(stratifier == "overall") 

# Week
figure1 <- ggplot(df_plot_1, aes(x = time, y = value)) +
	geom_line() +
	geom_point() +
	xlab("Date") +
	ylab("Proportion Overall") +
	ggtitle(stringr::str_to_title(outcome_of_interest)) +
	theme_classic() +
	theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
	scale_x_continuous(
		breaks = c(0, 52, 104, 156, 167, 208),
		labels = c("2017", "2018", "2019", "2020", "Lockdown", "2021")
	) +
	geom_vline(xintercept = 167, linetype = "dashed")
figure1
ggsave(figure1, file = paste0("graphfiles/figure1_A_",outcome_of_interest,".pdf"), width = 8, height = 6)

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
	mutate(plotWeek = as.Date("1991-01-01")+(week*7)) 

bkg_colour <- "white"
colors <- c(#"2017" = alpha("gray10",0.2), "2018" = alpha("gray10",0.2), "2019" = alpha("gray10",0.2),  
						"2020" = "red", "2017-2019 average" = "black")
figure_1b <- ggplot(df_plot2, aes(x = plotWeek, y = value, group = year)) +
	geom_line(data = filter(df_plot2, year == 2017), alpha = 0.2) +  #aes(col = "2017"), 
	geom_line(data = filter(df_plot2, year == 2018), alpha = 0.2) +  #aes(col = "2018"), 
	geom_line(data = filter(df_plot2, year == 2019), alpha = 0.2) +  #aes(col = "2019"), 
	geom_line(aes(y = value_20, col = "2020"), lwd = 1.2) +
	geom_line(aes(y = value_hist, col = "2017-2019 average"), lwd = 1.2) +
	geom_ribbon(aes(ymin = value_20, ymax = value_hist), fill = alpha(2, 0.2)) +
	scale_x_date(date_labels = "%b") +
	geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
	labs(x = "Date", y = "Proportion Overall", title = str_to_title(outcome_of_interest)) +
	theme_classic() +
	theme(axis.text.x = element_text(angle = 60, hjust = 1),
				legend.position = "top",
				plot.background = element_rect(fill = bkg_colour, colour =  NA),
				panel.background = element_rect(fill = bkg_colour, colour =  NA),
				legend.background = element_rect(fill = bkg_colour, colour = NA),
				strip.background = element_rect(fill = bkg_colour, colour =  NA),
				panel.grid.major = element_blank(),
				panel.grid.minor.x = element_blank(),
				panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
				panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3))) +
	scale_color_manual(name = "",
										 breaks = c("2017-2019 average", "2020"),
										 values = colors)
figure_1b

# Plot style 3 - 2020 focus -----------------------------------------------
df_plot3 <- df_plot2 %>%
	mutate(month = month(weekDate))

figure_1c <- ggplot(df_plot3, aes(x = as.Date("1991-01-01")+(30*month), y = value, group = month)) +
	geom_boxplot(outlier.size=0, fill = "gray80", position="identity", alpha=.5) +
	geom_line(aes(x = plotWeek, y = value_20), col = 2, lwd = 1.2) + 
	scale_x_date(date_labels = "%b") +
	geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
	labs(x = "Time", y = "Proportion Overall", title = str_to_title(outcome_of_interest)) +
	theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
	theme_classic() +
	theme(axis.text.x = element_text(angle = 60, hjust = 1),
				legend.position = "top",
				plot.background = element_rect(fill = bkg_colour, colour =  NA),
				panel.background = element_rect(fill = bkg_colour, colour =  NA),
				legend.background = element_rect(fill = bkg_colour, colour = NA),
				strip.background = element_rect(fill = bkg_colour, colour =  NA),
				panel.grid.major = element_blank(),
				panel.grid.minor.x = element_blank(),
				panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
				panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3))) 
figure_1c


# Plot style 4 - change from baseline  ------------------------------------
df_plot4 <- df_plot2 %>%
	filter(year < 2020) %>%
	group_by(week) %>%
	summarise(avg_val = mean(value), val2020 = mean(value_20)) %>%
	mutate(change_baseline =( (val2020 - avg_val)/avg_val)*100)

figure_1d <- ggplot(filter(df_plot4, !is.na(change_baseline)), aes(x = as.Date("2020-01-01")+(week*7), y = change_baseline)) +
	geom_line(col = 2, lwd = 1.2) + 
	geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed", col = 2) +
	geom_hline(yintercept = 0, linetype = "dashed", col = "gray60") +
	scale_x_date(date_labels = "%b") +
	labs(x = "Time", y = "% change from weekly average (2017-2019)", title = str_to_title(outcome_of_interest)) +
	theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
	theme_classic() +
	theme(axis.text.x = element_text(angle = 60, hjust = 1),
				legend.position = "top",
				plot.background = element_rect(fill = bkg_colour, colour =  NA),
				panel.background = element_rect(fill = bkg_colour, colour =  NA),
				legend.background = element_rect(fill = bkg_colour, colour = NA),
				strip.background = element_rect(fill = bkg_colour, colour =  NA),
				panel.grid.major = element_blank(),
				panel.grid.minor.x = element_blank(),
				panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
				panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3))) 
figure_1d
plot_options <- cowplot::plot_grid(figure1, figure_1b ,figure_1c, figure_1d, ncol = 2)
plot_options
ggsave(plot_options, file = paste0("graphfiles/figure1_options_",outcome_of_interest,".pdf"), width = 8, height = 6)
ggsave(figure_1b, file = paste0("graphfiles/figure1_B_",outcome_of_interest,".pdf"), width = 8, height = 6)
ggsave(figure_1c, file = paste0("graphfiles/figure1_C_",outcome_of_interest,".pdf"), width = 8, height = 6)
ggsave(figure_1d, file = paste0("graphfiles/figure1_D_",outcome_of_interest,".pdf"), width = 8, height = 6)


# do plot 1b by strata ----------------------------------------------------
plot_strata <- function(strata_group = "age"){

plot_strata <- outcome_1 %>%
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

figure_1_strata <- ggplot(df_plot2_strata, aes(x = plotWeek, y = value, group = factor(category_cat), col = factor(category_cat))) +
	geom_line(data = filter(df_plot2_strata, year == 2017), alpha = 0.2) + 
	geom_line(data = filter(df_plot2_strata, year == 2018), alpha = 0.2) + 
	geom_line(data = filter(df_plot2_strata, year == 2019), alpha = 0.2) + 
	geom_line(aes(y = value_20), lwd = 1.2) +
	geom_line(aes(y = value_hist), lwd = 0.5) +
	#geom_ribbon(aes(ymin = value_20, ymax = value_hist), fill = alpha(2, 0.2)) +
	scale_x_date(date_labels = "%b") +
	geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
	labs(x = "Time", y = "Proportion Overall", colour = "category_cat") +
	theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
	#scale_color_manual(values = c("1" = "deeppink", "2" = "Blue")) +
	theme_classic()
#figure_1_strata

df_plot3_strata <- df_plot2_strata %>%
	mutate(month = month(weekDate))

figure_1c_strata <- ggplot(df_plot3_strata, aes(x = as.Date("1991-01-01"), y = value, group = factor(category_cat), col = factor(category_cat))) +
	geom_boxplot(outlier.size=0, fill = "white", position="identity", alpha=.5) +
	geom_line(data = filter(df_plot3_strata, !is.na(value_20)), aes(x = plotWeek, y = value_20), lwd = 1.2) + 
	scale_x_date(date_labels = "%b") +
	geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
	labs(x = "Time", y = "Proportion Overall", title = str_to_title(outcome_of_interest), colour = strata_group) +
	theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
	theme_classic()

df_plot4_strata <- df_plot2_strata %>%
	filter(year < 2020) %>%
	group_by(week, category_cat) %>%
	summarise(avg_val = mean(value, na.rm = T), val2020 = mean(value_20, na.rm = T)) %>%
	mutate(change_baseline =( (val2020 - avg_val)/avg_val)*100)
figure_1d_strata <- ggplot(filter(df_plot4_strata, !is.na(change_baseline)), aes(x = as.Date("2020-01-01")+(week*7), y = change_baseline, group = factor(category_cat), col = factor(category_cat))) +
	geom_line(lwd = 1.2) + 
	geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed", col = 2) +
	geom_hline(yintercept = 0, linetype = "dashed", col = "gray60") +
	scale_x_date(date_labels = "%b") +
	labs(x = "Time", y = "% change from weekly average (2017-2019)", 
			 title = str_to_title(outcome_of_interest), col = strata_group) +
	theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
	theme_classic() +
	theme(axis.text.x = element_text(angle = 60, hjust = 1),
				legend.position = "top",
				plot.background = element_rect(fill = bkg_colour, colour =  NA),
				panel.background = element_rect(fill = bkg_colour, colour =  NA),
				legend.background = element_rect(fill = bkg_colour, colour = NA),
				strip.background = element_rect(fill = bkg_colour, colour =  NA),
				panel.grid.major = element_blank(),
				panel.grid.minor.x = element_blank(),
				panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
				panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3))) 
figure_1_strata
figure_1c_strata
figure_1d_strata
return(figure_1d_strata)
}


unique(outcome_1$stratifier)
plot_strata(strata_group = "age")
ggsave(file = paste0("graphfiles/figure1_strata_",outcome_of_interest,"_age.pdf"), width = 8, height = 6)
plot_strata(strata_group = "gender")
ggsave(file = paste0("graphfiles/figure1_strata_",outcome_of_interest,"_gender.pdf"), width = 8, height = 6)
#plot_strata(strata_group = "region")
#ggsave(file = paste0("graphfiles/figure1_strata_",outcome_of_interest,"_region.pdf"), width = 8, height = 6)

