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

outcome_of_interest <- ""

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
outcome_1

df_plot_1 <- outcome_1 %>%
	rename("value" = proportion) %>%
	filter(stratifier == "overall")
	
# Week
figure1 <- ggplot(df_plot_1, aes(x = time, y = value)) +
	geom_line() +
	geom_point() +
	xlab("Time") +
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

	
figure_1b <- ggplot(df_plot2, aes(x = plotWeek, y = value, group = year)) +
	geom_line(data = filter(df_plot2, year == 2017), alpha = 0.2) + 
	geom_line(data = filter(df_plot2, year == 2018), alpha = 0.2) + 
	geom_line(data = filter(df_plot2, year == 2019), alpha = 0.2) + 
	geom_line(aes(y = value_20), col = 2, lwd = 2) +
	geom_line(aes(y = value_hist), col = 1, lwd = 1.2) +
	geom_ribbon(aes(ymin = value_20, ymax = value_hist), fill = alpha(2, 0.2)) +
	scale_x_date(date_labels = "%b") +
	geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
	labs(x = "Time", y = "Proportion Overall", title = str_to_title(outcome_of_interest)) +
	theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
	theme_classic()

figure_1b

# Plot style 3 - 2020 focus -----------------------------------------------
df_plot3 <- df_plot2 %>%
	mutate(month = month(weekDate))

figure_1c <- ggplot(df_plot3, aes(x = as.Date("1991-01-01")+(30*month), y = value, group = month)) +
	geom_boxplot(outlier.size=0, fill = "white", position="identity", alpha=.5) +
	geom_line(aes(x = plotWeek, y = value_20), col = 2, lwd = 2) + 
	scale_x_date(date_labels = "%b") +
	geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
	labs(x = "Time", y = "Proportion Overall", title = str_to_title(outcome_of_interest)) +
	theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
	theme_classic()

cowplot::plot_grid(figure1, figure_1b ,figure_1c, ncol = 3)
#ggsave(cowplot::plot_grid(figure1, figure_1b ,figure_1c, ncol = 3), file = "graphfiles/figure1_options_depression.pdf", width = 8, height = 6)
#ggsave(figure_1b, file = "graphfiles/figure1_options_depression_B.pdf", width = 8, height = 6)

# do plot 1b by strata ----------------------------------------------------
strata_group <- "age"

plot_strata <- outcome_1 %>%
	rename("value" = proportion) %>%
	filter(stratifier == strata_group)

Plot_fmt_strata <- plot_strata %>%
	mutate(year = year(weekDate)) %>%
	mutate(week = week(weekDate)) 

Plot_2020_strata <- Plot_fmt_strata %>%
	filter(year == 2020) %>% 
	select(week, "value_20" = value, category)

Plot_historical_strata <- Plot_fmt_strata %>%
	filter(year != 2020) %>%
	group_by(week, category) %>%
	summarise(value = mean(value)) %>% 
	rename("value_hist" = value)

df_plot2_strata <- Plot_fmt_strata %>% 
	filter(year != 2020) %>%
	left_join(Plot_historical_strata, by = c("week", "category")) %>%
	left_join(Plot_2020_strata, by = c("week", "category")) %>%
	mutate(plotWeek = as.Date("1991-01-01")+(week*7)) 

figure_1_strata <- ggplot(df_plot2_strata, aes(x = plotWeek, y = value, group = factor(category), col = factor(category))) +
	geom_line(data = filter(df_plot2_strata, year == 2017), alpha = 0.2) + 
	geom_line(data = filter(df_plot2_strata, year == 2018), alpha = 0.2) + 
	geom_line(data = filter(df_plot2_strata, year == 2019), alpha = 0.2) + 
	geom_line(aes(y = value_20), lwd = 2) +
	geom_line(aes(y = value_hist), lwd = 0.5, lty = 2) +
	#geom_ribbon(aes(ymin = value_20, ymax = value_hist), fill = alpha(2, 0.2)) +
	scale_x_date(date_labels = "%b") +
	geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
	labs(x = "Time", y = "Proportion Overall", colour = "category") +
	theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
	#scale_color_manual(values = c("1" = "deeppink", "2" = "Blue")) +
	theme_classic()
figure_1_strata

df_plot3_strata <- df_plot2_strata %>%
	mutate(month = month(weekDate))

figure_1c_strata <- ggplot(df_plot3_strata, aes(x = as.Date("1991-01-01"), y = value, group = factor(category), col = factor(category))) +
	geom_boxplot(outlier.size=0, fill = "white", position="identity", alpha=.5) +
	geom_line(data = filter(df_plot3_strata, !is.na(value_20)), aes(x = plotWeek, y = value_20), lwd = 2) + 
	scale_x_date(date_labels = "%b") +
	geom_vline(xintercept = as.Date("1991-03-23"), linetype = "dashed", col = 2) +
	labs(x = "Time", y = "Proportion Overall", title = str_to_title(outcome_of_interest), colour = strata_group) +
	theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
	theme_classic()
#ggsave(figure_1c_strata, file = "graphfiles/figure1_options_depression_age.pdf", width = 8, height = 6)
