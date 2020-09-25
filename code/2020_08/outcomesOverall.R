#---------------------------------------------------------------------------------------
# Project: Covid-Collateral
# Program Name: outcomesOverall
# Author: Alasdair Henderson 
# Date Created: 16/06/2020
# Notes: Overall trends in recording over time
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
outcome_of_interest <- sort(c("alcohol","anxiety", "cba", "depression", "diabetes", "feedingdisorders", "hf", "mi", "ocd", "selfharm","smi", "tia", "ua", "vte",  "asthma", "copd"))

files_to_import <- list.files(pattern = paste0("an_", outcome_of_interest, collapse = "|"))

for(i in 1:length(files_to_import)){
	load_file <- read_csv(paste0(files_to_import[i])) %>%
		mutate_at(vars(weekDate), dmy) %>%
	  mutate(proportion = numOutcome/numEligible) 
	  # convert to date
	assign(paste0("outcome_", i), load_file)
}
length(files_to_import)

ii <- 1
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
	labs(x = "Date", y = "Proportion Overall", title = str_to_title(outcome_of_interest[ii])) +
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
	if(ii==100){
		figure_1b <- figure_1b +
			theme(legend.position = "none")
	}
figure_1b
}
cowplot::plot_grid(
	plot_main(1),
	plot_main(2),
	plot_main(3),
	plot_main(4),
	plot_main(5),
	plot_main(6),
	plot_main(7),
	plot_main(8),
	plot_main(9),
	plot_main(10),
	plot_main(11),
	plot_main(12),
	plot_main(13),
	plot_main(14),
	plot_main(15), 
	plot_main(16),
	
	ncol = 3
)
ggsave(file = paste0("J:/EHR-Working/Sinead_Covid_Collaterol/graphfiles/overallOutcomes.pdf"), width = 14, height = 20)

