#---------------------------------------------------------------------------------------
# Project: Covid-Collateral
# Program Name: outcomesOverall
# Author: Alasdair Henderson 
# Date Created: 02/09/2020
# Notes: ITS analysis of lockdown on multiple outcomes
# Ref: https://github.com/johntaz/COVID-Collateral 
#---------------------------------------------------------------------------------------

# load the packages
library(foreign)
library(tsModel)
library(lmtest)
library(Epi)
library(multcomp)
library(splines)
library(vcd)
library(here)
library(lubridate)
library(stringr)
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)

bkg_colour <- "gray99"

# read data from csv file
all_files <- list.files(here::here("data/"), pattern = "an_")
outcomes <- stringr::str_remove_all(all_files, c("an_|.csv"))

outcome_of_interest_namematch <- bind_cols("outcome" = outcomes, 
																					 "outcome_name" = (c("Acute Alcohol-Related Event", "Anxiety", "Asthma exacerbations",  "Cerebrovascular Accident", "COPD",
																					 										"Depression", "Diabetes Emergencies", "Feeding Disorders", 
																					 										"Heart Failure", "Myocardial Infarction", "OCD", "Self-harm", "Severe Mental Illness", "Transient Ischaemic Attacks", 
																					 										"Unstable Angina", "Venous Thromboembolism"))
)
plot_order <- c(7,1,2,6,8,11,12,13,4,14,9,10,15,16,3,5) ## plots the outcomes by disease system as defined by ICD-10 chapter (diabetes, alcohol, mental health, circulatory system, respiratory system)

# load data ---------------------------------------------------------------
for(ii in 1:length(outcomes)){
	load_file <- read.csv(here::here("data", paste0("an_", outcomes[ii], ".csv")))
	assign(outcomes[ii], load_file)
}

its_function <- function(outcomes_vec = outcomes,
												 cutData = as.Date("2018-01-01"), 
												 start_lockdown =   as.Date("2020-03-08"),
												 lockdown_adjustment_period_wks = 4,
												 end_post_lockdown_period = as.Date("2020-08-01"),
												 chop_selfharm = TRUE,
												 display_from = as.Date("2020-01-01")
												 ){
			
			xmas_dates <- c( ## 1 week with 25th Decemeber as a midpoint, for each of 2017, 18, & 19 
				seq.Date(as.Date("2017-12-22"), as.Date("2017-12-29"), by = "1 day"),
				seq.Date(as.Date("2018-12-22"), as.Date("2018-12-29"), by = "1 day"),
				seq.Date(as.Date("2019-12-22"), as.Date("2019-12-29"), by = "1 day"))
		
		plot_its <- function(outcome){
			if(outcome == "selfharm" & chop_selfharm){cutData <- as.Date("2019-01-01")}
			df_outcome <- get(outcome)
			df_outcome <- df_outcome %>%
				filter(stratifier == "overall") %>%
				dplyr::select(-stratifier, -category) %>%
				mutate(weekPlot = (time*7) + as.Date("2017-01-01")) %>%
				mutate(months = as.numeric(format.Date(weekPlot, "%m"))) %>%
				mutate(year = as.numeric(format.Date(weekPlot, "%Y"))-2017) %>%
				mutate(pre_lockdown = ifelse(weekPlot < start_lockdown, 1 , 0),
							 post_lockdown = ifelse(weekPlot > (start_lockdown + (7*lockdown_adjustment_period_wks)) & 
							 											 	weekPlot < end_post_lockdown_period, 1, 0)) %>%
				mutate_at("lockdown", ~ifelse(weekPlot %in% seq.Date(start_lockdown, (start_lockdown + (7*lockdown_adjustment_period_wks)), "1 day"), NA, 
																			ifelse(weekPlot >= end_post_lockdown_period, NA, .))) %>%
				mutate(xmas = ifelse(weekPlot %in% xmas_dates, 1, 0)) %>%
				filter(weekPlot >= cutData)
	
			## model binomial 
			# Change in level + slope:
			### include interaction with time (centred at end of Lockdown adjustment period)
			ldn_centre <- df_outcome$time[min(which(df_outcome$lockdown == 1))]
			
			## fit model, calculate lagged residuals to fit in final model
			binom_model1 <- glm(as.matrix(cbind(numOutcome, numEligible)) ~ lockdown + I(time-ldn_centre) + I(time-ldn_centre):lockdown + as.factor(months) , family=binomial, data = filter(df_outcome, !is.na(lockdown)))
			ci.exp(binom_model1)
			binom_lagres <- lag(residuals(binom_model1)) %>% as.numeric()
			res1 <- residuals(binom_model1,type="deviance")
			
			## manipulate data so output looks cleaner
			model_data <- df_outcome %>% 
				mutate(timeC = time - ldn_centre) %>%
				mutate_at("months", ~as.factor(.)) 
			## fit model with lagged residuals 
			binom_model2 <- glm(as.matrix(cbind(numOutcome, numEligible)) ~ lockdown + timeC + timeC:lockdown + as.factor(months)  + binom_lagres, family=binomial, data = filter(model_data, !is.na(lockdown)))
			ci.exp(binom_model2)
			summary.glm(binom_model2)
			
			## calculate dispersion adjustment parameter -- https://online.stat.psu.edu/stat504/node/162/
			#Pearson Goodness-of-fit statistic
			pearson_gof <- sum(residuals(binom_model2, type = "pearson")^2)
			df <- binom_model2$df.residual
			deviance_adjustment <- pearson_gof/df
			
			## some manual manipulation to merge the lagged residuals varaible back with the original data
			missing_data_start <- min(which(is.na(model_data$lockdown)))
			missing_data_end <- max(which(is.na(model_data$lockdown)))
			missing_data_restart <- max(which(is.na(model_data$lockdown)))
			binom_lagres_timing <- bind_cols("time" = model_data$time[!is.na(model_data$lockdown)],
																			 "binom_lagres" = binom_lagres)
			
			## set up data frame to calculate linear predictions
			outcome_pred <- model_data %>%
				left_join(binom_lagres_timing, by = "time") %>%
				mutate_at("binom_lagres", ~(. = 0)) 
			
			## set up data frame to calculate linear predictions with month and xmas averaged at Sep
			outcome_pred_zeroed <- model_data %>%
				left_join(binom_lagres_timing, by = "time") %>%
				mutate_at("binom_lagres", ~(. = 0)) %>%
				mutate_at("xmas", ~(. = 0)) %>%
				mutate_at("year", ~(. = 0)) %>% 
				mutate_at("months", ~(. = 9)) 
			
			## predict values adjusted for overdispersion
			pred1 <- predict(binom_model2, newdata = outcome_pred, se.fit = TRUE, interval="confidence", dispersion = deviance_adjustment)
				predicted_vals <- pred1$fit
				stbp <- pred1$se.fit
			
			## predict values adjusted for overdispersion
			pred0 <- predict(binom_model2, newdata = outcome_pred_zeroed, se.fit = TRUE, interval="confidence", dispersion = deviance_adjustment)
				predicted_vals_0 <- pred0$fit
				stbp0 <- pred0$se.fit
			
			## set up data frame to calculate linear predictions with no Lockdown and predict values
			outcome_pred_nointervention <- outcome_pred %>%
				mutate_at("lockdown", ~(.=0))
			pred_noLockdown <- predict(binom_model2, newdata = outcome_pred_nointervention, se.fit = TRUE, interval="confidence", dispersion = deviance_adjustment) 
				pred_noLdn <- pred_noLockdown$fit
				stbp_noLdn <- pred_noLockdown$se.fit
				
			## combine all those predictions and convert from log odds to percentage reporting
			df_se <- bind_cols(stbp = stbp, stbp0 = stbp0, stbp_noLdn = stbp_noLdn, 
												 pred = predicted_vals, pred0 = predicted_vals_0, pred_noLdn = pred_noLdn) %>%
				mutate(
					#CIs
					upp = pred + (1.96*stbp),
					low = pred - (1.96*stbp),
					upp0 = pred0 + (1.96*stbp0),
					low0 = pred0 - (1.96*stbp0),
					upp_noLdn = pred_noLdn + (1.96*stbp_noLdn),
					low0_noLdn = pred_noLdn - (1.96*stbp_noLdn),
					# probline
					predicted_vals = exp(pred)/(1+exp(pred)),
					probline_0 = exp(pred0)/(1+exp(pred0)),
					probline_noLdn = exp(pred_noLdn)/(1+exp(pred_noLdn)),
					#
					uci = exp(upp)/(1+exp(upp)),
					lci = exp(low)/(1+exp(low)),
					#
					uci0 = exp(upp0)/(1+exp(upp0)),
					lci0 = exp(low0)/(1+exp(low0)),
					#
					uci_noLdn = exp(upp_noLdn)/(1+exp(upp_noLdn)),
					lci_noLdn = exp(low0_noLdn)/(1+exp(low0_noLdn)) 
					)
			
			## combine data set and predictions
			outcome_plot <- bind_cols(outcome_pred, df_se)
			
			## blank out "lockdown period" in predicted values
			outcome_plot <- outcome_plot %>%
				mutate_at(vars("predicted_vals", "lci", "uci"), ~ifelse(weekPlot < as.Date("2020-03-30") & weekPlot >= as.Date("2020-03-16"), NA, .)) %>%
				mutate(var = outcome)
			
			## Get ORs for effect of lockdown
			paramter_estimates <- as.data.frame(ci.exp(binom_model2))
			vals_to_print <- paramter_estimates %>%
				mutate(var = rownames(paramter_estimates)) %>%
				filter(var == "lockdown") %>%
				mutate(var = outcome)
			
			## Get ORs for effect of time on outcome after lockdown (time + interaction of time:lockdown)
			interaction_lincom <- glht(binom_model2, linfct = c("timeC + lockdown:timeC = 0"))
			summary(interaction_lincom)
			
			out <- confint(interaction_lincom)
			time_grad_postLdn <- out$confint[1,] %>% exp() %>% t() %>% as.data.frame() 
			interaction_to_print <- time_grad_postLdn %>%
				mutate(var = outcome)
			
			## output
		return(list(df_1 = outcome_plot, vals_to_print = vals_to_print, interaction_to_print = interaction_to_print))
		}
		
		# the plot ----------------------------------------------------------------
		main_plot_data <- NULL
		forest_plot_data <- NULL
		interaction_tbl_data <- NULL
		for(ii in 1:length(outcomes_vec)){
			main_plot_data <- main_plot_data %>%
				bind_rows(
					plot_its(outcomes_vec[ii])$df_1
				)
			forest_plot_data <- forest_plot_data %>%
				bind_rows(
					plot_its(outcomes_vec[ii])$vals_to_print
				)
			interaction_tbl_data <- interaction_tbl_data %>%
				bind_rows(
					plot_its(outcomes_vec[ii])$interaction_to_print
				)
		}
		
		## convert proportions into percentage 
		main_plot_data <- main_plot_data %>%
			mutate(pc_consult = (numOutcome/numEligible)*100) %>%
			mutate_at(.vars = c("predicted_vals", "lci", "uci", "probline_noLdn", "uci_noLdn", "lci_noLdn", "probline_0", "lci0", "uci0"), 
								~.*100) %>%
			left_join(outcome_of_interest_namematch, by = c("var" = "outcome"))
		## replace outcome name with the pretty name for printing on results
		main_plot_data$outcome_name <- factor(main_plot_data$outcome_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])
		
		abline_min <- main_plot_data$weekPlot[min(which(is.na(main_plot_data$lockdown)))]-7
		abline_max <- main_plot_data$weekPlot[max(which(is.na(main_plot_data$lockdown)))+1]
		plot1 <- ggplot(filter(main_plot_data, weekPlot >= display_from), aes(x = weekPlot, y = pc_consult, group = outcome_name)) +
			# the data
			geom_line(col = "gray60") +
			### the probability if therer was no lockdwon
			#geom_line(data = filter(main_plot_data, weekPlot >= abline_min), aes(y = probline_noLdn), col = 2, lty = 2) +
			#geom_ribbon(data = filter(main_plot_data, weekPlot >= abline_min), aes(ymin = lci_noLdn, ymax=uci_noLdn), fill = alpha(2,0.4), lty = 0) +
			### probability with model (inc. std. error)
			geom_line(aes(y = predicted_vals), col = 4, lty = 2) +
			geom_ribbon(aes(ymin = lci, ymax=uci), fill = alpha(4,0.4), lty = 0) +
			### format the plot
			facet_wrap(~outcome_name, scales = "free", ncol = 4) +
			geom_vline(xintercept = c(abline_min, 
																abline_max), col = 1, lwd = 1) + # 2020-04-05 is first week/data After lockdown gap
			labs(y = "% of people consulting for outcome", title = "A") +
			theme_classic() +
			theme(axis.title = element_text(size =16), 
						axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
						legend.position = "top",
						plot.background = element_rect(fill = bkg_colour, colour =  NA),
						panel.background = element_rect(fill = bkg_colour, colour =  NA),
						legend.background = element_rect(fill = bkg_colour, colour = NA),
						legend.text = element_text(size = 12),
						legend.title = element_text(size = 12),
						strip.text = element_text(size = 12, hjust = 0),
						strip.background = element_rect(fill = bkg_colour, colour =  NA),
						panel.grid.major = element_blank(),
						panel.grid.minor.x = element_blank(),
						panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
						panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3))) 
		
		if(lubridate::year(display_from) == 2020){
			plot1 <- plot1 + 
				scale_x_date(breaks = "1 month", date_labels = "%b") +
				labs(x = "Date (2020)")
		}else{
			plot1 <- plot1 + 
				scale_x_date(breaks = "1 year", date_labels = "%Y") +
				labs(x = "Year")
		}
		#plot1
		
		# Forest plot of interaction terms ------------------------------------------------------
		## clean up the names
		interaction_tbl_data <- interaction_tbl_data %>%
			rename("Est" = "Estimate", lci = lwr, uci = upr) %>%
			left_join(outcome_of_interest_namematch, by = c("var" = "outcome"))
		
		# changes the names of outcomes to full names
		interaction_tbl_data$outcome_name <- factor(interaction_tbl_data$outcome_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])
			# export table of results for the appendix 
			pastename_year_cutdata <- lubridate::year(cutData)
			write.csv(interaction_tbl_data, file = here::here("graphfiles", paste0("its_main_INTORs_",start_lockdown,"_",lockdown_adjustment_period_wks, "_", pastename_year_cutdata, ".csv")))
		
		# forest plot of estiamtes
		fp2 <- ggplot(data=interaction_tbl_data, aes(x=outcome_name, y=Est, ymin=lci, ymax=uci)) +
			geom_point(size = 0.2, pch = 1) +
			geom_errorbar(width = 0.2) +
			geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
			coord_flip() +  # flip coordinates (puts labels on y axis)
			labs(x = "", y = 'OR (95% CI)', title = "C: Recovery") + 
			theme_classic() +
			theme(axis.title = element_text(size = 16),
						axis.text.x = element_text(angle = 45),
						axis.text.y = element_blank(),
						legend.position = "top",
						plot.background = element_rect(fill = bkg_colour, colour =  NA),
						panel.background = element_rect(fill = bkg_colour, colour =  NA),
						legend.background = element_rect(fill = bkg_colour, colour = NA),
						strip.background = element_rect(fill = bkg_colour, colour =  NA),
						panel.grid.major = element_blank(),
						panel.grid.minor.x = element_blank(),
						panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
						panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3))) +
			scale_x_discrete(limits = rev(levels(as.factor(interaction_tbl_data$outcome_name))))
		#fp2
		
		# Forest plot of ORs ------------------------------------------------------
		## clean up the names
		forest_plot_df <- forest_plot_data %>%
			rename("Est" = "exp(Est.)", "lci" = "2.5%", "uci" = "97.5%") %>%
			left_join(outcome_of_interest_namematch, by = c("var" = "outcome"))
		
		# changes the names of outcomes to full names
		forest_plot_df$outcome_name <- factor(forest_plot_df$outcome_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])
					# export table of results for the appendix 
					write.csv(forest_plot_df, file = here::here("graphfiles", paste0("its_main_ORs_",start_lockdown,"_",lockdown_adjustment_period_wks,  "_", pastename_year_cutdata, ".csv")))
		
		## Forest plot
		fp <- ggplot(data=forest_plot_df, aes(x=outcome_name, y=Est, ymin=lci, ymax=uci)) +
			geom_point(size = 0.2, pch = 1) +
			geom_errorbar(width = 0.2) +
			geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
			coord_flip() +  # flip coordinates (puts labels on y axis)
			labs(x = "", y = "OR (95% CI)", title = "B: Reduction") + 
			theme_classic() +
			theme(axis.title = element_text(size = 16),
						axis.text.y = element_text(angle = 45, hjust = 1),
						axis.text.x = element_text(angle = 45),
						legend.position = "top",
						plot.background = element_rect(fill = bkg_colour, colour =  NA),
						panel.background = element_rect(fill = bkg_colour, colour =  NA),
						legend.background = element_rect(fill = bkg_colour, colour = NA),
						strip.background = element_rect(fill = bkg_colour, colour =  NA),
						panel.grid.major = element_blank(),
						panel.grid.minor.x = element_blank(),
						panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
						panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3))) +
			scale_x_discrete(limits = rev(levels(as.factor(forest_plot_df$outcome_name))))
		#fp
		
		# Export plot -------------------------------------------------------------
		## uses patchwork package to combine plots
		layout = "
			AAAAAAAABC
			AAAAAAAABC
			AAAAAAAABC
		"
		plot1 + fp + fp2 + 
			plot_layout(design = layout) 
}

## Figure 3 in in paper
pdf(file = here::here("graphfiles", paste0("Figure2_its_backdata_full", ".pdf")), width = 15, height = 7)
its_function(outcomes_vec = outcomes,
						 cutData = as.Date("2017-01-01"),
						 start_lockdown = as.Date("2020-03-08"),
						 lockdown_adjustment_period_wks = 5,
						 end_post_lockdown_period = as.Date("2020-07-31"), 
						 chop_selfharm <- T, 
						 display_from <- as.Date("2020-01-01")
)
dev.off()


# SA1 compare cutting data at 2017 or 2019 --------------------------------
## run once with all data (including self-harm) back to 2017 for the appendix
pdf(file = here::here("graphfiles", paste0("SA1_its_backdata_2017_inclSelfHarm", ".pdf")), width = 15, height = 7)
its_function(outcomes_vec = outcomes,
						 cutData = as.Date("2017-01-01"),
						 start_lockdown = as.Date("2020-03-08"),
						 lockdown_adjustment_period_wks = 5,
						 end_post_lockdown_period = as.Date("2020-07-31"), 
						 chop_selfharm = F,
						 display_from = as.Date("2017-01-01")
)
dev.off()


## Run as in paper
pdf(file = here::here("graphfiles", paste0("SA1_its_backdata_2017", ".pdf")), width = 15, height = 7)
its_function(outcomes_vec = outcomes,
						 cutData = as.Date("2017-01-01"),
						 start_lockdown = as.Date("2020-03-08"),
						 lockdown_adjustment_period_wks = 5,
						 end_post_lockdown_period = as.Date("2020-07-31"), 
						 chop_selfharm <- T,
						 display_from = as.Date("2017-01-01")
)
dev.off()
## 2019 data
pdf(file = here::here("graphfiles", paste0("SA1_its_backdata_2019", ".pdf")), width = 15, height = 7)
its_function(outcomes_vec = outcomes,
						 cutData = as.Date("2019-01-01"),
						 start_lockdown = as.Date("2020-03-08"),
						 lockdown_adjustment_period_wks = 5,
						 end_post_lockdown_period = as.Date("2020-07-31"),
						 display_from = as.Date("2019-01-01")
)
dev.off()

			## read in data for 2017 chop and 2020 chop 
			ors_2017 <- read.csv(here::here("graphfiles", "its_main_ORs_2020-03-08_5_2017.csv")) %>%
				mutate(chopdata = as.character(2017))
			ors_2019 <- read.csv(here::here("graphfiles", "its_main_ORs_2020-03-08_5_2019.csv")) %>%
				mutate(chopdata = as.character(2019))
			fp_ors <- ors_2017 %>% 
				bind_rows(ors_2019)
			
			# changes the names of outcomes to full names
			fp_ors$outcome_name <- factor(fp_ors$outcome_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])
			levels(fp_ors$outcome_name)[7] <- "Self-harm*"
			
			fp_compare1 <- ggplot(data=fp_ors, aes(x=chopdata, y=Est, ymin=lci, ymax=uci, group = chopdata , colour = chopdata)) +
				geom_point(size = 0.2, pch = 1) +
				geom_errorbar(width = 0.2) +
				#geom_pointrange(size = 0.2, pch = 1) +
				geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
				#facet_wrap(~outcome_name, ncol = 1)
				facet_wrap(~outcome_name, strip.position="right", dir = "h", ncol=1, scales = "free_y") +
				labs(x = "", y = "OR (95% CI)", title = "Reduction", colour = "Using data from", 
						 caption = "*Self-harm data restricted to 2019 because of coding switch in 2018") + 
				theme_classic() +
				theme(axis.title = element_text(size = 16),
							axis.text.x = element_text(angle = 45),
							#axis.text.y = element_text(angle = 45, hjust = 1),
							#strip.text = element_text(angle = 45, hjust = 1),
							axis.text.y = element_blank(),
							axis.ticks.y = element_blank(),
							axis.line.y =  element_blank(),
							legend.position = "top",
							plot.background = element_rect(fill = bkg_colour, colour =  NA),
							panel.background = element_rect(fill = bkg_colour, colour =  NA),
							legend.background = element_rect(fill = bkg_colour, colour = NA),
							strip.background = element_rect(fill = bkg_colour, colour =  NA),
							strip.text.y = element_text(hjust=0.5, vjust = 0, angle=0, size = 10),
							panel.grid.major = element_blank(),
							panel.grid.minor.x = element_blank(),
							panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
							panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3))) +
				coord_flip() 
			
			## repeat for recovery in data for 2017 chop and 2020 chop 
			int_ors_2017 <- read.csv(here::here("graphfiles", "its_main_INTORs_2020-03-08_5_2017.csv")) %>%
				mutate(chopdata = as.character(2017))
			int_ors_2019 <- read.csv(here::here("graphfiles", "its_main_INTORs_2020-03-08_5_2019.csv")) %>%
				mutate(chopdata = as.character(2019))
			fp_int_ors <- int_ors_2017 %>% 
				bind_rows(int_ors_2019)
			
			# changes the names of outcomes to full names
			fp_int_ors$outcome_name <- factor(fp_int_ors$outcome_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])
			levels(fp_int_ors$outcome_name)[7] <- "Self-harm*"
			
			fp_compare2 <- ggplot(data=fp_int_ors, aes(x=chopdata, y=Est, ymin=lci, ymax=uci, group = chopdata , colour = chopdata)) +
				geom_point(size = 0.2, pch = 1) +
				geom_errorbar(width = 0.2) +
				#geom_pointrange(size = 0.2, pch = 1) +
				geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
				#facet_wrap(~outcome_name, ncol = 1)
				facet_wrap(~outcome_name, strip.position="right", dir = "h", ncol=1, scales = "free_y") +
				labs(x = "", y = "OR (95% CI)", title = "Recovery", colour = "Using data from") + 
				theme_classic() +
				theme(axis.title = element_text(size = 16),
							axis.text.x = element_text(angle = 45),
							axis.text.y = element_blank(),
							axis.ticks.y = element_blank(),
							axis.line.y =  element_blank(),
							legend.position = "top",
							plot.background = element_rect(fill = bkg_colour, colour =  NA),
							panel.background = element_rect(fill = bkg_colour, colour =  NA),
							legend.background = element_rect(fill = bkg_colour, colour = NA),
							strip.background = element_rect(fill = bkg_colour, colour =  NA),
							#strip.text.y = element_text(hjust=1, vjust = 0, angle=0, size = 10),
							strip.text.y = element_blank(),
							panel.grid.major = element_blank(),
							panel.grid.minor.x = element_blank(),
							panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
							panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3))) +
				coord_flip() 
			
			## output the two forest plots side by side
			pdf(file = here::here("graphfiles", paste0("FP_compare_data_exclusion.pdf")), width = 12, height = 8)
				fp_compare1 +
					fp_compare2
			dev.off()	
	
	## export these ORs and int_ORs as a table 
	fp_ors <- fp_ors %>%
		mutate_at(c("Est", "lci", "uci"), ~signif(., 3)) %>%
		mutate(estimate = paste0(Est, " (", lci, "-", uci, ")")) %>%
		dplyr::select(-Est, -lci, -uci, estimate_reduction = estimate)
	fp_int_ors <- fp_int_ors %>%
		mutate_at(c("Est", "lci", "uci"), ~signif(., 3)) %>%
		mutate(estimate = paste0(Est, " (", lci, "-", uci, ")")) %>%
		dplyr::select(-Est, -lci, -uci, estimate_recovery = estimate)
	
	fp_ors_full <- fp_ors %>%
		left_join(fp_int_ors, by = c("var", "outcome_name", "chopdata")) %>%
		dplyr::select(-starts_with("X")) %>%
		pivot_wider(names_from = "chopdata", values_from = starts_with("estimate")) %>%
		dplyr::select(-var)
	
	sort_order <- data.frame(plot_order) %>%
		mutate(rowSort=1:n())
	sort_table <- outcome_of_interest_namematch %>%
		mutate(rowN = 1:n()) %>%
		left_join(sort_order, by = c("rowN"="plot_order"))
	levels(fp_ors_full$outcome_name)[7] <- "Self-harm"
	#fp_ors_full$outcome_name <- factor(fp_ors_full$outcome_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])
	
	fp_ors_full <- fp_ors_full %>%
		left_join(sort_table, by = "outcome_name") %>%
		arrange(rowSort)  %>%
		dplyr::select(-outcome, -rowN, -rowSort)
	
	##
	write.csv(fp_ors_full, here::here("graphfiles", "FP_compare_data_exclusion.csv"))

# same lockdown start date - change adjustment period ---------------------
#### 3 weeks
pdf(file = here::here("graphfiles", paste0("SA_its_SA_march1_3wks", ".pdf")), width = 15, height = 7)
its_function(outcomes_vec = outcomes,
						 cutData = as.Date("2017-01-01"),
						 start_lockdown = as.Date("2020-03-08"),
						 lockdown_adjustment_period_wks = 3,
						 end_post_lockdown_period = as.Date("2020-07-31")
)
dev.off()
#### 5 weeks (as in paper)
pdf(file = here::here("graphfiles", paste0("SA_its_SA_march1_5wks", ".pdf")), width = 15, height = 7)
its_function(outcomes_vec = outcomes,
						 cutData = as.Date("2017-01-01"),
						 start_lockdown = as.Date("2020-03-08"),
						 lockdown_adjustment_period_wks = 5,
						 end_post_lockdown_period = as.Date("2020-07-31")
)
dev.off()
#### 7 weeks 
pdf(file = here::here("graphfiles", paste0("SA_its_SA_march1_7wks", ".pdf")), width = 15, height = 7)
its_function(outcomes_vec = outcomes,
						 cutData = as.Date("2017-01-01"),
						 start_lockdown = as.Date("2020-03-08"),
						 lockdown_adjustment_period_wks = 7,
						 end_post_lockdown_period = as.Date("2020-07-31")
)
dev.off()
# lockdown on March 23rd & change adjustment period ---------------------
#### 3 weeks
pdf(file = here::here("graphfiles", paste0("SA_its_SA_march23_3wks", ".pdf")), width = 15, height = 7)
its_function(outcomes_vec = outcomes,
						 cutData = as.Date("2017-01-01"),
						 start_lockdown = as.Date("2020-03-22"),
						 lockdown_adjustment_period_wks = 3,
						 end_post_lockdown_period = as.Date("2020-07-31")
)
dev.off()
#### 5 weeks (as in paper)
pdf(file = here::here("graphfiles", paste0("SA_its_SA_march23_5wks", ".pdf")), width = 15, height = 7)
its_function(outcomes_vec = outcomes,
						 cutData = as.Date("2017-01-01"),
						 start_lockdown = as.Date("2020-03-22"),
						 lockdown_adjustment_period_wks = 5,
						 end_post_lockdown_period = as.Date("2020-07-31")
)
dev.off()
#### 7 weeks 
pdf(file = here::here("graphfiles", paste0("SA_its_SA_march23_7wks", ".pdf")), width = 15, height = 7)
its_function(outcomes_vec = outcomes,
						 cutData = as.Date("2017-01-01"),
						 start_lockdown = as.Date("2020-03-22"),
						 lockdown_adjustment_period_wks = 7,
						 end_post_lockdown_period = as.Date("2020-07-31")
)
dev.off()

# Make Sensitivity analysis table about change in ORs for interaction term  ---------------------
all_files <- list.files(here::here("graphfiles"), pattern = "its_main_INTORs_")
all_files <- all_files[stringr::str_detect(all_files, "2017")]

SA_tab <- NULL; jj <- 1
for(ii in all_files){
	load_file <- read.csv(here::here("graphfiles", ii))
	start_Ldn <- substr(ii, 17,26)
	Ldn_length <- substr(ii, 27,28)
	var_name <- paste0("_", start_Ldn, Ldn_length)
	if(jj>1){
		load_file <- load_file %>%
			dplyr::select(-outcome_name) 
	}else{
		load_file <- load_file %>%
			dplyr::select(outcome_name, everything()) 
	}
	load_file <- load_file %>%
		dplyr::select(-var, -X) %>%
		mutate_if(is.numeric, ~signif(., 3)) %>% 
		mutate(summ = paste0(Est, " (", lci, "-", uci,")")) %>%
		dplyr::select(-Est, -lci, -uci)
	names(load_file)[names(load_file)=="summ"] <- var_name
	SA_tab <- SA_tab %>%
		bind_cols(load_file)
	jj <- jj+1
}
sort_order <- data.frame(plot_order) %>%
	mutate(rowSort=1:n())
sort_table <- outcome_of_interest_namematch %>%
	mutate(rowN = 1:n()) %>%
	left_join(sort_order, by = c("rowN"="plot_order"))
SA_tab$outcome_name <- factor(SA_tab$outcome_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])

SA_tab <- SA_tab %>%
	left_join(sort_table, by = "outcome_name") %>%
	arrange(rowSort)  %>%
	dplyr::select(-outcome, -rowN, -rowSort)
write.csv(SA_tab, file =  here::here("graphfiles/its_main_summ_INT_ORs.csv"))

# Make Sensitivity analysis table about change in ORs ---------------------
all_files <- list.files(here::here("graphfiles"), pattern = "its_main_ORs_")
all_files <- all_files[stringr::str_detect(all_files, "2017")]

SA_tab <- NULL; jj <- 1
for(ii in all_files){
	load_file <- read.csv(here::here("graphfiles", ii))
	start_Ldn <- substr(ii, 14,23)
	Ldn_length <- substr(ii, 24,25)
	var_name <- paste0("_", start_Ldn, Ldn_length)
	if(jj>1){
		load_file <- load_file %>%
			dplyr::select(-outcome_name) 
	}else{
		load_file <- load_file %>%
			dplyr::select(outcome_name, everything()) 
	}
	load_file <- load_file %>%
		dplyr::select(-var, -X) %>%
		mutate_if(is.numeric, ~signif(., 2)) %>% 
		mutate(summ = paste0(Est, " (", lci, "-", uci,")")) %>%
		dplyr::select(-Est, -lci, -uci)
	names(load_file)[names(load_file)=="summ"] <- var_name
	SA_tab <- SA_tab %>%
		bind_cols(load_file)
	jj <- jj+1
}
sort_order <- data.frame(plot_order) %>%
	mutate(rowSort=1:n())
sort_table <- outcome_of_interest_namematch %>%
	mutate(rowN = 1:n()) %>%
	left_join(sort_order, by = c("rowN"="plot_order"))
SA_tab$outcome_name <- factor(SA_tab$outcome_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])

SA_tab <- SA_tab %>%
	left_join(sort_table, by = "outcome_name") %>%
	arrange(rowSort)  %>%
	dplyr::select(-outcome, -rowN, -rowSort)
write.csv(SA_tab, file =  here::here("graphfiles/its_main_summ_ORs.csv"))
