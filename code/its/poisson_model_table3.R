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
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)
bkg_colour <- "gray99"

# import data -------------------------------------------------------------
all_files <- list.files(here::here("data/"), pattern = "an_")
outcomes <- stringr::str_remove_all(all_files, c("an_|.csv"))
outcome_of_interest_namematch <- bind_cols("outcome" = outcomes, 
																					 "outcome_name" = (c("Acute Alcohol-Related Event", "Anxiety", "Asthma exacerbations",  "Cerebrovascular Accident", "COPD",
																					 										"Depression", "Diabetes Emergencies", "Eating Disorders", 
																					 										"Heart Failure", "Myocardial Infarction", "OCD", "Self-harm", "Severe Mental Illness", "Transient Ischaemic Attacks", 
																					 										"Unstable Angina", "Venous Thromboembolism"))
)
plot_order <- c(7,1,2,6,8,11,12,13,4,14,9,10,15,16,3,5) ## plots the outcomes by disease system as defined by ICD-10 chapter (diabetes, alcohol, mental health, circulatory system, respiratory system)

for(ii in 1:length(outcomes)){
	load_file <- read.csv(here::here("data", paste0("an_", outcomes[ii], ".csv")))
	assign(outcomes[ii], load_file)
}

# function to generate data for table 3 one outcome at a time -----------------------------------
tab3_function <- function(outcome){
	df_outcome <- get(outcome)
	
	if(outcome == "selfharm"){cutData <- as.Date("2019-01-01")}else{cutData <- as.Date("2017-01-01")}
	
	#cutData = as.Date("2019-01-01")
	start_lockdown = as.Date("2020-03-09")
	lockdown_adjustment_period_wks = 3
	end_post_lockdown_period = as.Date("2020-08-01")
																		 
	xmas_dates <- c(
		seq.Date(as.Date("2017-12-22"), as.Date("2017-12-29"), by = "1 day"),
		seq.Date(as.Date("2018-12-22"), as.Date("2018-12-29"), by = "1 day"),
		seq.Date(as.Date("2019-12-22"), as.Date("2019-12-29"), by = "1 day"))
	
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
	
	# start of post-lockdown period
	ldn_centre <- df_outcome$time[min(which(df_outcome$lockdown == 1))]
	
	## model Poisson 
	po_model1 <- glm(numOutcome ~ offset(log(numEligible)) + lockdown + time + I(time-ldn_centre):lockdown + as.factor(months) , family=quasipoisson, data = filter(df_outcome, !is.na(lockdown)))
	# get lagged residuals
	lagres1 <- lag(residuals(po_model1))
	
	## full model with lagged residuals
	po_model2 <- glm(numOutcome ~ offset(log(numEligible)) + lockdown + time + I(time-ldn_centre):lockdown + as.factor(months)   + lagres1, family=quasipoisson, data = filter(df_outcome, !is.na(lockdown)))
	
		## adjust predicted values
		pearson_gof <- sum(residuals(po_model2, type = "pearson")^2)
		df <- po_model2$df.residual
		deviance_adjustment <- pearson_gof/df
		
		po_lagres_timing <- bind_cols("time" = df_outcome$time[!is.na(df_outcome$lockdown)],
																		 "lagres1" = lagres1)
		
		## data frame to predict values from 
		outcome_pred <- df_outcome %>%
			left_join(po_lagres_timing, by = "time") %>%
			mutate_at("lagres1", ~(. = 0))
		
		## predict values
		pred1 <- predict(po_model2, newdata = outcome_pred, se.fit = TRUE, interval="confidence", dispersion = deviance_adjustment)
		predicted_vals <- pred1$fit
		stbp <- pred1$se.fit
		
		## predict values if no lockdown 
		outcome_pred_nointervention <- outcome_pred %>%
			mutate_at("lockdown", ~(.=0))
		predicted_vals_nointervention <- predict(po_model2, newdata = outcome_pred_nointervention, se.fit = TRUE, dispersion = deviance_adjustment) 
		stbp_noLdn <- predicted_vals_nointervention$se.fit	
		predicted_vals_noLdn <- predicted_vals_nointervention$fit	
		
		## standard errors
		df_se <- bind_cols(stbp = stbp, 
											 pred = predicted_vals, 
											 stbp_noLdn = stbp_noLdn, 
											 pred_noLdn = predicted_vals_noLdn, 
											 denom = df_outcome$numEligible) %>%
			mutate(
				#CIs
				upp = pred + (1.96*stbp),
				low = pred - (1.96*stbp),
				upp_noLdn = pred_noLdn + (1.96*stbp_noLdn),
				low_noLdn = pred_noLdn - (1.96*stbp_noLdn),
				# probline
				predicted_vals = exp(pred)/denom,
				probline_noLdn = exp(pred_noLdn)/denom,
				#
				uci = exp(upp)/denom,
				lci = exp(low)/denom,
				uci_noLdn = exp(upp_noLdn)/denom,
				lci_noLdn = exp(low_noLdn)/denom
				)
		
		mo1_post_ldn <- start_lockdown + (lockdown_adjustment_period_wks*7) + 30
		mo3_post_ldn <- start_lockdown + (lockdown_adjustment_period_wks*7) + 92
		
		sigdig <- 2
		model_out <- signif(ci.exp(po_model2)[2,], sigdig)
		
		tab3_dates <- bind_cols("weekPlot" = df_outcome$weekPlot, df_se) %>%
			mutate(target_1mo = mo1_post_ldn,
						 target_3mo = mo3_post_ldn,
						 days2 = abs(target_1mo - weekPlot),
						 days3 = abs(target_3mo - weekPlot),
						 # estimated number of weekly ooutcomes with NO LOCKDOWN
						 col1 = paste0(prettyNum(probline_noLdn*1e6,big.mark=",",digits = 0, scientific=FALSE), 
						 							" (", prettyNum(lci_noLdn*1e6,big.mark=",",digits = 0, scientific=FALSE), 
						 							" - ", prettyNum(uci_noLdn*1e6,big.mark=",",digits = 0, scientific=FALSE),")"),
						 # estimated number of weekly ooutcomes with LOCKDOWN
						 col3 = paste0(prettyNum(predicted_vals*1e6,big.mark=",",digits = 0, scientific=FALSE), 
						 							" (", prettyNum(lci*1e6,big.mark=",",digits = 0, scientific=FALSE), 
						 							" - ", prettyNum(uci*1e6,big.mark=",",digits = 0, scientific=FALSE),")")
						 ) %>%
			## filter to post-lockdown data only
			filter(weekPlot >= start_lockdown+(lockdown_adjustment_period_wks*7)) %>%
			## calculate cumulative sum of predicted vals with/without lockdown 
			mutate(cumsum_ldn = cumsum(predicted_vals*1e6),
						 lci_cumsum_ldn = cumsum(lci*1e6),
						 uci_cumsum_ldn = cumsum(uci*1e6),
							cumsum_noLdn = cumsum(probline_noLdn*1e6),
							lci_cumsum_noLdn = cumsum(low_noLdn*1e6),
							uci_cumsum_noLdn = cumsum(upp_noLdn*1e6),
						 prettyNum(uci*1e6,big.mark=",",digits = 0, scientific=FALSE),
						 ## weekly difference in Lockdown vs No lockdown
						 col5 = prettyNum(signif((probline_noLdn*1e6) - (predicted_vals*1e6),3), big.mark=",", digits = 0, scientific=FALSE),
						 ## cumulative sum of Lockdown vs No lockdown
						 col6 = prettyNum(signif((cumsum_noLdn) - (cumsum_ldn),3), big.mark=",", digits = 0, scientific=FALSE)
			)  %>%
			## censor data if it is too small
			mutate_at(.vars = c("col5"), ~ifelse((probline_noLdn*1e6) - (predicted_vals*1e6) < 10,
																					 "<10", 
																					 ifelse((probline_noLdn*1e6) - (predicted_vals*1e6) < 100,
																					 			 "<100",
																					 			 .))
			) %>%
			mutate_at(.vars = c("col6"), ~ifelse((cumsum_noLdn) - (cumsum_ldn) < 10,
																					 "<10", 
																					 ifelse((cumsum_noLdn) - (cumsum_ldn) < 100,
																					 			 "<100",
																					 			 .))
			) %>%
			## only keep the data for 1 month and 2 months post lockdown
			filter(days2 == min(days2) | 
						 	days3 == min(days3)) 
		
		rate_diff <- tab3_dates %>% 
			mutate(rate_diff = (exp(pred)/denom) - (exp(pred_noLdn)/denom),
						 	chisq_stat = (exp(pred) - (((exp(pred)+exp(pred_noLdn))*denom)/(denom+denom)))^2 / (((exp(pred)+exp(pred_noLdn))*denom*denom)/(denom^2)),
							lci_rd = rate_diff - 1.96*(sqrt((rate_diff^2)/chisq_stat)),
						 	uci_rd = rate_diff + 1.96*(sqrt((rate_diff^2)/chisq_stat))
			) %>%
			select(rate_diff, lci_rd, uci_rd)
		
		
		tab3_fmt <- tab3_dates %>% 
			bind_cols(rate_diff) %>%
			mutate(outcome = outcome_of_interest_namematch$outcome_name[outcome_of_interest_namematch$outcome == outcome]) %>%
			select(outcome, weekPlot, starts_with("col")) %>%
			pivot_wider(values_from = starts_with("col")) %>%
			mutate_at("weekPlot", ~as.character(format.Date(., "%d-%b"))) %>%
			mutate_at("outcome", ~ifelse(row_number(.)==2, "", .))
	return(tab3_fmt) 
}

tab3 <- NULL
for(ii in plot_order){
	tab3 <- bind_rows(tab3,
										tab3_function(outcomes[ii]))
	tab3[nrow(tab3)+1,] <- ""
}
tab3
write.csv(tab3, file = here::here("graphfiles/table3.csv"), row.names = F)
