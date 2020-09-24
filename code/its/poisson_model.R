library(foreign) ; library(tsModel) ; library("lmtest") ; library("Epi")
library("splines") ; library("vcd")
library(here) ; library(dplyr) ; library(tidyr)
library(ggplot2); library(patchwork)
# read data from csv file
here::here()
bkg_colour <- "gray99"

# based on Anthony code ---------------------------------------------------
anxiety <- read.csv(here::here("data/an_anxiety.csv"))
depression <- read.csv(here::here("data/an_depression.csv"))
diabetes <- read.csv(here::here("data/an_diabetes.csv"))
heart_failure <- read.csv(here::here("data/an_hf.csv"))
OCD <- read.csv(here::here("data/an_ocd.csv"))
myocardial_infarction <- read.csv(here::here("data/an_mi.csv"))
alcohol <- read.csv(here::here("data/an_alcohol.csv"))

## choose an outcome
outcome <- "diabetes"
all_files <- list.files(here::here("data/"), pattern = "an_")
outcomes <- stringr::str_remove_all(all_files, c("an_|.csv"))

outcome_of_interest_namematch <- bind_cols("outcome" = outcomes, 
																					 "outcome_name" = (c("Acute Alcohol Abuse", "Anxiety", "Asthma exacerbations",  "Cerebrovascular Accident", "COPD",
																					 										"Depression", "Diabetes emergencies", "Feeding Disorders", 
																					 										"Heart Failure", "Myocardial Infarction", "Obsessive Compulsive Disorder", "Self-harm", "Severe Mental Illness", "Transient Ischaemic Attacks", 
																					 										"Unstable Angina", "Venous Thromboembolism"))
)

plot_order <- c(7,1,2,6,8,11,12,13,4,9,10,14,15,16,3,5)

# load data ---------------------------------------------------------------
for(ii in 1:length(outcomes)){
	load_file <- read.csv(here::here("data", paste0("an_", outcomes[ii], ".csv")))
	assign(outcomes[ii], load_file)
}

tab3_function <- function(outcome){
	data_frame_of_joy <- get(outcome)
	
	cutData = as.Date("2019-01-01")
	start_lockdown =   as.Date("2020-03-16")
	lockdown_adjustment_period_wks = 4
	end_post_lockdown_period = as.Date("2020-08-01")
																		 
	xmas_dates <- c(
		seq.Date(as.Date("2017-12-22"), as.Date("2017-12-29"), by = "1 day"),
		seq.Date(as.Date("2018-12-22"), as.Date("2018-12-29"), by = "1 day"),
		seq.Date(as.Date("2019-12-22"), as.Date("2019-12-29"), by = "1 day"))
	
		data_frame_of_joy <- data_frame_of_joy %>%
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
	
	
	# choose a model  ---------------------------------------------------------
	ldn_centre <- data_frame_of_joy$time[min(which(data_frame_of_joy$lockdown == 1))]
	
	## model Poisson 
	po_model1 <- glm(numOutcome ~ offset(log(numEligible)) + lockdown + time + I(time-ldn_centre):lockdown + as.factor(months) , family=quasipoisson, data = filter(data_frame_of_joy, !is.na(lockdown)))
	lagres1 <- lag(residuals(po_model1))
	
	po_model2 <- glm(numOutcome ~ offset(log(numEligible)) + lockdown + time + I(time-ldn_centre):lockdown + as.factor(months)   + lagres1, family=quasipoisson, data = filter(data_frame_of_joy, !is.na(lockdown)))
	
		### put option in to turn on or off variables for the predicted vars
		pearson_gof <- sum(residuals(po_model2, type = "pearson")^2)
		df <- po_model2$df.residual
		deviance_adjustment <- pearson_gof/df
		
		po_lagres_timing <- bind_cols("time" = data_frame_of_joy$time[!is.na(data_frame_of_joy$lockdown)],
																		 "lagres1" = lagres1)
		
		outcome_pred <- data_frame_of_joy %>%
			left_join(po_lagres_timing, by = "time") %>%
			mutate_at("lagres1", ~(. = 0))# %>%
			#mutate_at("xmas", ~(. = 0)) %>%
			#mutate_at("months", ~(. = 6)) 
		
		pred1 <- predict(po_model2, newdata = outcome_pred, se.fit = TRUE, interval="confidence", dispersion = deviance_adjustment)
		predicted_vals <- pred1$fit
		stbp <- pred1$se.fit
		
		outcome_pred_nointervention <- outcome_pred %>%
			mutate_at("lockdown", ~(.=0))
		predicted_vals_nointervention <- predict(po_model2, newdata = outcome_pred_nointervention, se.fit = TRUE, dispersion = deviance_adjustment) 
		stbp_noLdn <- predicted_vals_nointervention$se.fit	
		predicted_vals_noLdn <- predicted_vals_nointervention$fit	
		
		stbp_diff <- sqrt(stbp^2 + stbp_noLdn^2)
		
		## standard errors
		df_se <- bind_cols(stbp = stbp, 
											 pred = predicted_vals, 
											 stbp_noLdn = stbp_noLdn, 
											 pred_noLdn = predicted_vals_noLdn, 
											 stbp_diff = stbp_diff,
											 denom = data_frame_of_joy$numEligible) %>%
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
				lci_noLdn = exp(low_noLdn)/denom,
				#
				pred_difference_log = pred-pred_noLdn,
				upp_diff = pred_difference_log + (1.96*stbp_diff),
				low_diff = pred_difference_log - (1.96*stbp_diff),
				#
				predicted_diff = exp(pred_difference_log)/denom,
				uci_diff = exp(upp_diff)/denom,
				lci_diff = exp(low_diff)/denom
				)
		
		mo1_post_ldn <- start_lockdown + (lockdown_adjustment_period_wks*7) + 30
		mo3_post_ldn <- start_lockdown + (lockdown_adjustment_period_wks*7) + 92
		
		sigdig <- 2
		model_out <- signif(ci.exp(po_model2)[2,], sigdig)
		
		tab3_dates <- bind_cols("weekPlot" = data_frame_of_joy$weekPlot, df_se) %>%
			mutate(#target_1wk = wk1_post_ldn,
						 target_1mo = mo1_post_ldn,
						 target_3mo = mo3_post_ldn,
						 #days1 = abs(target_1wk - weekPlot),
						 days2 = abs(target_1mo - weekPlot),
						 days3 = abs(target_3mo - weekPlot),
						 #
						 col1 = paste0(prettyNum(probline_noLdn*1e6,big.mark=",",digits = 0, scientific=FALSE), 
						 							" (", prettyNum(lci_noLdn*1e6,big.mark=",",digits = 0, scientific=FALSE), 
						 							" - ", prettyNum(uci_noLdn*1e6,big.mark=",",digits = 0, scientific=FALSE),")"),
						 col2 = paste0(signif(probline_noLdn*100,sigdig), "% (", signif(lci_noLdn*100,sigdig), " - ", signif(uci_noLdn*100,sigdig),"%)"),
						 col3 = paste0(prettyNum(predicted_vals*1e6,big.mark=",",digits = 0, scientific=FALSE), 
						 							" (", prettyNum(lci*1e6,big.mark=",",digits = 0, scientific=FALSE), 
						 							" - ", prettyNum(uci*1e6,big.mark=",",digits = 0, scientific=FALSE),")")
						 ) %>%
			filter(weekPlot >= start_lockdown+(lockdown_adjustment_period_wks*7)) %>%
			mutate(cumsum_ldn = cumsum(predicted_vals*1e6),
						 lci_cumsum_ldn = cumsum(lci*1e6),
						 uci_cumsum_ldn = cumsum(uci*1e6),
							cumsum_noLdn = cumsum(probline_noLdn*1e6),
							lci_cumsum_noLdn = cumsum(low_noLdn*1e6),
							uci_cumsum_noLdn = cumsum(upp_noLdn*1e6),
						 prettyNum(uci*1e6,big.mark=",",digits = 0, scientific=FALSE),
						 col5 = prettyNum(signif((probline_noLdn*1e6) - (predicted_vals*1e6),3), big.mark=",", digits = 0, scientific=FALSE),
						 col6 = prettyNum(signif((cumsum_noLdn) - (cumsum_ldn),3), big.mark=",", digits = 0, scientific=FALSE)
			)  %>%
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
write.csv(tab3, file = here::here("graphfiles/table3_09-22.csv"), row.names = F)

	outcome_plot <- bind_cols(outcome_pred, df_se)
	
	## blank out "lockdown period"
	outcome_plot <- outcome_plot %>%
		mutate_at(vars("predicted_vals", "lci", "uci"), ~ifelse(weekPlot < as.Date("2020-03-30") & weekPlot >= as.Date("2020-03-16"), NA, .)) %>%
		mutate(var = outcome)
	
	## get text to put on plot 
	paramter_estimates <- as.data.frame(ci.exp(po_model2))
	vals_to_print <- paramter_estimates %>%
		mutate(var = rownames(paramter_estimates)) %>%
		filter(var == "lockdown") %>%
		mutate(var = outcome)
	#
	df_1 <- outcome_plot %>%
		mutate(prop_consult = numOutcome/numEligible,
					 rate = numOutcome/numEligible)
	
	# the plot ----------------------------------------------------------------
	plot1 <- ggplot(filter(df_1, weekPlot > as.Date("2019-01-01")), aes(x = weekPlot, y = rate, group = var)) +
		geom_point(col = "gray60", shape = 16) +
		#geom_line(col = "gray60", lty = 3) +
		geom_line(aes(y = predicted_vals), col = 4, lty = 2) +
		geom_line(aes(y = probline_noLdn), col = 4, lty = 2) +
		geom_ribbon(aes(ymin = lci, ymax=uci), fill = alpha(4,0.4), lty = 0) +
		geom_vline(xintercept = c(start_lockdown, 
															start_lockdown + (7*lockdown_adjustment_period_wks)), col = 1, lwd = 1) + # 2020-04-05 is first week/data After lockdown gap
		geom_vline(xintercept = c(end_post_lockdown_period), col = 1, lwd = 0.5, lty = 2) + # 2020-04-05 is first week/data After lockdown gap
		labs(x = "Date", y = "Proportion of people consulting for outcome", title = "") + # stringr::str_to_title(outcome)) +
		facet_wrap(~var, scales = "free") +
		labs(x = "Date", y = "Rate (per 100) ", title = "") + # stringr::str_to_title(outcome)) +
		theme_classic() +
		theme(axis.text.x = element_text(angle = 60, hjust = 1),
					legend.position = "top",
					plot.background = element_rect(fill = bkg_colour, colour =  NA),
					panel.background = element_rect(fill = bkg_colour, colour =  NA),
					legend.background = element_rect(fill = bkg_colour, colour = NA),
					strip.background = element_rect(fill = bkg_colour, colour =  NA),
					strip.text = element_text(hjust = 0),
					panel.grid.major = element_blank(),
					panel.grid.minor.x = element_blank(),
					panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
					panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3))) #+
	plot1
	
#ggsave(here::here("graphfiles/ITS_poisson.pdf"), width = 10, height = 8)
	
summary(po_model2)
ci.exp(po_model2)

