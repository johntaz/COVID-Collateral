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
outcome <- "depression"

data_frame_of_joy <- get(outcome)
xmas_dates <- c(
	seq.Date(as.Date("2017-12-22"), as.Date("2017-12-29"), by = "1 day"),
	seq.Date(as.Date("2018-12-22"), as.Date("2018-12-29"), by = "1 day"),
	seq.Date(as.Date("2019-12-22"), as.Date("2019-12-29"), by = "1 day"))
data_frame_of_joy <- data_frame_of_joy %>%
	filter(stratifier == "overall") %>%
	select(-stratifier, -category) %>%
	mutate(weekPlot = (time*7) + as.Date("2017-01-01")) %>%
	mutate(months = as.numeric(format.Date(weekPlot, "%m"))) %>%
	mutate(pre_lockdown = ifelse(weekPlot < as.Date("2020-03-16"), 1 , 0),
				 #post_lockdown = ifelse(weekPlot > as.Date("2020-03-30") & weekPlot < as.Date("2020-05-10"), 1, 0))
				 post_lockdown = ifelse(weekPlot >= as.Date("2020-03-30"), 1, 0)) %>%
	mutate_at("lockdown", ~ifelse(weekPlot %in% seq.Date(as.Date("2020-03-16"), as.Date("2020-03-30"), "1 day"), NA, .)) %>%
	mutate(xmas = ifelse(weekPlot %in% xmas_dates, 1, 0)) 


# choose a model  ---------------------------------------------------------
## model Poisson 
po_model1 <- glm(numOutcome ~ offset(log(numEligible)) + lockdown + time + months, family=quasipoisson, data = filter(data_frame_of_joy, !is.na(lockdown)))
lagres1 <- lag(residuals(po_model1))
po_model2 <- glm(numOutcome ~ offset(log(numEligible)) + lockdown + time + months + lagres1, family=quasipoisson, data = filter(data_frame_of_joy, !is.na(lockdown)))

## model Poisson with harmonic term for months
harm_model1 <- glm(numOutcome ~ offset(log(numEligible)) + lockdown + time + harmonic(months,2,12), family=quasipoisson, data = filter(data_frame_of_joy, !is.na(lockdown)))
lagres1 <- lag(residuals(po_model1))
harm_model2 <- glm(numOutcome ~ offset(log(numEligible)) + lockdown + time + harmonic(months,2,12) + lagres1, family=quasipoisson, data = filter(data_frame_of_joy, !is.na(lockdown)))




final_model <- harm_model1
lagres1 <- lagged_residuals


	missing_data_start <- min(which(is.na(data_frame_of_joy$lockdown)))
	missing_data_end <- max(which(is.na(data_frame_of_joy$lockdown)))
	
	binom_lagres_missing <- c(lagged_residuals[1:missing_data_start-1],
														rep(NA, missing_data_end - missing_data_start +1),
														lagged_residuals[missing_data_start:length(lagged_residuals)])
	
	
	### put option in to turn on or off variables for the predicted vars
	outcome_pred <- data_frame_of_joy %>%
		bind_cols(lagres1 = binom_lagres_missing) %>%
		mutate_at("lagres1", ~(. = 0)) %>%
		mutate_at("xmas", ~(. = 0)) %>%
		mutate_at("months", ~(. = 6)) 
	
	pred1 <- predict(final_model, newdata = outcome_pred, se.fit = TRUE, interval="confidence")
	predicted_vals <- pred1$fit
	stbp <- pred1$se.fit
	
	outcome_pred_nointervention <- outcome_pred %>%
		mutate_at("lockdown", ~(.=0))
	predicted_vals_nointervention <- predict(final_model, newdata = outcome_pred_nointervention) 
	
	## standard errors
	df_se <- bind_cols(stbp = stbp, pred = predicted_vals, pred_noLdn = predicted_vals_nointervention, denom = data_frame_of_joy$numEligible) %>%
		mutate(
			#CIs
			upp = pred + (1.96*stbp),
			low = pred - (1.96*stbp),
			# probline
			predicted_vals = exp(pred)/denom*10^5,
			probline_noLdn = exp(pred)/denom*10^5,
			#
			uci = exp(upp)/denom*10^5,
			lci = exp(low)/denom*10^5)
	
	outcome_plot <- bind_cols(outcome_pred, df_se)
	
	## blank out "lockdown period"
	outcome_plot <- outcome_plot %>%
		mutate_at(vars("predicted_vals", "lci", "uci"), ~ifelse(weekPlot < as.Date("2020-03-30") & weekPlot >= as.Date("2020-03-16"), NA, .)) %>%
		mutate(var = outcome)
	
	## get text to put on plot 
	paramter_estimates <- as.data.frame(ci.exp(final_model))
	vals_to_print <- paramter_estimates %>%
		mutate(var = rownames(paramter_estimates)) %>%
		filter(var == "lockdown") %>%
		mutate(var = outcome)
	#
	df_1 <- outcome_plot %>%
		mutate(prop_consult = numOutcome/numEligible,
					 rate = numOutcome/numEligible*10^5)
	
	# the plot ----------------------------------------------------------------
	plot1 <- ggplot(filter(df_1, weekPlot > as.Date("2019-01-01")), aes(x = weekPlot, y = rate, group = var)) +
		geom_point(col = "gray60", shape = 16) +
		#geom_line(col = "gray60", lty = 3) +
		geom_line(aes(y = predicted_vals), col = 4, lty = 2) +
		geom_line(aes(y = probline_noLdn), col = 4, lty = 2) +
		geom_ribbon(aes(ymin = lci, ymax=uci), fill = alpha(4,0.4), lty = 0) +
		geom_vline(xintercept = c(as.Date("2020-03-16"), as.Date("2020-04-05")), col = 1, lwd = 1) + # 2020-04-05 is first week/data After lockdown gap
		#geom_vline(xintercept = as.Date("2020-05-10"), col = 1, lty = 4, lwd = 1.2) +
		facet_wrap(~var, scales = "free") +
		labs(x = "Date", y = "Rate (per 100,000) ", title = "") + # stringr::str_to_title(outcome)) +
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
	
ggsave(here::here("graphfiles/ITS_poisson.pdf"), width = 10, height = 8)
	
summary(po_model2)
ci.exp(po_model2)
