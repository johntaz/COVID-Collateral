################################################################################
# Updated version of the code for the analysis in:
#
#   "Interrupted time series regression for the evaluation of public health 
#     interventions: a tutorial"
#   J. Lopez Bernal, S. Cummins, A. Gasparrini 
#   International Journal of Epidemiology - 2017
#   http://www.ag-myresearch.com/2017_lopezbernal_ije.html
#
# Update: 21 May 2020
# * an updated version of this code, compatible with future versions of the
#   software, is available at:
#   https://github.com/gasparrini/2017_lopezbernal_IJE_codedata
################################################################################

# Install packages required for the analysis (uncomment if needed)
#install.packages("lmtest") ; install.packages("Epi")
#install.packages("tsModel"); install.packages("vcd")

# load the packages
library(foreign) ; library(tsModel) ; library("lmtest") ; library("Epi")
library("splines") ; library("vcd")
library(here) ; library(dplyr) ; library(tidyr)
library(ggplot2); library(patchwork)
# read data from csv file
bkg_colour <- "gray99"

all_files <- list.files(here::here("data/"), pattern = "an_")
outcomes <- stringr::str_remove_all(all_files, c("an_|.csv"))

## this bit is all manual and shit for now
outcome_of_interest_namematch <- bind_cols("outcome" = outcomes, 
																					 "outcome_name" = sort(c("Alcohol", "Anxiety", "Asthma", "COPD", "Cerebrovascular Accident", "Depression", "Diabetes", "Feeding Disorders", "Heart Failure", "Myocardial Infarction", "OCD", "Self-harm", "Severe Mental Illness", "Transient Ischaemic Attacks", "Unstable Angina", "Venous Thromboembolism"))
)
# load data ---------------------------------------------------------------
for(ii in 1:length(outcomes)){
	load_file <- read.csv(here::here("data", paste0("an_", outcomes[ii], ".csv")))
	assign(outcomes[ii], load_file)
}

plot_name <- "ITS_attempt3_2019data_4wkLdn"
use_data_from <- as.Date("2019-01-01")

its_function <- function(outcomes_vec = outcomes,
												 cutData = as.Date("2018-01-01"), 
												 start_lockdown =   as.Date("2020-03-16"),
												 lockdown_adjustment_period_wks = 4,
												 end_post_lockdown_period = as.Date("2020-08-01")
												 ){
			
			xmas_dates <- c( ## 1 week with 25th Decemeber as a midpoint, for each of 2017, 18, & 19 
				seq.Date(as.Date("2017-12-22"), as.Date("2017-12-29"), by = "1 day"),
				seq.Date(as.Date("2018-12-22"), as.Date("2018-12-29"), by = "1 day"),
				seq.Date(as.Date("2019-12-22"), as.Date("2019-12-29"), by = "1 day"))
			
		plot_its <- function(outcome){
			data_frame_of_joy <- get(outcome)
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
			
			## model binomial 
			# Change in level + slope:
			### include interaction with time (centred at end of Lockdown adjustment period)
			# Included vars
			### include xmas alongside months to capture the rapid drop in that one xmas week
			# Calculate Pearson X2 gof/d.f. as dispersion parameter for std.errors (and predicted values)
			### Only do this from the final model that will be used for predictions 
			# And add in a year variable because there are some strange things going on -- see selfharm and MI
			
			
			ldn_centre <- data_frame_of_joy$time[min(which(data_frame_of_joy$lockdown == 1))]
			
			binom_model1 <- glm(as.matrix(cbind(numOutcome, numEligible)) ~ lockdown + time + I(time-ldn_centre):lockdown + as.factor(months) + xmas, family=binomial, data = filter(data_frame_of_joy, !is.na(lockdown)))
			ci.exp(binom_model1)
			binom_lagres <- lag(residuals(binom_model1)) %>% as.numeric()
			res1 <- residuals(binom_model1,type="deviance")
			#pacf(res1)
			
			binom_model2 <- glm(as.matrix(cbind(numOutcome, numEligible)) ~ lockdown + time + I(time-ldn_centre):lockdown + as.factor(months) + xmas + binom_lagres, family=binomial, data = filter(data_frame_of_joy, !is.na(lockdown)))
			ci.exp(binom_model2)
			summary.glm(binom_model2)
			
			## plot residuals
			#res2 <- residuals(binom_model2, type="deviance")
			#plot(res2,pch=19,cex=0.7,col=grey(0.6),
			#		 main="Residuals over time",ylab="Deviance residuals",xlab="Date")
			#abline(h=0,lty=2,lwd=2)
			#acf(res2)
			#pacf(res2)
			
			## calculate dispersion adjustment parameter -- https://online.stat.psu.edu/stat504/node/162/
			#Pearson Goodness-of-fit statistic
			pearson_gof <- sum(residuals(binom_model2, type = "pearson")^2)
			df <- binom_model2$df.residual
			deviance_adjustment <- pearson_gof/df
			
			missing_data_start <- min(which(is.na(data_frame_of_joy$lockdown)))
			missing_data_end <- max(which(is.na(data_frame_of_joy$lockdown)))
			missing_data_restart <- max(which(is.na(data_frame_of_joy$lockdown)))
			
			binom_lagres_timing <- bind_cols("time" = data_frame_of_joy$time[!is.na(data_frame_of_joy$lockdown)],
																			 "binom_lagres" = binom_lagres)
			
			#binom_lagres_missing <- c(binom_lagres[1:missing_data_start-1],
			#													rep(NA, missing_data_end - missing_data_start +1),
			#													binom_lagres[missing_data_start:length(binom_lagres)])
			outcome_pred <- data_frame_of_joy %>%
				left_join(binom_lagres_timing, by = "time") %>%
				mutate_at("binom_lagres", ~(. = 0)) 
		
			outcome_pred_zeroed <- data_frame_of_joy %>%
				left_join(binom_lagres_timing, by = "time") %>%
				mutate_at("binom_lagres", ~(. = 0)) %>%
				mutate_at("xmas", ~(. = 0)) %>%
				mutate_at("year", ~(. = 0)) %>% 
				mutate_at("months", ~(. = 9)) 
			
			pred1 <- predict(binom_model2, newdata = outcome_pred, se.fit = TRUE, interval="confidence", dispersion = deviance_adjustment)
				predicted_vals <- pred1$fit
				stbp <- pred1$se.fit
			pred0 <- predict(binom_model2, newdata = outcome_pred_zeroed, se.fit = TRUE, interval="confidence", dispersion = deviance_adjustment)
				predicted_vals_0 <- pred0$fit
				stbp0 <- pred0$se.fit
			
			outcome_pred_nointervention <- outcome_pred_zeroed %>%
				mutate_at("lockdown", ~(.=0))
			predicted_vals_nointervention <- predict(binom_model2, newdata = outcome_pred_nointervention) 
			
			## standard errors
			df_se <- bind_cols(stbp = stbp, stbp0 = stbp0, pred = predicted_vals, pred0 = predicted_vals_0, pred_noLdn = predicted_vals_nointervention) %>%
				mutate(
					#CIs
					upp = pred + (1.96*stbp),
					low = pred - (1.96*stbp),
					upp0 = pred0 + (1.96*stbp0),
					low0 = pred0 - (1.96*stbp0),
					# probline
					predicted_vals = exp(pred)/(1+exp(pred)),
					probline_noLdn = exp(pred_noLdn)/(1+exp(pred_noLdn)),
					probline_0 = exp(pred0)/(1+exp(pred0)),
					#
					uci = exp(upp)/(1+exp(upp)),
					lci = exp(low)/(1+exp(low)),
					#
					uci0 = exp(upp0)/(1+exp(upp0)),
					lci0 = exp(low0)/(1+exp(low0)) 
					)
			
			outcome_plot <- bind_cols(outcome_pred, df_se)
			
			## blank out "lockdown period"
			outcome_plot <- outcome_plot %>%
				mutate_at(vars("predicted_vals", "lci", "uci"), ~ifelse(weekPlot < as.Date("2020-03-30") & weekPlot >= as.Date("2020-03-16"), NA, .)) %>%
				mutate(var = outcome)
			
			## get text to put on plot 
			paramter_estimates <- as.data.frame(ci.exp(binom_model2))
			vals_to_print <- paramter_estimates %>%
				mutate(var = rownames(paramter_estimates)) %>%
				filter(var == "lockdown") %>%
			#	mutate_at(1:3, ~signif(., 2)) %>%
				mutate(var = outcome)
				#annotate("text", x = as.Date("2019-06-01"), y = plot_text_height, label= text_to_print)
		return(list(df_1 = outcome_plot, vals_to_print = vals_to_print))
		}
		
		# the plot ----------------------------------------------------------------
		main_plot_data <- NULL
		forest_plot_data <- NULL
		for(ii in 1:length(outcomes_vec)){
			main_plot_data <- main_plot_data %>%
				bind_rows(
					plot_its(outcomes_vec[ii])$df_1
				)
			forest_plot_data <- forest_plot_data %>%
				bind_rows(
					plot_its(outcomes_vec[ii])$vals_to_print
				)
		}
		
		main_plot_data <- main_plot_data %>%
			mutate(prop_consult = numOutcome/numEligible) %>%
			left_join(outcome_of_interest_namematch, by = c("var" = "outcome"))
			#mutate_at("var", ~stringr::str_replace(., "_", " ")) %>%
			#mutate_at("var", ~stringr::str_to_title(.)) %>%
			#mutate_at("var", ~ifelse(stringr::str_to_upper(.) %in% 
			#												 	c("OCD", "SMI", "VTE", "CBA", "HF", "TIA", "UA", "MI", "COPD"), 
			#												 stringr::str_to_upper(.), 
			#												 .)) %>%
			#mutate_at("var", ~ifelse(. == "CBA", "CVA", .))
			
		
		plot1 <- ggplot(filter(main_plot_data, weekPlot >= cutData), aes(x = weekPlot, y = prop_consult, group = outcome_name)) +
			#geom_point(col = "gray60", shape = 16) +
			# the data
			geom_line(col = "gray60") +
			# the probability if therer was no lockdwon
			#geom_line(aes(y = probline_noLdn), col = 2, lty = 2) +
			# probability with model zeroed (inc. std. error)
			#geom_line(aes(y = probline_0), col = 2, lty = 2) +
			#geom_ribbon(aes(ymin = lci0, ymax=uci0), fill = alpha(2,0.4), lty = 0) +
			# probability with model (inc. std. error)
			geom_line(aes(y = predicted_vals), col = 4, lty = 2) +
			geom_ribbon(aes(ymin = lci, ymax=uci), fill = alpha(4,0.4), lty = 0) +
			### format the plot
			facet_wrap(~outcome_name, scales = "free", ncol = 4) +
			geom_vline(xintercept = c(start_lockdown, 
																start_lockdown + (7*lockdown_adjustment_period_wks)), col = 1, lwd = 1) + # 2020-04-05 is first week/data After lockdown gap
			#geom_vline(xintercept = c(end_post_lockdown_period), col = 1, lwd = 0.5, lty = 2) + # 2020-04-05 is first week/data After lockdown gap
			labs(x = "Date", y = "Proportion of people consulting for outcome", title = "") + # stringr::str_to_title(outcome)) +
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
						panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3))) 
		#plot1
		
		# Forest plot of ORs ------------------------------------------------------
		## clean up the names
		forest_plot_df <- forest_plot_data %>%
			rename("Est" = "exp(Est.)", "lci" = "2.5%", "uci" = "97.5%") %>%
			left_join(outcome_of_interest_namematch, by = c("var" = "outcome"))
			
			# mutate_at("var", ~stringr::str_replace(., "_", " ")) %>%
			# mutate_at("var", ~stringr::str_to_title(.)) %>% 
			# mutate_at("var", ~ifelse(stringr::str_to_upper(.) %in% 
			# 												 	c("OCD", "SMI", "VTE", "CBA", "HF", "TIA", "UA", "MI", "COPD"), 
			# 												 stringr::str_to_upper(.), 
			# 												 .)) %>%
			# mutate_at("var", ~ifelse(. == "CBA", "CVA", .)) 
			
		
		# orders the factor by the size of the effect
		forest_plot_df$outcome_name <- factor(forest_plot_df$outcome_name, levels=unique(forest_plot_df$outcome_name[order(forest_plot_df$Est)]), ordered = T)
		
		fp <- ggplot(data=forest_plot_df, aes(x=outcome_name, y=Est, ymin=lci, ymax=uci)) +
			geom_point(size = 0.8, pch = 1) +
			geom_errorbar(width = 0.2) +
			#ylim(c(0,1)) + 
			geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
			coord_flip() +  # flip coordinates (puts labels on y axis)
			labs(x = "", y = "OR (95% CI)", title = "") + 
			theme_classic() +
			theme(axis.text.y = element_text(angle = 45, hjust = 1),
						legend.position = "top",
						plot.background = element_rect(fill = bkg_colour, colour =  NA),
						panel.background = element_rect(fill = bkg_colour, colour =  NA),
						legend.background = element_rect(fill = bkg_colour, colour = NA),
						strip.background = element_rect(fill = bkg_colour, colour =  NA),
						panel.grid.major = element_blank(),
						panel.grid.minor.x = element_blank(),
						panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
						panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3)))
		#fp
		
		# Export plot -------------------------------------------------------------
		layout = "
			AAAAAAAAB
			AAAAAAAAB
			AAAAAAAAB
			"
		plot1 + fp + 
			plot_layout(design = layout) +
			plot_annotation(tag_levels = 'A')
			#plot_annotation('Quantifying the reduction in GP consultations over lockdown', subtitle = 'An interrupted time series analysis',
			#								caption = "A - full model; B - estimated reduction in consultations during lockdown",
			#								tag_levels = 'A')
}

its_function(outcomes_vec = outcomes, 
						 cutData = as.Date("2019-01-01"),
						 start_lockdown =   as.Date("2020-03-16"),
						 lockdown_adjustment_period_wks =  4,
						 end_post_lockdown_period = as.Date("2020-07-31")
						 )
ggsave(here::here("graphfiles", paste0("its_attempt5_2019_data", ".pdf")), width = 12, height = 10)


# its_function(outcomes_vec = c("alcohol", "selfharm", "ua"), 
# 						 cutData = as.Date("2019-01-01"),
# 						 start_lockdown =   as.Date("2020-03-16"),
# 						 lockdown_adjustment_period_wks =  4,
# 						 end_post_lockdown_period = as.Date("2020-07-31")
# 						 )
# ggsave(here::here("graphfiles", paste0("its_attempt4_2019_data_subset", ".pdf")), width = 12, height = 4)
# 
# its_function(outcomes_vec = c("alcohol", "selfharm", "ua"), 
# 						 cutData = as.Date("2019-01-01"),
# 						 start_lockdown =   as.Date("2020-03-23"),
# 						 lockdown_adjustment_period_wks =  4,
# 						 end_post_lockdown_period = as.Date("2020-07-31")
# 						 )
# ggsave(here::here("graphfiles", paste0("its_attempt4_2019_data_subset_latestart", ".pdf")), width = 12, height = 4)
# 
# its_function(outcomes_vec = c("alcohol", "selfharm", "ua"), 
# 						 cutData = as.Date("2019-01-01"),
# 						 start_lockdown =   as.Date("2020-03-16"),
# 						 lockdown_adjustment_period_wks =  2,
# 						 end_post_lockdown_period = as.Date("2020-07-31")
# )
# ggsave(here::here("graphfiles", paste0("its_attempt4_2019_data_subset_shortadj", ".pdf")), width = 12, height = 4)
# 
# its_function(outcomes_vec = c("alcohol", "selfharm", "ua"), 
# 						 cutData = as.Date("2019-01-01"),
# 						 start_lockdown =   as.Date("2020-03-16"),
# 						 lockdown_adjustment_period_wks =  4,
# 						 end_post_lockdown_period = as.Date("2020-06-15")
# )
# ggsave(here::here("graphfiles", paste0("its_attempt4_2019_data_subset_endLdn", ".pdf")), width = 12, height = 4)
