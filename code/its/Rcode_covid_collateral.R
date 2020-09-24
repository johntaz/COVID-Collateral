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

its_function <- function(outcomes_vec = outcomes,
												 cutData = as.Date("2018-01-01"), 
												 start_lockdown =   as.Date("2020-03-08"),
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
			
			binom_model1 <- glm(as.matrix(cbind(numOutcome, numEligible)) ~ lockdown + I(time-ldn_centre) + I(time-ldn_centre):lockdown + as.factor(months) , family=binomial, data = filter(data_frame_of_joy, !is.na(lockdown)))
			ci.exp(binom_model1)
			binom_lagres <- lag(residuals(binom_model1)) %>% as.numeric()
			res1 <- residuals(binom_model1,type="deviance")
			#pacf(res1)
			
			binom_model2 <- glm(as.matrix(cbind(numOutcome, numEligible)) ~ lockdown + I(time-ldn_centre) + I(time-ldn_centre):lockdown + as.factor(months)  + binom_lagres, family=binomial, data = filter(data_frame_of_joy, !is.na(lockdown)))
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
			
			outcome_pred_nointervention <- outcome_pred %>%
				mutate_at("lockdown", ~(.=0))
			pred_noLockdown <- predict(binom_model2, newdata = outcome_pred_nointervention, se.fit = TRUE, interval="confidence", dispersion = deviance_adjustment) 
				pred_noLdn <- pred_noLockdown$fit
				stbp_noLdn <- pred_noLockdown$se.fit
				
			## standard errors
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
				mutate(var = outcome)
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
			mutate(pc_consult = (numOutcome/numEligible)*100) %>%
			mutate_at(.vars = c("predicted_vals", "lci", "uci", "probline_noLdn", "uci_noLdn", "lci_noLdn", "probline_0", "lci0", "uci0"), 
								~.*100) %>%
			left_join(outcome_of_interest_namematch, by = c("var" = "outcome"))
		main_plot_data$outcome_name <- factor(main_plot_data$outcome_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])
		
		abline_min <- main_plot_data$weekPlot[min(which(is.na(main_plot_data$lockdown)))]-7
		abline_max <- main_plot_data$weekPlot[max(which(is.na(main_plot_data$lockdown)))+1]
		plot1 <- ggplot(filter(main_plot_data, weekPlot >= as.Date("2020-01-01")), aes(x = weekPlot, y = pc_consult, group = outcome_name)) +
			#geom_point(col = "gray60", shape = 16) +
			# the data
			geom_line(col = "gray60") +
			### the probability if therer was no lockdwon
			geom_line(data = filter(main_plot_data, weekPlot >= abline_min), aes(y = probline_noLdn), col = 2, lty = 2) +
			geom_ribbon(data = filter(main_plot_data, weekPlot >= abline_min), aes(ymin = lci_noLdn, ymax=uci_noLdn), fill = alpha(2,0.4), lty = 0) +
			### probability with model zeroed (inc. std. error)
			#geom_line(aes(y = probline_0), col = 2, lty = 2) +
			#geom_ribbon(aes(ymin = lci0, ymax=uci0), fill = alpha(2,0.4), lty = 0) +
			### probability with model (inc. std. error)
			geom_line(aes(y = predicted_vals), col = 4, lty = 2) +
			geom_ribbon(aes(ymin = lci, ymax=uci), fill = alpha(4,0.4), lty = 0) +
			### format the plot
			facet_wrap(~outcome_name, scales = "free", ncol = 4) +
			scale_x_date(breaks = "1 month", date_labels = "%b") +
			geom_vline(xintercept = c(abline_min, 
																abline_max), col = 1, lwd = 1) + # 2020-04-05 is first week/data After lockdown gap
			#geom_vline(xintercept = c(end_post_lockdown_period), col = 1, lwd = 0.5, lty = 2) + # 2020-04-05 is first week/data After lockdown gap
			labs(x = "Date", y = "% of people consulting for outcome", title = "") + # stringr::str_to_title(outcome)) +
			theme_classic() +
			theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
						legend.position = "top",
						plot.background = element_rect(fill = bkg_colour, colour =  NA),
						panel.background = element_rect(fill = bkg_colour, colour =  NA),
						legend.background = element_rect(fill = bkg_colour, colour = NA),
						strip.background = element_rect(fill = bkg_colour, colour =  NA),
						strip.text = element_text(size = 10, hjust = 0),
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
		
		# orders the factor by the size of the effect
		forest_plot_df$outcome_name <- factor(forest_plot_df$outcome_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])
		
		fp <- ggplot(data=forest_plot_df, aes(x=outcome_name, y=Est, ymin=lci, ymax=uci)) +
			geom_point(size = 0.2, pch = 1) +
			geom_errorbar(width = 0.2) +
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
						panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3))) +
			scale_x_discrete(limits = rev(levels(as.factor(forest_plot_df$outcome_name))))
		#fp
		
		# Export plot -------------------------------------------------------------
		layout = "
			AAAAAAAAB
			AAAAAAAAB
			AAAAAAAAB
			"
		plot1 + fp + 
			plot_layout(design = layout) 
}

pdf(file = here::here("graphfiles", paste0("its_attempt7_SA_2019_data_5wk_09-24", ".pdf")), width = 14, height = 7)
its_function(outcomes_vec = outcomes,
						 cutData = as.Date("2019-01-01"),
						 start_lockdown = as.Date("2020-03-08"),
						 lockdown_adjustment_period_wks = 5,
						 end_post_lockdown_period = as.Date("2020-07-31")
						 )
dev.off()
#ggsave(here::here("graphfiles", paste0("its_attempt7_2019_data_SA", ".pdf")), width = 14, height = 7)
