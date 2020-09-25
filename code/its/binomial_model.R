library(foreign) ; library(tsModel) ; library("lmtest") ; library("Epi")
library("splines") ; library("vcd")
library(here) ; library(dplyr) ; library(tidyr)
library(ggplot2); library(patchwork)
library(multcomp)
# read data from csv file
here::here()
bkg_colour <- "gray99"

# based on Anthony code ---------------------------------------------------
alcohol <- read.csv(here::here("data/an_alcohol.csv"))
anxiety <- read.csv(here::here("data/an_anxiety.csv"))
cba <- read.csv(here::here("data/an_cba.csv"))
depression <- read.csv(here::here("data/an_depression.csv"))
diabetes <- read.csv(here::here("data/an_diabetes.csv"))
feedingdisorders <- read.csv(here::here("data/an_feedingdisorders.csv"))
hf <- read.csv(here::here("data/an_hf.csv"))
mi <- read.csv(here::here("data/an_mi.csv"))
ocd <- read.csv(here::here("data/an_ocd.csv"))
selfharm <- read.csv(here::here("data/an_selfharm.csv"))
smi <- read.csv(here::here("data/an_smi.csv"))
tia <- read.csv(here::here("data/an_tia.csv"))
ua <- read.csv(here::here("data/an_ua.csv"))
vte <- read.csv(here::here("data/an_vte.csv"))


## choose an outcome
outcome <- "alcohol"

data_frame_of_joy <- get(outcome)
xmas_dates <- c(
	seq.Date(as.Date("2017-12-22"), as.Date("2017-12-29"), by = "1 day"),
	seq.Date(as.Date("2018-12-22"), as.Date("2018-12-29"), by = "1 day"),
	seq.Date(as.Date("2019-12-22"), as.Date("2019-12-29"), by = "1 day"))
data_frame_of_joy <- data_frame_of_joy %>%
	filter(stratifier == "overall") %>%
	dplyr::select(-stratifier, -category) %>%
	mutate(weekPlot = (time*7) + as.Date("2017-01-01")) %>%
	mutate(months = as.numeric(format.Date(weekPlot, "%m"))) %>%
	mutate(pre_lockdown = ifelse(weekPlot < as.Date("2020-03-16"), 1 , 0),
				 #post_lockdown = ifelse(weekPlot > as.Date("2020-03-30") & weekPlot < as.Date("2020-05-10"), 1, 0))
				 post_lockdown = ifelse(weekPlot >= as.Date("2020-03-30"), 1, 0)) %>%
	mutate_at("lockdown", ~ifelse(weekPlot %in% seq.Date(as.Date("2020-03-16"), as.Date("2020-03-30"), "1 day"), NA, .)) %>%
	mutate(xmas = ifelse(weekPlot %in% xmas_dates, 1, 0)) 

# the data ----------------------------------------------------------------
ggplot(data_frame_of_joy, aes(x = weekPlot, y = numEligible)) +
	geom_line() +
	labs(title = "Denominator population") + 
	theme_classic()
ggplot(data_frame_of_joy, aes(x = weekPlot, y = numOutcome)) +
	geom_line() +
	labs(title = "Count outcomes population") + 
	theme_classic()

# the outcome -------------------------------------------------------------
head(data_frame_of_joy)
ggplot(data_frame_of_joy, aes(x = weekPlot, y = numOutcome/numEligible)) +
	geom_line() +
	scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
	labs(title = "Denominator population") + 
	theme_classic()

# choose a model  ---------------------------------------------------------
## model Binomial 
head(data_frame_of_joy)
## include an intercation with time centred at lockdown introduction
lockdown_intro_t <- data_frame_of_joy$time[min(which(data_frame_of_joy$lockdown==1))]
binom_model1 <- glm(as.matrix(cbind(numOutcome, numEligible)) ~ lockdown + time + I(time-lockdown_intro_t):lockdown + as.factor(months) + xmas, family=binomial, data = filter(data_frame_of_joy, !is.na(lockdown)))
summary.glm(binom_model1)
lagres1 <- lag(residuals(binom_model1))


# do I want one or two lockdown vars? -------------------------------------
binom_model1 <- glm(as.matrix(cbind(numOutcome, numEligible)) ~ lockdown + time + I(time-lockdown_intro_t):lockdown + as.factor(months) + xmas, family=binomial, data = filter(data_frame_of_joy, !is.na(lockdown)))
binom_model1_2ldns <- glm(as.matrix(cbind(numOutcome, numEligible)) ~ pre_lockdown + post_lockdown + time + I(time-lockdown_intro_t):post_lockdown + as.factor(months) + xmas, family=binomial, data = data_frame_of_joy)
ci.exp(binom_model1)
ci.exp(binom_model1_2ldns)

## with one lockdown var ### WORK IN PROGRESS
coefeq <- matrix(data=0, nrow=1, ncol=length(binom_model1$coefficients))
colnames(coefeq) <- names(binom_model1$coefficients)
coefeq[1,"(Intercept)"] <- 1
coefeq[1,"lockdown"] <- 1
coefeq[1,"time"] <- 170
coefeq[1,"lockdown:I(time - lockdown_intro_t)"] <- 0
mod.lh <- glht(binom_model1, linfct = coefeq)
test <- confint(mod.lh)
exp(-9.0783)

## plot acf
res2 <- residuals(binom_model1)
acf(res2)
binom_model2 <- glm(as.matrix(cbind(numOutcome, numEligible)) ~ lockdown + time + I(time-lockdown_intro_t):lockdown + as.factor(months) + lagres1  + xmas, family=binomial, data = filter(data_frame_of_joy, !is.na(lockdown)))
summary.glm(binom_model2)

## get Pearson Goodness-of-fit statistic
pearson_gof <- sum(residuals(binom_model2, type = "pearson")^2)
df <- binom_model2$df.residual
deviance_adjustment <- pearson_gof/df

## decided against harmocnic because it acheives the same thing but it is easier to explain months adjustment as factor
final_model <- binom_model2
lagged_residuals <- lag(residuals(binom_model1))


missing_data_start <- min(which(is.na(data_frame_of_joy$lockdown)))
missing_data_end <- max(which(is.na(data_frame_of_joy$lockdown)))

binom_lagres_missing <- c(lagged_residuals[1:missing_data_start-1],
													rep(NA, missing_data_end - missing_data_start +1),
													lagged_residuals[missing_data_start:length(lagged_residuals)])


### put option in to turn on or off variables for the predicted vars
outcome_pred <- data_frame_of_joy %>%
	bind_cols(lagres1 = binom_lagres_missing) #%>%
	#mutate_at("lagres1", ~(. = 0)) %>%
	#mutate_at("xmas", ~(. = 0)) %>%
	#mutate_at("months", ~(. = 4)) 

pred1 <- predict(final_model, newdata = outcome_pred, se.fit = TRUE, interval="confidence")
pred1_overd <- predict(final_model, newdata = outcome_pred, se.fit = TRUE, interval="confidence", dispersion = deviance_adjustment)
predicted_vals <- pred1$fit ## predicted vals shouldn't change but std. errors. should
stbp <- pred1$se.fit
stbp_overd <- pred1_overd$se.fit

outcome_pred_nointervention <- outcome_pred %>%
	mutate_at("lockdown", ~(.=0))
predicted_vals_nointervention <- predict(final_model, newdata = outcome_pred_nointervention) 

## standard errors
df_se <- bind_cols(stbp = stbp_overd, pred = predicted_vals, pred_noLdn = predicted_vals_nointervention) %>%
	mutate(
		#CIs
		upp = pred + (1.96*stbp),
		low = pred - (1.96*stbp),
		# probline
		predicted_vals = exp(pred)/(1+exp(pred)),
		probline_noLdn = exp(pred_noLdn)/(1+exp(pred_noLdn)),
		#
		uci = exp(upp)/(1+exp(upp)),
		lci = exp(low)/(1+exp(low)))
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
	mutate(prop_consult = numOutcome/numEligible)

# the plot ----------------------------------------------------------------
vals_to_print <- signif(vals_to_print[1:3],3)

text_to_print <- paste0("OR: ", vals_to_print[1], " (", vals_to_print[2], " - ", vals_to_print[3],")")
plot_text_height <- max(df_1$prop_consult[outcome_plot$weekPlot > as.Date ("2019-01-01")], na.rm = T)/2

plot1 <- ggplot(filter(df_1, weekPlot > as.Date("2020-01-01")), aes(x = weekPlot, y = prop_consult, group = var)) +
	geom_point(col = "gray60", shape = 16) +
	#geom_line(col = "gray60", lty = 3) +
	geom_line(aes(y = predicted_vals), col = 4, lty = 2) +
	geom_line(aes(y = probline_noLdn), col = 4, lty = 2) +
	geom_ribbon(aes(ymin = lci, ymax=uci), fill = alpha(4,0.4), lty = 0) +
	geom_vline(xintercept = c(as.Date("2020-03-16"), as.Date("2020-04-05")), col = 1, lwd = 1) + # 2020-04-05 is first week/data After lockdown gap
	#geom_vline(xintercept = as.Date("2020-05-10"), col = 1, lty = 4, lwd = 1.2) +
	facet_wrap(~var, scales = "free") +
	labs(x = "Date", y = "Proportion of consultation ", title = "") + # stringr::str_to_title(outcome)) +
	theme_classic() +
	annotate("text", x = as.Date("2019-06-01"), y = plot_text_height, label= text_to_print) +
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
#ggsave(here::here("graphfiles/ITS_binom.pdf"), width = 10, height = 8)


summary(final_model)