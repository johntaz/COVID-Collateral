###########################
## Process data for shiny app
## basically move and process data ready for app deployment
library(dplyr)

# load data ---------------------------------------------------------------
data_files <- list.files(here::here("../../../data/"), pattern = "*.csv")
## append all datafiles
shiny_file <- NULL
for(ii in 1:length(data_files)){
	load_file <- read.csv(paste0(here::here("../../../data/"), data_files[ii]), stringsAsFactors = F)
	outcome_name <- stringr::str_remove_all(data_files[ii], "an_|.csv")
	load_file$outcome <- outcome_name
	
	shiny_file <- bind_rows(shiny_file, load_file)
	#assign(paste0("df_", outcome_name), load_file)
}


## delete old file types that had prooportion
shiny_file <- shiny_file %>%
	filter(!is.na(numOutcome)) 

## create proportion and a proper R object for date
shiny_file <- shiny_file %>% 
	mutate(model_out = (numOutcome/numEligible)*100) %>%
	#mutate_at("numOutcome", ~ifelse(numOutcome == 5, 0, .)) %>%
	mutate(weekPlot = (time*7) + as.Date("2017-01-01")) 

# build main database to plot that groups everything ----------------------
stratifiers <- stringr::str_to_title(c("gender", "age", "region", "ethnicity"))
strats <- unique(shiny_file$stratifier)
strats <- strats[strats!="overall"]

ethnicity_cats <- c("White", 
										"South Asian", 
										"Black", 
										"Other/Mixed", 
										"Other/Mixed", 
										"Missing")
gender_cats <- c("Female", "Male")
region_cats <- c("North East" ,
								 "North West" ,
								 "Yorkshire & the Humber" ,
								 "East Midlands" ,
								 "West Midlands" ,
								 "East of England" ,
								 "South West" ,
								 "South Central" ,
								 "London" ,
								 "South East Coast" ,
								 "Northern Ireland",
								 "Missing")

age_cats <- c(
						"11 - 30",
						"11 - 30",
						"31 - 50",
						"31 - 50",
						"51 - 70",
						"51 - 70",
						"71+",
						"71+",
						"71+")

categories <- list( ## careful of the order  here -- has to match the order of "strats"
	ethnicity = ethnicity_cats,
	gender = gender_cats,
	region = region_cats,
	age = age_cats
)

## take out that stratifier, make factor, relabel levels
refactored_shiny <- NULL
for(xx in 1:length(strats)){
	temp_file <- shiny_file %>%
		filter(stratifier == stringr::str_to_lower(strats[xx])) %>%
		mutate_at("category", ~as.factor(.))
	levels(temp_file$category) <- categories[[xx]]
	temp_file <- temp_file %>%
		group_by(weekDate, category, time, stratifier, outcome, weekPlot) %>%
		summarise(numEligible = sum(numEligible), numOutcome = sum(numOutcome)) %>%
		ungroup() %>%
		mutate(model_out = (numOutcome/numEligible)*100)
	refactored_shiny <- bind_rows(refactored_shiny, temp_file)
}
refactored_shiny <- shiny_file %>%
	filter(stratifier == "overall") %>%
	mutate_at("category", ~as.factor(.)) %>%
	bind_rows(refactored_shiny) %>%
	mutate_at("category", ~as.character(.))
refactored_shiny <- refactored_shiny %>%
	select(weekPlot, outcome, stratifier, category, model_out)

outcome_of_interest <- sort(c("alcohol","anxiety","asthma", "cba", "copd", "depression", "diabetes", "feedingdisorders", "hf", "mi", "ocd", "selfharm","smi", "tia", "ua", "vte"))
outcome_of_interest_namematch <- bind_cols("outcome" = outcome_of_interest, 
																					 "outcome_name" = (c("Acute Alcohol-Related Event", "Anxiety", "Asthma Exacerbations",  "Cerebrovascular Accident", "COPD Exacerbations",
																					 										"Depression", "Diabetic Emergencies", "Feeding Disorders", 
																					 										"Heart Failure", "Myocardial Infarction", "OCD", "Self-harm", "Serious Mental Illness", "Transient Ischaemic Attacks", 
																					 										"Unstable Angina", "Venous Thromboembolism"))
)
plot_order <- c(7,1,2,6,8,11,12,13,4,9,10,14,15,16,3,5) 

refactored_shiny <- refactored_shiny %>% 
	left_join(outcome_of_interest_namematch, by = "outcome") 

refactored_shiny$outcome_name <-  factor(refactored_shiny$outcome_name, levels = outcome_of_interest_namematch$outcome_name[plot_order])

refactored_shiny <- refactored_shiny %>%
	select(-outcome, outcome = outcome_name)

save(refactored_shiny, file = here::here("data/refactored_shiny.RData"))
