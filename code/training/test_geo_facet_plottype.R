weekDate   numEligible  time lockdown numOutcome
<date>           <dbl> <dbl>    <dbl>      <dbl>
	1 2017-01-01    10000196     0        0      67513
2 2017-01-08    10012539     1        0      83179
3 2017-01-15    10017329     2        0      75125
4 2017-01-22    10019702     3        0      73302
5 2017-01-29    10025399     4        0      73269
6 2017-02-05    10029949     5        0      75883
7 2017-02-12    10034373     6        0      71903
8 2017-02-19    10038262     7        0      71561
9 2017-02-26    10043484     8        0      78471
10 2017-03-05    10048795     9        0      74866

diabetes_john <- tibble(
	weekDate = seq.Date(as.Date("2017-01-01"), as.Date("2020-04-2020"), by = "7 days"),
	numEligible = c(round(runif(166, 1e7, 1.05e7)), 7592029, 5634328, 5566744, 5555731, 5540986, 5484581, 5460294),
	numOutcome = c(round(runif(165, 6e4, 75e3)), 44846, 22017, 15784, 13138,  8830, 8039,  9535,  2097)
) %>%
	mutate(time = 1:n(),
				 lockdown = 0) %>%
	mutate_at("lockdown", ~ifelse(weekDate>as.Date("2020-03-28"), 1, 0)) 


outcome <-  with(diabetes_john, cbind(numOutcome, numEligible))
model1 <- glm(outcome ~ time * lockdown, data = diabetes_john, family = binomial)
summary(model1)

# figure
figure1 <- ggplot(diabetes_john, aes(x = time, y = model1$y)) +
	geom_line(color = "steelblue") +
	geom_point(size = 1) +
	xlab("Time") +
	ylab("Proportion Overall") +
	theme_classic() +
	theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
	scale_x_continuous(
		breaks = c(0, 52, 104, 156, 167, 208),
		labels = c("2017", "2018", "2019", "2020", "Lockdown", "2021")
	) +
	geom_vline(xintercept = 167, linetype = "dashed")
figure1

diabetes_john_region <- diabetes_john %>%
	mutate(region = 99)
for(ii in 1:11){
	random_denom <- round(runif(1, 8, 12))
	region_temp <- tibble(
			weekDate = seq.Date(as.Date("2017-01-01"), as.Date("2020-04-2020"), by = "7 days"),
			numEligible = c(round(runif(166, 1e7/random_denom, 1.05e7/random_denom)), round(7592029/random_denom), round(runif(6, 5e6/random_denom, 6e6/random_denom))),
			numOutcome = c(round(runif(165, 6e4/random_denom, 75e3/random_denom)), round(c(44846/random_denom, 22000/random_denom)), round(runif(6, 8000/random_denom, 12000/random_denom)))
		) %>%
		mutate(time = 1:n(),
					 lockdown = 0,
					 region = ii) %>%
		mutate_at("lockdown", ~ifelse(weekDate>as.Date("2020-03-28"), 1, 0)) 
	
	diabetes_john_region <- diabetes_john_region %>%
		bind_rows(region_temp)
}

diabetes_john_region <- diabetes_john_region %>%
	filter(region != 99)


# Ignoring seasonal trend
outcomeRegion <-  with(diabetes_john_region, cbind(numOutcome, numEligible))
modelRegion <- glm(outcomeRegion ~ region*time*lockdown, data = diabetes_john_region, family = binomial)
summary(modelRegion)

# figure
df_figureRegion <-  diabetes_john_region %>% 
	mutate(region = as.factor(region)) %>%
	mutate(labels = 
				 	case_when( region == 1 ~ "North East",
				 						 region == 2 ~ "North West",
				 						 region == 3 ~ "Yorkshire & the Humber",
				 						 region == 4 ~ "East Midlands",
				 						 region == 5 ~ "West Midlands",
				 						 region == 6 ~ "Eastern",
				 						 region == 7 ~ "South West",
				 						 region == 8 ~ "South Central",
				 						 region == 9 ~ "London",
				 						 region == 10 ~ "South East",
				 						 region == 11 ~ "Northern Ireland" ) 
				 
	) %>%
	rename(name = labels)

figureRegion <- ggplot(df_figureRegion, aes(x=time, y=modelRegion$y)) +
	geom_line(aes(color = name)) +   
	xlab("Time") +
	ylab("Proportion Overall") +
	theme_classic() +
	theme(axis.text.x=element_text(angle=60, hjust=1)) +
	scale_x_continuous(breaks = c(0,52, 104, 156, 167, 208), labels = c("2017","2018","2019","2020","Lockdown","2021")) +
	geom_vline(xintercept = 167, linetype = "dashed") + 
	theme(legend.title=element_blank())

figureRegion


namestoinclude <- c("North East","North West","Yorkshire & the Humber","East Midlands","West Midlands","Eastern","South West","South Central","London","South East","Northern Ireland")
southcentral <- bind_cols("row" = 7, "col" = 3, "name" = "South Central", "code" = "UKK")
my_grid <- geofacet::uk_regions1 %>%
	filter(name %in% namestoinclude) %>%
	bind_rows(southcentral)
ggplot(df_figureRegion, aes(x=time, y=modelRegion$y, group = name, colour = name)) +
	geom_line() +   
	xlab("Time") +
	ylab("Proportion Overall") +
	theme_classic() +
	theme(axis.text.x=element_text(angle=60, hjust=1)) +
	scale_x_continuous(breaks = c(0,52, 104, 156, 167, 208), labels = c("2017","2018","2019","2020","Lockdown","2021")) +
	#geom_vline(xintercept = 167, linetype = "dashed") + 
	theme(legend.position = "none") + 	
	#organise by state name in grid file
	facet_geo( ~name, grid = my_grid) 

ggsave("~lsh1510922/Documents/COVID-collateral/graphfiles/test_UKfacet.pdf", width = 8, height = 8)
