#pacman::p_load(shiny, shinythemes, dplyr, rmarkdown, ggplot2, here)
library(shiny)
library(dplyr)
library(rmarkdown)
library(ggplot2)
library(patchwork)
library(here)
here()

# load data ---------------------------------------------------------------
#data_files <- list.files(here::here("../../../data/"), pattern = "*.csv")
data_file <- list.files(here::here("data/"), pattern = "*.RData")
lapply(here::here("data",data_file), load,.GlobalEnv)

## change gender to sex 
refactored_shiny <- refactored_shiny %>%
	mutate_at("stratifier", ~ifelse(. == "gender", "sex", .)) %>%
	mutate_at("stratifier", ~ifelse(. == "region_sum", "region (summary)", .)) %>%
	mutate_at("stratifier", ~ifelse(. == "region", "region (detail)", .)) 

## get unique levels to label choice boxes in app
sex_choice <- unique(filter(refactored_shiny, stratifier == "sex")$category) %>% as.list()
ethnicity_choice <- unique(filter(refactored_shiny, stratifier == "ethnicity")$category) %>% as.list()
region_choice <- unique(filter(refactored_shiny, stratifier == "region (detail)")$category) %>% as.list()
region_sum_choice <- unique(filter(refactored_shiny, stratifier == "region (summary)")$category) %>% as.list()
age_choice <- unique(filter(refactored_shiny, stratifier == "age")$category) %>% as.list()
outcome_choices_reordered <- levels(refactored_shiny$outcome)
apptitle <- "COVID-Collateral"
bkg_colour <- "gray99"

# user defined ggplot theme -----------------------------------------------
theme_collateral <- function (base_size = 11, base_family = ""){
	theme(plot.title = element_text(size = 12, hjust = 0),
				axis.title = element_text(size = rel(1.5), colour = alpha(1, 0.6), hjust=0.5, family = "Helvetica"), 
				axis.text = element_text(size=rel(1.1)),
				axis.text.x = element_text(angle=20, hjust=1),
				legend.text = element_text(size = rel(1.3)),
				panel.background = element_rect(fill = "white",
																				colour = "white",
																				size = 0.5, linetype = "solid"),
				axis.line = element_line(size=rel(1.5), colour = alpha(1,0.3)),
				panel.grid.major = element_blank(),
				panel.grid.minor.x = element_blank(),
				panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
				panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3)) ,
				axis.ticks = element_line( size=.1, color=rgb(0,0,0,0.4)),
				strip.background = element_rect(fill = "white",
																				colour = "white",
																				size = 0.5, linetype = "solid"),
				strip.text = element_text(size = rel(1.2), hjust = 0 , family = "Helvetica", face = "bold"),
				legend.title = element_text(size = rel(1), family = "Helvetica", colour = alpha(1, 0.6)),
				legend.position = "top",
				rect = element_rect(fill = "white"),
				legend.background = element_blank(), legend.key = element_blank()
	)
}

#div(
#	a(img(src="collateral_logo1.png", height = "35px", align = "top"), 
#		href="https://twitter.com/ehr_lshtm?lang=en"),
#	a(apptitle, href = "https://twitter.com/ehr_lshtm?lang=en")
#),

# user interface ----------------------------------------------------------
ui <- shinyUI(
	navbarPage(
		title = "COVID-Collateral",
		theme = "bootstrap.min.css",
		tabPanel("Primary care contacts",
						 sidebarLayout(
						 	position = "right",
						 	## make the sidebar
						 	sidebarPanel(
						 		actionButton(inputId = "runPlot", 
						 								 style = "color: white; background-color:#4682b4", 
						 								 label = "Plot"
						 		),
						 		helpText("Data can be shown for several conditions simultaneously or one at a time by checking the boxes below"),
						 		actionButton("selectAll", label = "Select All"),
						 		actionButton("deselectAll", label = "Deselect All"),
						 		checkboxGroupInput("var", 
						 											 label = "Choose outcome variables to display", 
						 											 choices = as.list(outcome_choices_reordered),
						 											 selected = as.list(outcome_choices_reordered)
						 											 ),
						 		helpText("Data can be shown from January 2017 to July 2020"),
						 		dateRangeInput("dates", 
						 									 label = "Date range:", 
						 									 start = "2020-01-01", 
						 									 end = "2020-07-31"),
						 		checkboxInput("lockdownLine",
						 									label = "Display lockdown date (23 March 2020)", 
						 									value = FALSE)
						 	),
						 	## make the main panel
						 	mainPanel(
						 		tabsetPanel(id = "stratifier",
						 		tabPanel("Overall",
					 								sidebarLayout(
					 									position = "left",
					 									sidebarPanel(
					 										helpText('Welcome to the COVID colateral Shiny app.'),
					 										helpText('You can use this app to examine the data used in an analysis of primary care records 
					 														 from approximately 10 million people across England and Northern Ireland between 2017 to 2020.'),
					 										helpText('Use the "Plot" button to display data from 
					 														 our analysis.'),
					 										helpText('The "Interrupted Time Series" tab shows results from our statistical analysis of 
					 														 these data. On this page you can vary the parameters in our analysis to assess the 
					 														 impact on our findings.'),
					 										helpText('The "Using the app" tab has information on how to make the 
					 														 most of this app.'),
					 										helpText('The "About the app" has information about the data and a link to the accompanying analysis.'),
					 										helpText('An important note: each of the conditions used a specific denominator population when the percentage
					 														 contacting primary care was calculated. For example, "anxiety" contacts were in anyone over 10 years old. 
					 														 However, "asthma exacerbations" were only in those aged over 10 years old with a diagnosis of asthma. 
					 														 Please see the "About the app" tab or our paper for more information.')
					 									),
					 									mainPanel(
					 										plotOutput("mainplot1")
					 									)
					 								)
						 				),
						 		tabPanel("Age",
						 						 sidebarLayout(
						 						 	position = "left",
						 						 	sidebarPanel(
						 						 		selectInput("labAge", 
						 						 								multiple = TRUE,
						 						 								label = "Age", 
						 						 								choices = age_choice,
						 						 								selected = age_choice
						 						 								)
						 						 	),
						 						 	mainPanel(
						 								plotOutput("mainplot2")
						 						 )
						 					)
						 				),
						 		tabPanel("Ethnicity",
						 						 sidebarLayout(
						 						 	position = "left",
						 						 	sidebarPanel(
						 						 		selectInput("labEthnicity", 
						 						 								multiple = TRUE,
						 						 								label = "Ethnicity", 
						 						 								choices = ethnicity_choice,
						 						 								selected = ethnicity_choice
						 						 		)
						 						 	),
						 						 	mainPanel(
						 						 		plotOutput("mainplot5")
						 						 	)
						 						 )
					),
						 		tabPanel("Region (summary)",
						 						 sidebarLayout(
						 						 	position = "left",
						 						 	sidebarPanel(
						 						 		selectInput("labRegionSum", 
						 						 								multiple = TRUE,
						 						 								label = "Region", 
						 						 								choices = region_sum_choice,
						 						 								selected = region_sum_choice[region_sum_choice != "Missing"]
						 						 								)
						 						 	),
						 						 	mainPanel(
						 								plotOutput("mainplot3A")
						 						 )
						 					)
						 				),
						 		tabPanel("Region (detail)",
						 						 sidebarLayout(
						 						 	position = "left",
						 						 	sidebarPanel(
						 						 		selectInput("labRegion", 
						 						 								multiple = TRUE,
						 						 								label = "Region", 
						 						 								choices = region_choice,
						 						 								selected = region_choice[region_choice != "Missing"]
						 						 								)
						 						 	),
						 						 	mainPanel(
						 								plotOutput("mainplot3")
						 						 )
						 					)
						 				),
						 		tabPanel("Sex",
						 						 sidebarLayout(
						 						 	position = "left",
						 						 	sidebarPanel(
						 						 		selectInput("labSex", 
						 						 								multiple = TRUE,
						 						 								label = "Sex", 
						 						 								choices = sex_choice,
						 						 								selected = sex_choice
						 						 		)
						 						 	),
						 						 	mainPanel(
						 								plotOutput("mainplot4")
						 						 )
						 					)
						 		)
						 		)
						 	)
						 )
					),
		tabPanel("Interrupted Time Series",
						 sidebarLayout(
						 	position = "left",
						 	## make the sidebar
						 	sidebarPanel(
						 		helpText('This page shows results from an interrupted time series (ITS) analysis. This ITS separates the time series of
						 						 primary care contacts into two period: "pre-restrictions" and "with-restrictions" (A). We then formally compare
						 						 whether the "with-restrictions" level of primary care contacts is lower than the "pre-restrictions" level and
						 						 by how much (B: Reduction)". We also show how quickly primary care contacts are "recovering" since the "with-
						 						 restrictions" period started (C: Recovery).'),
						 		helpText('Here you can change two factors that affect this analysis: when to end the "pre-restrictions" period, and 
						 						 when to start the "with-restrictions" period.'),
						 		radioButtons("restrictions_start",
						 									label = "Choose when the pre-restrictions period ends", 
						 								 	choices = c(
						 								 		"1st March" = as.Date("2020-03-08"),
						 										"23rd March" = as.Date("2020-03-22"))),
						 		radioButtons("adj_gap",
						 								 label = "Choose how many weeks to exclude (adjustment to restrictions period)", 
						 								 choices = c(
						 								 	"0" = 0,
						 								 	"3" = 3,
						 								 	"5" = 5,
						 								 	"7" = 7),
						 								 selected = "3"),
						 	
						 	actionButton("selectAll2", label = "Select All"),
						 	actionButton("deselectAll2", label = "Deselect All"),
						 	checkboxGroupInput("var2", 
						 										 label = "Choose outcome variables to display", 
						 										 choices = as.list(outcome_choices_reordered),
						 										 selected = as.list(outcome_choices_reordered)
						 	),
						 	width = 3),
						 	## make the main panel
						 	mainPanel(
						 		plotOutput("its_plot")
						 		)
						 )
		),
		tabPanel("Using the App",
						 fluidRow(
						 	column(8,
						 				 includeMarkdown(here::here("usingnotes.md"))
						 	)
						 )
		),
		tabPanel("About the App",
						 fluidRow(
						 	column(8,
						 				 includeMarkdown(here::here("notes.md"))
						 	)
						 )
		),
		tags$style(HTML(".navbar {height: 55px;
										min-height:25px !important;}
										.navbar-header {
										padding-top:20px !important; 
										padding-bottom:35px !important;}
										.navbar-nav > li > a, 
										.navbar-brand {
										padding-top:20px !important; 
										padding-bottom:35px !important;
										height: 25px;
										}"))
	)
)


server <- function(input, output, session){
	## is "select all" ticked? 
	observeEvent(
		eventExpr = input$selectAll,
		handlerExpr = 
		{
		 	updateCheckboxInput(session = session, 
		 											inputId = "var", 
		 											value = outcome_choices_reordered)
		}
	)
	observeEvent(
		eventExpr = input$deselectAll,
		handlerExpr = 
		{
			updateCheckboxInput(session = session, 
													inputId = "var", 
													value = NA)
		}
	)
	
	## Repeat for page 2
	observeEvent(
		eventExpr = input$selectAll2,
		handlerExpr = 
		{
		 	updateCheckboxInput(session = session, 
		 											inputId = "var2", 
		 											value = outcome_choices_reordered)
		}
	)
	observeEvent(
		eventExpr = input$deselectAll2,
		handlerExpr = 
		{
			updateCheckboxInput(session = session, 
													inputId = "var2", 
													value = NA)
		}
	)
	
	
	v <- reactiveValues(plot = NULL)
	
	observeEvent(input$stratifier, {
		v$plot <- NULL
	})  
	## define df to plot as reactive
	df_shiny <- reactive({
		data <- refactored_shiny %>%
			filter(stratifier == stringr::str_to_lower(input$stratifier))
		
		labs_to_find <- if (input$stratifier == "Age") {
			input$labAge
		} else if (input$stratifier == "Region (summary)"){
			input$labRegionSum
		} else if (input$stratifier == "Region (detail)"){
			input$labRegion
		} else if (input$stratifier == "Sex"){
			input$labSex
		} else if (input$stratifier == "Ethnicity"){
			input$labEthnicity
		} else (1)
		
		df_shiny <- data %>% 
			filter(weekPlot >= format(input$dates[1]) & weekPlot <= format(input$dates[2])) %>% 
			filter(category %in% labs_to_find) %>%
			filter(outcome %in% input$var) %>%
			mutate_at("category", ~as.factor(.))
	})
		
	## set up reactive values to store plot
	v <- reactiveValues(plot = NULL)
	
	## if runPlot button is pressed then build the plot
	observeEvent(input$runPlot, {
		v$plot <- ggplot(df_shiny(), aes(x=weekPlot, y=model_out, group = category, colour = category, shape = category)) +
			geom_point(size = 1.5) +
			geom_line(size = 0.5) +
			xlab("Date") +
			ylab("% of people at-risk* consulting for condition") +
			labs(caption = c('*see "About this app" tab for detail', "OCD: Obsessive Compulsive Disorder. COPD: Chronic Obstructive Pulmonary Disease")) +
			labs(colour = input$stratifier, shape = input$stratifier) +
			facet_wrap(~outcome, ncol = 2, scales = "free") +
			theme_collateral() +
			theme(plot.caption = element_text(hjust=c(0, 1)))
	})
	
	output$mainplot1 <- renderPlot({
		if (is.null(v$plot)) return()
		if(input$lockdownLine) {
			v$plot + geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed", col = 2) +
				theme(legend.position = "none")
		}else{
			v$plot +
				theme(legend.position = "none")
		}
	},
	height=800)
	output$mainplot2 <- renderPlot({
		if (is.null(v$plot)) return()
		if(input$lockdownLine) {
			v$plot + geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed", col = 2)
		}else{
			v$plot
		}
	},
	height=800)
	output$mainplot3A <- renderPlot({
		if (is.null(v$plot)) return()
		if(input$lockdownLine) {
			v$plot + geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed", col = 2)
		}else{
			v$plot 
		}
	},
	height=800)
	output$mainplot3 <- renderPlot({
		if (is.null(v$plot)) return()
		if(input$lockdownLine) {
			v$plot + geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed", col = 2)
		}else{
			v$plot 
		}
	},
	height=800)
	output$mainplot4 <- renderPlot({
		if (is.null(v$plot)) return()
		if(input$lockdownLine) {
			v$plot + geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed", col = 2)
		}else{
			v$plot
		}
	},
	height=800)
	output$mainplot5 <- renderPlot({
		if (is.null(v$plot)) return()
		if(input$lockdownLine) {
			v$plot + geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed", col = 2)
		}else{
			v$plot
		}
	},
	height=800)
	

	# ITS plot ----------------------------------------------------------------
	output$its_plot <- renderPlot({
		its_data <- mainplot_results %>%
			filter(start_lockdown == input$restrictions_start,
						 adj_remove == input$adj_gap,
						 weekPlot >= as.Date("2020-01-01"),
						 outcome_name %in% input$var2) 
		
		its_fp1_data <- fp1_results %>%
			filter(start_lockdown == input$restrictions_start,
						 adj_remove == input$adj_gap,
						 outcome_name %in% input$var2) %>%
			mutate(dummy_facet = "A")
		
		its_fp2_data <- fp2_results %>%
			filter(start_lockdown == input$restrictions_start,
						 adj_remove == input$adj_gap,
						 outcome_name %in% input$var2) %>%
			mutate(dummy_facet = "A")
		
		abline_min <- its_data %>% summarise(x = min(ldn_start)) %>% pull()
		abline_max <- its_data %>% summarise(x = min(ldn_end)) %>% pull()
		plot1 <- ggplot(data = its_data, aes(x = weekPlot, y = pc_consult, group = outcome_name)) +
			# the data
			geom_line(col = "gray60") +
			geom_line(aes(y = predicted_vals), col = 4, lty = 2) +
			geom_ribbon(aes(ymin = lci, ymax=uci), fill = alpha(4,0.4), lty = 0) +
			### format the plot
			facet_wrap(~outcome_name, scales = "free", ncol = 4) +
			geom_vline(xintercept = c(abline_min, 
																abline_max), col = 1, lwd = 1) + # 2020-04-05 is first week/data After lockdown gap
			labs(y = "% of people at-risk consulting for condition", title = "A", caption = "OCD: Obsessive Compulsive Disorder. COPD: Chronic Obstructive Pulmonary Disease") +
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
						panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3)))  +
				scale_x_date(breaks = "1 month", date_labels = "%b") +
				labs(x = "Date (2020)")
		
		# Forest plot of ORs ------------------------------------------------------
		## Forest plot
		fp <- ggplot(data = its_fp1_data, aes(x = dummy_facet, y=Est, ymin=lci, ymax=uci)) +
			geom_linerange(lwd = 1.5, colour = "darkred") +
			geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
			coord_flip() +  # flip coordinates (puts labels on y axis)
			labs(x = "", y = "95% CI", title = "B: Reduction") +
			facet_wrap(~outcome_name, ncol = 1, dir = "h", strip.position = "right") +
			theme_classic() +
			theme(axis.title = element_text(size = 16),
						axis.text.y = element_blank(),
						axis.line.y.left = element_blank(),
						axis.line.y.right = element_line(),
						axis.ticks.y = element_blank(),
						axis.text.x = element_text(angle = 0),
						legend.position = "top",
						plot.background = element_rect(fill = bkg_colour, colour =  NA),
						panel.background = element_rect(fill = bkg_colour, colour =  NA),
						legend.background = element_rect(fill = bkg_colour, colour = NA),
						strip.background = element_rect(fill = bkg_colour, colour =  NA),
						strip.text.y = element_text(hjust = 0.5, vjust = 0.5, angle=0, size = 10),
						strip.placement = "outside",
						panel.grid.major = element_blank(),
						panel.grid.minor.x = element_blank(),
						panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
						panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3)))
		
		# Forest plot of interaction terms ------------------------------------------------------
		# forest plot of estiamtes
		fp2 <- ggplot(data = its_fp2_data, aes(x=dummy_facet, y=Est, ymin=lci, ymax=uci)) +
			geom_linerange(lwd = 1.5, colour = "orange") +
			geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
			coord_flip() +  # flip coordinates (puts labels on y axis)
			labs(x = "", y = '95% CI', title = "C: Recovery") +
			facet_wrap(~outcome_name, ncol = 1, dir = "h", strip.position = "left") +
			theme_classic() +
			theme(axis.title = element_text(size = 16),
						axis.text.y = element_blank(),
						axis.line.y.left = element_blank(),
						axis.line.y.right = element_line(),
						axis.ticks.y = element_blank(),
						axis.text.x = element_text(angle = 0),
						legend.position = "top",
						plot.background = element_rect(fill = bkg_colour, colour =  NA),
						panel.background = element_rect(fill = bkg_colour, colour =  NA),
						legend.background = element_rect(fill = bkg_colour, colour = NA),
						strip.background = element_rect(fill = bkg_colour, colour =  NA),
						strip.text = element_text(hjust=0.5, vjust = 0.5, angle=0, size = 0),
						panel.grid.major = element_blank(),
						panel.grid.minor.x = element_blank(),
						panel.grid.minor.y = element_line(size=.2, color=rgb(0,0,0,0.2)) ,
						panel.grid.major.y = element_line(size=.2, color=rgb(0,0,0,0.3)))
		
		# Export plot -------------------------------------------------------------
		## uses patchwork package to combine plots
		layout = "
			AAAAAA
			AAAAAA
			BBBCCC
			BBBCCC
		"
		plot1 + fp + fp2 + 
			plot_layout(design = layout) 
	},
	height = 1000)
	
}

shinyApp(ui, server)
