#pacman::p_load(shiny, shinythemes, dplyr, rmarkdown, ggplot2, here)
library(shiny)
#library(shinythemes)
library(dplyr)
library(rmarkdown)
library(ggplot2)
library(here)
here()

# load data ---------------------------------------------------------------
#data_files <- list.files(here::here("../../../data/"), pattern = "*.csv")
data_file <- list.files(here::here("data/"), pattern = "*.RData")
lapply(here::here("data",data_file), load,.GlobalEnv)

## change gender to sex 
refactored_shiny <- refactored_shiny %>%
	mutate_at("stratifier", ~ifelse(. == "gender", "sex", .))
## get unique levels to label choice boxes in app
sex_choice <- unique(filter(refactored_shiny, stratifier == "sex")$category) %>% as.list()
ethnicity_choice <- unique(filter(refactored_shiny, stratifier == "ethnicity")$category) %>% as.list()
region_choice <- unique(filter(refactored_shiny, stratifier == "region")$category) %>% as.list()
age_choice <- unique(filter(refactored_shiny, stratifier == "age")$category) %>% as.list()
outcome_choices_reordered <- unique(refactored_shiny$outcome)[c(7,1,2,6,8,11,12,13,4,9,10,14,15,16,3,5)]
apptitle <- "COVID-Collateral"

# user defined ggplot theme -----------------------------------------------
theme_collateral <- function (base_size = 11, base_family = ""){
	theme(plot.title = element_text(size = 12, hjust = 0),
				axis.title = element_text(size = rel(1), colour = alpha(1, 0.6), hjust=0.5, family = "Helvetica"), 
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
				rect = element_rect(fill = "white"),
				legend.background = element_blank(), legend.key = element_blank()
	)
}

# user interface ----------------------------------------------------------
ui <- shinyUI(
	navbarPage(
		theme = "bootstrap.min.css",
		 title = div(
		 	a(img(src="collateral_logo.pdf", height = "35px", align = "top"), 
		 				href="https://www.lshtm.ac.uk/research/centres-projects-groups/electronic-health-records"),
		 	span(apptitle)
		 ),
			
		tabPanel("Plot",
						 sidebarLayout(
						 	position = "right",
						 	## make the sidebar
						 	sidebarPanel(
						 		actionButton(inputId = "runPlot", 
						 								 style = "color: white; background-color:#4682b4", 
						 								 label = "Plot"
						 		),
						 		helpText("Data can be shown for several outcomes simultaneously or one at a time by checking the boxes below"),
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
						 									 end = "2020-07-01"),
						 		checkboxInput("lockdownLine",
						 									label = "Display lockdown timing", 
						 									value = FALSE)
						 	),
						 	## make the main panel
						 	mainPanel(
						 		tabsetPanel(id = "stratifier",
						 		tabPanel("Overall",
					 								sidebarLayout(
					 									position = "left",
					 									sidebarPanel("Outcome totals"
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
						 		tabPanel("Region",
						 						 sidebarLayout(
						 						 	position = "left",
						 						 	sidebarPanel(
						 						 		selectInput("labRegion", 
						 						 								multiple = TRUE,
						 						 								label = "Region", 
						 						 								choices = region_choice,
						 						 								selected = region_choice
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
						 		)
						 		)
						 		)
						 )
					),
		tabPanel("ITS",
	 				 sidebarLayout(
	 				 	position = "right",
	 				 	## make the sidebar
	 				 	sidebarPanel(
	 				 		helpText("Testing 1,2,3..."),
	 				 		checkboxGroupInput("Lockdown start", 
	 				 											 label = "Choose outcome variables to display", 
	 				 											 choices = as.list(unique(refactored_shiny$lockdown)),
	 				 											 selected = as.list(unique(refactored_shiny$lockdown))
	 				 		),
	 				 		actionButton(inputId = "runITS", 
	 				 								 label = "Testing"
	 				 		)
	 				 	),
	 				 	## make the main panel
						 	mainPanel(
						 		uiOutput("pdfview")
						 	)
						)
		),
		tabPanel("About",
						 fluidRow(
						 	column(8,
						 				 includeMarkdown(here::here("notes.md"))
						 	)
						 )
		)
	)
)

server <- function(input, output, session){
	## is "select all" ticked? 
	#* This observer will update checkboxes 1 - 4 to TRUE whenever selectAll is clicked
	observeEvent(
		eventExpr = input$selectAll,
		handlerExpr = 
		{
		 	updateCheckboxInput(session = session, 
		 											inputId = "var", 
		 											value = outcome_choices_reordered)
		}
	)
	
	#* This observer will update checkboxes 1 - 4 to FALSE whenever deselectAll is clicked
	observeEvent(
		eventExpr = input$deselectAll,
		handlerExpr = 
		{
			updateCheckboxInput(session = session, 
													inputId = "var", 
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
		} else if (input$stratifier == "Region"){
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
		v$plot <- ggplot(df_shiny(), aes(x=weekPlot, y=model_out, group = category, colour = category)) +
			geom_line() +
			xlab("Date") +
			ylab("Proportion Overall") +
			theme(axis.text.x = element_text(angle=60, hjust=1)) +
			#labs(colour = "Region") +
			facet_wrap(~outcome, ncol = 2, scales = "free") +
			theme_collateral()
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
	})
	output$mainplot2 <- renderPlot({
		if (is.null(v$plot)) return()
		if(input$lockdownLine) {
			v$plot + geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed", col = 2)
		}else{
			v$plot
		}
	})
	output$mainplot3 <- renderPlot({
		if (is.null(v$plot)) return()
		if(input$lockdownLine) {
			v$plot + geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed", col = 2)
		}else{
			v$plot 
		}
	})
	output$mainplot4 <- renderPlot({
		if (is.null(v$plot)) return()
		if(input$lockdownLine) {
			v$plot + geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed", col = 2)
		}else{
			v$plot
		}
	})
	output$mainplot5 <- renderPlot({
		if (is.null(v$plot)) return()
		if(input$lockdownLine) {
			v$plot + geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed", col = 2)
		}else{
			v$plot
		}
	})
	
	## plot a pdf image
	observeEvent(input$runITS, {
		output$pdfview <- renderUI({
			tags$iframe(style="height:600px; width:100%", src="Figure3_its_backdata_full.pdf")
		})
	})
	
}

shinyApp(ui, server, options = list(height = 1300))
