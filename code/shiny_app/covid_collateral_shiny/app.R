#pacman::p_load(shiny, shinythemes, dplyr, rmarkdown, ggplot2, here)
library(shiny)
library(shinythemes)
library(dplyr)
library(rmarkdown)
library(ggplot2)
library(here)
here()

# load data ---------------------------------------------------------------
#data_files <- list.files(here::here("../../../data/"), pattern = "*.csv")
data_file <- list.files(here::here("data/"), pattern = "*.RData")
lapply(here::here("data",data_file), load,.GlobalEnv)

## get unique levels to label choice boxes in app
ethnicity_choice <- unique(filter(refactored_shiny, stratifier == "ethnicity")$category) %>% as.list()
gender_choice <- unique(filter(refactored_shiny, stratifier == "gender")$category) %>% as.list()
region_choice <- unique(filter(refactored_shiny, stratifier == "region")$category) %>% as.list()
age_choice <- unique(filter(refactored_shiny, stratifier == "age")$category) %>% as.list()

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
		#theme = shinytheme("cerulean"),
		theme = "bootstrap.min.css",
		#title = apptitle,
		 title = div(
		 	a(img(src="lshtm.png", height = "20px", align = "top"), 
		 				href="https://lshtm.ac.uk/"),
		 	span(apptitle)
		 ),
		tabPanel("Plot",
						 sidebarLayout(
						 	position = "right",
						 	## make the sidebar
						 	sidebarPanel(
						 		helpText("Data can be shown for several outcomes simultaneously or one at a time by checking the boxes below"),
						 		checkboxGroupInput("var", 
						 											 label = "Choose outcome variables to display", 
						 											 choices = as.list(unique(refactored_shiny$outcome)),
						 											 selected = as.list(unique(refactored_shiny$outcome))[[1]]
						 											 ),
						 		dateRangeInput("dates", 
						 									 label = "Date range:", 
						 									 start = "2017-01-01", 
						 									 end = "2020-07-01"),
						 		checkboxInput("lockdownLine",
						 									label = "Display lockdown timing", 
						 									value = FALSE),
						 		actionButton(inputId = "runPlot", 
						 								 label = "Plot"
						 		)
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
						 		tabPanel("Gender",
						 						 sidebarLayout(
						 						 	position = "left",
						 						 	sidebarPanel(
						 						 		selectInput("labGender", 
						 						 								multiple = TRUE,
						 						 								label = "Gender", 
						 						 								choices = gender_choice,
						 						 								selected = gender_choice
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
		} else if (input$stratifier == "Gender"){
			input$labGender
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
			facet_wrap(~outcome, ncol = 3, scales = "free") +
			theme_collateral()
	})
	
	output$mainplot1 <- renderPlot({
		if (is.null(v$plot)) return()
		if(input$lockdownLine) {
			v$plot + geom_vline(xintercept = as.Date("2020-03-16"), linetype = "dashed", col = 2) +
				theme(legend.position = "none")
		}else{
			v$plot +
				theme(legend.position = "none")
		}
	})
	output$mainplot2 <- renderPlot({
		if (is.null(v$plot)) return()
		if(input$lockdownLine) {
			v$plot + geom_vline(xintercept = as.Date("2020-03-16"), linetype = "dashed", col = 2)
		}else{
			v$plot
		}
	})
	output$mainplot3 <- renderPlot({
		if (is.null(v$plot)) return()
		if(input$lockdownLine) {
			v$plot + geom_vline(xintercept = as.Date("2020-03-16"), linetype = "dashed", col = 2)
		}else{
			v$plot 
		}
	})
	output$mainplot4 <- renderPlot({
		if (is.null(v$plot)) return()
		if(input$lockdownLine) {
			v$plot + geom_vline(xintercept = as.Date("2020-03-16"), linetype = "dashed", col = 2)
		}else{
			v$plot
		}
	})
	output$mainplot5 <- renderPlot({
		if (is.null(v$plot)) return()
		if(input$lockdownLine) {
			v$plot + geom_vline(xintercept = as.Date("2020-03-16"), linetype = "dashed", col = 2)
		}else{
			v$plot
		}
	})
}

shinyApp(ui, server)
