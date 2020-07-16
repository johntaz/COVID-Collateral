library(shiny)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(here)
library(geofacet)
here()

# load data ---------------------------------------------------------------
data_files <- list.files(here::here("data/"), pattern = "*RData")
lapply(here::here("data",data_files), load,.GlobalEnv)

## bodge grid
namestoinclude <- c("North East","North West","Yorkshire & the Humber","East Midlands","West Midlands","Eastern","South West","South Central","London","South East","Northern Ireland")
southcentral <- bind_cols("row" = 7, "col" = 3, "name" = "South Central", "code" = "UKK")
my_grid <- geofacet::uk_regions1 %>%
	filter(name %in% namestoinclude) %>%
	bind_rows(southcentral)

regions <- as.list(unique(df_figureRegion$name))

# user interface ----------------------------------------------------------
ui <- fluidPage(
	tags$head(tags$script(src = "message-handler.js")),
	
	titlePanel("COVID-collateral"),
	sidebarLayout(
		position = "right",
		## make the sidebar
		sidebarPanel(
			helpText("Data can be shown for several outcomes simultaneously or one at a time by checking the boxes below"),
			checkboxGroupInput("var", 
												 label = "Choose outcome variables to display", 
												 choices = list("Diabetes",
												 							 "Other (does nothing currently)"),
												 						selected = "Diabetes"),
			checkboxGroupInput("regions", 
												 label = "Regions to display", 
												 choices = regions,
												 selected = "London"),
			dateRangeInput("dates", 
									label = "Date range:", 
									start = "2017-01-01", 
									end = "2020-07-01"),
			checkboxInput("lockdownLine",
									label = "Display lockdown timing", 
									value = FALSE),
			actionButton(inputId = "runPlot", 
									 label = "Submit"
									 )
			),
		## make the main panel
		mainPanel(
			textOutput("selected_var"),
			plotOutput("mainplot")
		)
	)
)

server <- function(input, output, session){
		text_to_print <- reactive({
			input$regions
		})
		output$selected_var <- renderText({
			paste("You have selected", text_to_print())
		})
		
		## define df to plot as reactive
		df_shiny <- reactive({
			dv_shiny <- df_figureRegion %>% 
				filter(weekDate >= format(input$dates[1]) & weekDate <= format(input$dates[2])) %>% 
				filter(name %in% input$regions) %>%
				mutate_at("name", ~as.factor(.))
		})
		
		## set up reactive values to store plot
		v <- reactiveValues(plot = NULL)
		
		## if runPlot button is pressed then build the plot
		observeEvent(input$runPlot, {
			v$plot <- ggplot(df_shiny(), aes(x=time, y=model_out, group = name, colour = name)) +
				geom_line() +
				xlab("Time") +
				ylab("Proportion Overall") +
				theme_classic() +
				theme(axis.text.x = element_text(angle=60, hjust=1)) +
				scale_x_continuous(breaks = c(0,52, 104, 156, 167, 208), labels = c("2017","2018","2019","2020","Lockdown","2021")) +
				labs(colour = "Region")
		})
		
			## if lockdownLine is on then put the x intercept on the plot 
			# observeEvent(input$lockdownLine, {
			# 	v$plot <- v$plot +
			# 		geom_vline(xintercept = 167, linetype = "dashed")
			# })
		
		output$mainplot <- renderPlot({
			if (is.null(v$plot)) return()
			if(input$lockdownLine) {
				v$plot + geom_vline(xintercept = 167, linetype = "dashed")
			}else{
				v$plot
			}
		})
	# v <- reactiveValues(doPlot = FALSE)
	# y <- reactiveValues(doAbline = FALSE)
	# observeEvent(input$runPlot, { 
	# 		# 0 will be coerced to FALSE
	# 		# 1+ will be coerced to TRUE
	# 		v$doPlot <- input$runPlot
	# })
	# observeEvent(input$lockdownLine, { 
	# 		# 0 will be coerced to FALSE
	# 		# 1+ will be coerced to TRUE
	# 		y$doAbline <- input$lockdownLine
	# })
	# 	output$mainplot <- renderPlot({
	# 		if (v$doPlot == FALSE) return()
	# 		isolate({
	# 			plot1 <- ggplot(df_shiny(), aes(x=time, y=model_out, group = name, colour = name)) +
	# 				geom_line() +   
	# 				xlab("Time") +
	# 				ylab("Proportion Overall") +
	# 				theme_classic() +
	# 				theme(axis.text.x=element_text(angle=60, hjust=1)) +
	# 				scale_x_continuous(breaks = c(0,52, 104, 156, 167, 208), labels = c("2017","2018","2019","2020","Lockdown","2021")) +
	# 				labs(colour = "Region")
	# 				#theme(legend.position = "none") +	
	# 				#organise by state name in grid file
	# 				#geofacet::facet_geo(~name, grid = my_grid) 
	# 		if (y$doAbline == F){
	# 			return(plot1)
	# 		}else {
	# 			return(plot1 + 
	# 						 	geom_vline(xintercept = 167, linetype = "dashed")
	# 			)
	# 		} 
	# 		})
	# 	})
}

shinyApp(ui, server)
