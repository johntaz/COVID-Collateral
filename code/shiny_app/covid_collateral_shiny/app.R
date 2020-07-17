library(shiny)
library(shinythemes)
library(dplyr)
#library(tidyr)
library(rmarkdown)
library(ggplot2)
library(here)
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

# build main database to plot that groups everything ----------------------
df_for_app <- df_figureRegion %>% 
	mutate(outcome = "Diabetes") %>%
	bind_rows(mutate(df_CMI_figureRegion, outcome = "CMI")) %>%
	bind_rows(mutate(df_CMI_figureRegion, outcome = "SMI")) %>%
	bind_rows(mutate(df_CMI_figureRegion, outcome = "CVD")) %>%
	bind_rows(mutate(df_CMI_figureRegion, outcome = "Breathe")) %>%
	bind_rows(mutate(df_CMI_figureRegion, outcome = "Alcohol")) 


regions <- as.list(unique(df_for_app$name))
outcomes <- as.list(unique(df_for_app$outcome))

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
						 	position = "left",
						 	## make the sidebar
						 	sidebarPanel(
						 		helpText("Data can be shown for several outcomes simultaneously or one at a time by checking the boxes below"),
						 		checkboxGroupInput("var", 
						 											 label = "Choose outcome variables to display", 
						 											 choices = outcomes,
						 											 selected = outcomes[[1]]),
						 		selectInput("regions", 
						 								multiple = TRUE,
						 								label = "Regions to display", 
						 								choices = regions,
						 								selected = regions[[1]]),
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
						 		fluidRow(
						 			column(12, plotOutput("mainplot"))
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
	## define df to plot as reactive
	df_shiny <- reactive({
		dv_shiny <- df_for_app %>% 
			filter(weekDate >= format(input$dates[1]) & weekDate <= format(input$dates[2])) %>% 
			filter(name %in% input$regions) %>%
			filter(outcome %in% input$var) %>%
			mutate_at("name", ~as.factor(.))
	})
	
	## set up reactive values to store plot
	v <- reactiveValues(plot = NULL)
	
	## if runPlot button is pressed then build the plot
	observeEvent(input$runPlot, {
		v$plot <- ggplot(df_shiny(), aes(x=weekDate, y=model_out, group = name, colour = name)) +
			geom_line() +
			xlab("Time") +
			ylab("Proportion Overall") +
			theme_classic() +
			theme(axis.text.x = element_text(angle=60, hjust=1)) +
			#scale_x_continuous(breaks = c(0,52, 104, 156, 167, 208), labels = c("2017","2018","2019","2020","Lockdown","2021")) +
			labs(colour = "Region") +
			facet_wrap(~outcome, ncol = 3) +
			theme_collateral()
	})
	
	output$mainplot <- renderPlot({
		if (is.null(v$plot)) return()
		if(input$lockdownLine) {
			v$plot + geom_vline(xintercept = as.Date("2020-03-28"), linetype = "dashed", col = 2)
		}else{
			v$plot
		}
	})
}
shinyApp(ui, server)
