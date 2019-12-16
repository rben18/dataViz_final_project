#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(KernSmooth)
library(raster)

#Read the phone call log and separating the "call_Date" date-time variable into 2 columns.
phone_call_log <- read_csv("311_Phone_Call_Log_Mod.csv")%>%
    rename(Call_Date_Time = "Call_Date", "Duration_Minutes" = "Duration__Minutes_") %>%
    dplyr::select(-"Work_Order_Type")%>%
    #Transform the call_date_time variable and then splot
    mutate(Call_Date_Time = as.POSIXct(Call_Date_Time, format="%Y:%m:%d %H:%M:%S"), 
           Time = format(Call_Date_Time,"%H:%M"),
           Date = as.Date(Call_Date_Time)
    )%>%
    dplyr::select(FID, Date, Time, Department, Called_About, Duration_Minutes, duration_Seconds)

parks.points <- readRDS("parks.points.rds")
cost <- c(13750,3000,3500,97750,24600,10833.33,26500,350601.13,180600,41250,11600,22750,109750,21000,25100,2000,11600,1233.33,11583.34,15100,350000,715600,285083.34,700000,26250,1233.33,2000,166383.34,216808.38,110916.68,61250,62750,17950,737200,128000,151666.68,87583.34,95333.34,158608.33,31583.34,26250,44916.68,3500,6250,26100,58208.33,32250,106583.33,39850,11600,39250,26250,18242.8,36576.14,6250,135650,72375,41583.34,92916.68,18600,3000,3000
)
parks.points$cost <- cost

# Loading the code violation dataset and filtering it to the public parking violations
code.points <- read.csv("Code_Enforcement_Cases.csv") 
code.points <- dplyr::filter(code.points, Case_Type_Code_Description == "VEHICLE-PUBLIC")

# Compute A 2D Binned Kernel Density Estimate
kde <- bkde2D(code.points[, 13:14],
              bandwidth=c(.0045, .0068), gridsize = c(1000,1000))

# Create Raster from Kernel Density output
KernelDensityRaster <- raster(list(x=kde$x1, y=kde$x2, z = kde$fhat))

# Create pal function for coloring the raster
KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 1)] <- NA
palRaster <- colorBin("Spectral", bins = 5, domain = KernelDensityRaster@data@values, na.color = "transparent")

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Team 2 Final Project"),
    h4("Park Management Dashboard"),
    
    tabsetPanel(
        tabPanel("Rodrigo",
                 sidebarLayout(
                     sidebarPanel(width = 3,
                                  selectInput(inputId = "department",
                                              label = "Choose department to focus on",
                                              choices = sort(unique(phone_call_log$Department)),
                                              selected = min(sort(phone_call_log$Department))
                                              ),
                                  sliderInput(inputId = "duration",
                                              label = "Duration of call (in seconds):",
                                              min = 0,
                                              max = max(phone_call_log$duration_Seconds,na.rm = TRUE),
                                              value = c(0,max(phone_call_log$duration_Seconds,na.rm = TRUE))
                                              ),
                                  dateRangeInput(inputId = "dates_selected",
                                                 label = "Choose a desired date range",
                                                 start = min(phone_call_log$Date),
                                                 end = max(phone_call_log$Date),
                                                 min = min(phone_call_log$Date),
                                                 max = max(phone_call_log$Date)
                                                 ),
                                  textOutput("error_message"),
                                  tags$head(tags$style("#error_message{color: red;font-size: 20px;}")
                                            )
                                  ),
                     # Show a plot of the generated distribution
                     mainPanel(
                         tabsetPanel(
                             tabPanel(title = "Graphical Form",
                                      fluidRow(
                                          column(6, 
                                                 h4("Graph below shows the number of times people call for a particular problem in selected department"),
                                                 plotOutput("called_about_plot")
                                          ),
                                          column(6,
                                                 h4("Graph below shows the proportion of calls at a given duration"),
                                                 plotOutput(outputId = "duration_plot"))
                                      )
                             ),
                             tabPanel(title = "Tabular Form",
                                      DT::dataTableOutput("phone_data")
                                      )
                             )
                         )
                     )
                 ),
        tabPanel("Raj",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(inputId = "park_type", "Type of Park:",
                                     choices=unique(parks.points$Park_Type)),

                         helpText("Data: Parks_Locations_and_Features")
                     ),
                     # Show a plot of the Parks in the Map
                     mainPanel(
                         h3("Parks in South Bend, IN"),
                         leafletOutput("ParkMap1")
                         )
                     )
                 ),
        tabPanel("Evan",
                     sidebarLayout(
                         sidebarPanel(
                                    sliderInput("slider2", "Cost per Year (Bubble Size = Cost)",
                                                min = min(parks.points$cost), max = max(parks.points$cost), value = c(1, 12))
                             ),
                     mainPanel(
                         h3("Park Maintenance Cost in the South Bend, IN"),
                         leafletOutput("ParkMap2")
                         )
                     )
                 ),
        tabPanel("Charle",
                     mainPanel(
                         h3("Parking Violations Density in South Bend"),
                         leafletOutput("map")
                         )
                 )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    ######
    #Rodrigo Section:
    #This recative section will filter the phone log depending on the values on the inputs in the sidebar
    filtered_phone_log <- reactive({
        phone_call_log %>%
            filter(duration_Seconds >= input$duration[1] & duration_Seconds <= input$duration[2],
                   Date >= input$dates_selected[1] & Date <= input$dates_selected[2],
                   Department == input$department)
    })
    
    #This is the data table of the filtered phone log to display on the second tab.
    output$phone_data <- DT::renderDataTable(
        DT::datatable(filtered_phone_log(),
                      colnames = c("Called About" = 'Called_About', "Duration Minutes" = 'Duration_Minutes', 
                                   "Duration Seconds" = 'duration_Seconds')
        )
        
    )
    
    #This is will render a bar plot with a count of the distinct things people called about.
    output$called_about_plot <- renderPlot(
        ggplot(data = filtered_phone_log(), aes (x = Called_About, fill = Called_About)) +
            geom_bar() + 
            labs(x = "What People Called About", y = "Count") + 
            coord_flip() + 
            guides(fill = "none") +
            theme_classic()
    )
    
    #This is the data used for the density plot. This is the same as the filtered phone log, except that the calls 
    #in the top 99 percentile duration_Second are filtered out into to have a prettier graph and less outliers.
    data_for_plot <- reactive({
        filtered_phone_log() %>%
            filter(quantile(duration_Seconds,.99,na.rm = T) > duration_Seconds)
    })
    
    #Renders the density plot for duration seconds.
    output$duration_plot <- renderPlot(
        ggplot(data = data_for_plot(), aes(x = duration_Seconds)) + 
            geom_density(fill="blue") +
            labs(x = "Duration (seconds)", y = "Density", 
                 title = paste("Median duration: ", round(median(data_for_plot()$duration_Seconds,na.rm = TRUE)), " seconds"),  
                 caption = "Only the shortest 99% of calls were plotted") + 
            theme_classic() +
            geom_vline(aes(xintercept=median(duration_Seconds,na.rm = TRUE)), #plots the median line
                       color="orange", linetype="dashed", size=2)
    )
    #Outputs and error message in case there is no data for the given parameters.
    output$error_message <- renderText(
        if(nrow(data_for_plot()) == 0){
            "There is no data that meets these parameters.\n Please change the input(s)"
        }
        else{
            ""
        }
    )
    
    #######
    #Raj Section:
    selectedType <- reactive({
        parks.points[parks.points$Park_Type == input$park_type,]
    })

    output$ParkMap1 <- renderLeaflet({
        leaflet(options = leafletOptions()) %>%
            addTiles() %>%
            addMarkers(selectedType()$Lon, selectedType()$Lat, popup = selectedType()$Park_Name)


    })
    
    ######
    #Evan section:
    filtered_data <- reactive({
        parks.points[parks.points$cost<=input$slider2[2]
                                           & parks.points$cost>=input$slider2[1],]
        })

    output$ParkMap2 <- renderLeaflet({
        leaflet(data=filtered_data())  %>%
            addTiles() %>%
            addCircleMarkers(stroke = FALSE,color = "red", fillOpacity = 0.5, radius=(filtered_data()$cost+10000)/10000, popup = paste(filtered_data()$Park_Name,"<br>","$",filtered_data()$cost,"<br>","Type:",filtered_data()$Park_Type
            ))
    })
    
    #Charle Section
    #####
    
    output$map <- renderLeaflet({
        leaflet(code.points) %>% 
            addTiles() %>%
            fitBounds(~min(Lon), ~min(Lat), ~max(Lon), ~max(Lat))%>%
            addRasterImage(KernelDensityRaster, 
                           colors = palRaster, 
                           opacity = .5) %>%
            addLegend(pal = palRaster, 
                      values = KernelDensityRaster@data@values, 
                      title = "Density of Violations",
                      position = "bottomright")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
