#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


#set up for the shiny app
library(shiny)
library(tmap)
library(sf)
library(dplyr)
library(readr)
library(tidyverse)
library(ggthemes)
# read in data

CO_14ers <- read_csv("CO_14ers.csv")


CO_14ers <- st_as_sf(CO_14ers, coords = c("Long", "Lat"), crs = 4326)

###
tmap_mode("view")

tm_shape(CO_14ers) +
  tm_dots(
    col = "Range",
    size = 0.1,
    palette = "Dark2",
    title = "Colorado 14ers",
    popup.vars = c("Range" = "Range",
                   "Elevation (ft)" = "Elevation",
                   "Class" = "Class",
                   "Climbed (y/n)" = "Status",
                   "Peak" = "Peak",
                   "Rank" = "Rank")
  )+
  tmap_options(max.categories = 58)


##
ui <- fluidPage(
  # App title
  titlePanel("Colorado Peaks Over 14,000 ft in Elevation: A Matthew Woodworth Product"),
  
  # Add some informational text using an HTML tag (i.e., a level 5 heading)
  h5(
    "In this app, you can filter the Colorado 14ers by Elevation, Range , Rank, Difficulty Class, and my summit status."
  ),
  
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel for widgets that users can interact with
    sidebarPanel(
      # Input: Select range of 14ers
      checkboxGroupInput(
        inputId = "Range",
        label = "Range",
        choices = list("Elk", "Front", "Mosquito", "San Juan", "Sangre de Cristo", "Sawatch", "Tenmile"),
        selected = c("Elk", "Front", "Mosquito", "San Juan", "Sangre de Cristo", "Sawatch", "Tenmile")
      ),
      
      # Input: Filter points by class
      checkboxGroupInput(
        inputId = "Class",
        label = "Class",
        choiceNames = list("1", "2", "3", "4"),
        choiceValues = list("1", "2", "3", "4"),
        selected = c("1", "2", "3", "4")
      ),
      
      # Input: Filter by elevation
      sliderInput(
        inputId = "Elevation",
        label = "Elevation (ft)",
        min = 14004,
        max = 14438,
        value = c(14004, 14438)
      ),
      
      # Input: Filter by rank
      sliderInput(
        inputId = "Rank",
        label = "Filter by Rank",
        min = 1,
        max = 58,
        value = c(1, 58)
      ),
      
      # Input: Filter by summit status
      checkboxGroupInput(
        inputId = "Status",
        label = "Climbed (y/n)",
        choices = list("yes", "no"),
        selected = c("yes", "no")
      ),
      
    ),
    
    mainPanel(
      tmapOutput("map", height = "700px")),
  ),
    
  
  fluidRow(
    column(8, 
           h5("Summary Statistics"),  # Title for the section
           plotOutput("status_plot"),  # Adjust height for plots
           plotOutput("range_plot"),
           plotOutput("class_plot")

    )
  )
)

  



# Server
server <- function(input, output) {
  # Set tmap mode to interactive
  tmap_mode("view")
  
  # Make a reactive object for the CO 14er data by calling inputIDs to extract the values the user chose
  
  CO14er_react <- reactive({
    CO_14ers %>%
      filter(
        Range %in% input$Range,
        Elevation >= input$Elevation[1],
        Elevation <= input$Elevation[2],
        Rank >= input$Rank[1],
        Rank <= input$Rank[2],
        Class %in% input$Class,
        Status %in% input$Status
      )
  })
  
  
  # Render the map based on our dataset
  output$map <- renderTmap({
    tm_shape(CO14er_react()) +
      tm_dots(
        col = "Range",
        size = 0.1,
        palette = "Dark2",
        title = "Mountain Range",
        id = "Peak",
        popup.vars = c(
          "Peak" = "Peak",
          "Range" = "Range",
          "Elevation (ft)" = "Elevation",
          "Class" = "Class",
          "Climbed (y/n)" = "Status",
          "Rank" = "Rank"
        )
      ) +
      tm_basemap("Esri.WorldTopoMap") +  # Change to the desired basemap
      tmap_options(max.categories = 58)
  })
  # Reactive summary for the plot
  summary_data <- reactive({
    CO14er_react() %>%
      group_by(Status) %>%
      summarize(Count = n()) %>%
      mutate(Percentage = Count / sum(Count) * 100)
  })
  
  # Render the plot
  output$status_plot <- renderPlot({
    summary <- summary_data()
    
    ggplot(summary, aes(x = Status, y = Percentage, fill = Status)) +
      geom_bar(stat = "identity") +
      labs(
        title = "Percentage of 14ers Climbed",
        x = "Summit Status",
        y = "Percentage"
      ) +
      theme_linedraw() +
      scale_fill_manual(values = c("yes" = "navy", "no" = "red"),
                        name = "Climbed (y/n)")+
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),  # Center title and increase size
        legend.title = element_text(size = 14),  # Set legend title size
        legend.text = element_text(size = 12)    # Set legend text size
      )
  })
  

  #Class Plot
  output$class_plot <- renderPlot({
    summary_class <- CO_14ers %>%
      group_by(Class) %>%
      summarize(
        Total = n(),
        Climbed = sum(Status == "yes"),
        Percentage = (Climbed / Total) * 100
      )
    
    # Class Plot
    summary_class$Class <- as.factor(summary_class$Class)
    
    ggplot(summary_class, aes(x = Class, y = Percentage, fill = Class)) +
      geom_bar(stat = "identity") +
      labs(
        title = "Percentage of 14ers Climbed by Class",
        x = "Class",
        y = "Percentage Climbed"
      ) +
      theme_linedraw() +
      scale_fill_manual(values = c("1" = "darkgreen", "2" = "orange", "3" = "darkorange", "4" = "red")) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  })
  
  #Range Plot
  output$range_plot <- renderPlot({
    summary_range <- CO_14ers %>%
      group_by(Range) %>%
      summarize(
        Total = n(),
        Climbed = sum(Status == "yes"),
        Percentage = (Climbed / Total) * 100
      )
    
    # Make sure 'Range' is a factor
    summary_range$Range <- as.factor(summary_range$Range)
    
    ggplot(summary_range, aes(x = Range, y = Percentage, fill = Range)) +
      geom_bar(stat = "identity") +
      labs(
        title = "Percentage of 14ers Climbed by Range",
        x = "Range",
        y = "Percentage Climbed"
      ) +
      theme_linedraw() +
      scale_fill_manual(values = c("Elk" = "#1B9E77", "Front" = "#D95F02",
                                   "Mosquito" = "#7570B3", "San Juan" = "#E7298A",
                                   "Sangre de Cristo" = "#66A61E",
                                   "Sawatch" = "#E6AB02", "Tenmile" = "#A6761D")) +  # Adjust colors as needed
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
