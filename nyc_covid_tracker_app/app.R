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
library(htmlwidgets)
library(BAMMtools)
library(htmltools)

setwd("/Users/justinwilliams/projects/nyc_covid_tracker/nyc_covid_tracker_app/")
all_sf <- readRDS("all_sf.rds")

# define map start point
start_point <- as.Date("2022-01-01",
                       format = "%Y-%m-%d")

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Covid-19 NYC Trends by Modified ZCTA 
               (August 2020 - present)"),
    # Sidebar with date input
    sidebarLayout(
        sidebarPanel(
          tags$a(href="https://github.com/nychealth/coronavirus-data",
                 "Data Repository", target="_blank"),
          h5("Data is aggregated by week, seperated by week ending date.
             All data sourced from the NYC Department of Health."),
            sliderInput(inputId = "date",
                        label = "Select a date (week ending in):",
                        min = as.Date(min(all_sf$week_ending)),
                        max = as.Date(max(all_sf$week_ending)),
                        value = start_point,
                        step = 7,
                        timeFormat = "%b %d-'%y"
                        # choices = unique(all_sf$week_ending)
            )
        ),
        # define panels
        mainPanel(
          tabsetPanel(
            tabPanel("Case Rate", leafletOutput(outputId = "cases",
                                                height = 450)),
            tabPanel("Test Rate", leafletOutput(outputId = "tests",
                                                height = 450)),
            tabPanel("Percent Positive", leafletOutput(outputId = "pctpos",
                                                       height = 450))
          )
        )
    )
)
# Define server logic required to draw maps
server <- function(input, output) {
  
    week_zcta <- reactive({
      w <- all_sf %>% filter(week_ending == input$date)
      return(w)
    })

  output$cases <- renderLeaflet({
    # set bins
    bins <- getJenksBreaks(all_sf$caserate, 9) # get natural breaks
    bins <- map(.x = bins, round) %>% 
      unlist() # round
    
    # set color palette
    pal <- colorBin(palette = "PuBu",
                    bins = bins, 
                    domain = all_sf$caserate)
    
    # set labels
    labels <- sprintf(
      "<h5>%s</h5><br/><strong>Zip Code: </strong>%s<br/><strong>Cases per 100,000 people: </strong>%g ",
      week_zcta()$modzcta_name,
      week_zcta()$label, round(week_zcta()$caserate)) %>% 
      lapply(HTML)
    
    week_zcta() %>% 
      st_transform(4326) %>% 
      leaflet() %>%
      setView(lng = -73.9,lat = 40.705,zoom = 10) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(label = labels,
                  weight = 0.25,
                  color = "white",
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~pal(week_zcta()$caserate),
                  highlightOptions = highlightOptions(weight = 1,
                                                      color = "#666",
                                                      fillOpacity = 0.7,
                                                      bringToFront = T)) %>% 
      addLegend("bottomright",
                pal = pal,
                values = ~caserate,
                title = "Cases per 100,000",
                opacity = 0.7)
    
  })
  
  output$tests <- renderLeaflet({
    # set bins
    bins <- getJenksBreaks(all_sf$testrate, 9) # get natural breaks
    bins <- map(.x = bins, round) %>%
      unlist()# round
    
    # set color palette
    pal <- colorBin(palette = "YlGn",
                    bins = bins, 
                    domain = all_sf$testrate)
    
    # set labels
    labels <- sprintf(
      "<h5>%s</h5><br/><strong>Zip Code: </strong>%s<br/><strong>Positive Tests per 100,000 people: </strong>%g ",
      week_zcta()$modzcta_name,
      week_zcta()$label, round(week_zcta()$caserate)) %>% 
      lapply(HTML)
    
    week_zcta() %>% 
      st_transform(4326) %>% 
      leaflet() %>%
      setView(lng = -73.9,lat = 40.705,zoom = 10) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(label = labels,
                  weight = 0.25,
                  color = "white",
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~pal(week_zcta()$testrate),
                  highlightOptions = highlightOptions(weight = 1,
                                                      color = "#666",
                                                      fillOpacity = 0.7,
                                                      bringToFront = T)) %>% 
      addLegend("bottomright",
                pal = pal,
                values = ~testrate,
                title = "Tests per 100,000",
                opacity = 0.7)
    
  })
  
  output$pctpos <- renderLeaflet({
    # set bins
    bins <- getJenksBreaks(all_sf$percpos, 9) # get natural breaks
    bins <- map(.x = bins, round) %>% 
      unlist() # round
    
    # set color palette
    pal <- colorBin(palette = "OrRd",
                    bins = bins, 
                    domain = all_sf$percpos)
    
    # set labels
    labels <- sprintf(
      "<h5>%s</h5><br/><strong>Zip Code: </strong>%s<br/><strong>Count of Positive Test results: </strong>%g ",
      week_zcta()$modzcta_name,
      week_zcta()$label, round(week_zcta()$caserate)) %>% 
      lapply(HTML)
    
    week_zcta() %>% 
      st_transform(4326) %>% 
      leaflet() %>%
      setView(lng = -73.9,lat = 40.705,zoom = 10) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(label = labels,
                  weight = 0.25,
                  color = "white",
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~pal(week_zcta()$percpos),
                  highlightOptions = highlightOptions(weight = 1,
                                                      color = "#666",
                                                      fillOpacity = 0.7,
                                                      bringToFront = T)) %>% 
      addLegend("bottomright",
                pal = pal,
                values = ~percpos,
                title = "% Positive of<br>people tested",
                opacity = 0.7,
                labFormat = labelFormat(suffix = "%"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
