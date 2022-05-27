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

setwd("/Users/justinwilliams/projects/nyc_covid_tracker/nyc_covid_tracker_app/")
all_sf <- readRDS("all_sf.rds")

# Define UI for application that draws a histogram
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
            selectInput("date",
                        "Select a date (week ending in):",
                        choices = unique(all_sf$week_ending)
            )
        ),
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Case Rate", leafletOutput("cases")),
            tabPanel("Test Rate", leafletOutput("tests")),
            tabPanel("Percent Positive", leafletOutput("pctpos"))
          )
        )
    )
  )
# Define server logic required to draw a histogram
server <- function(input, output) {
  
    week_zcta <- reactive({
      w <- all_sf %>% filter(week_ending == input$date)
      return(w)
    })

  output$cases <- renderLeaflet({
    # set bins
    bins <- getJenksBreaks(all_sf$caserate, 10) # get natural breaks
    bins <- map(.x = bins, round) %>% 
      unlist() # round
    
    # set color palette
    pal <- colorBin(palette = "PuBu",
                    bins = bins, 
                    domain = all_sf$caserate)
    
    # set labels
    labels <- sprintf(
      "<strong>%s</strong><br/>%g cases per 100,000 people",
      week_zcta()$MODZCTA, round(week_zcta()$caserate)) %>% 
      lapply(htmltools::HTML)
    
    week_zcta() %>% 
      st_transform(4326) %>% 
      leaflet() %>%
      setView(lng = -73.9,lat = 40.7,zoom = 10) %>% 
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
    bins <- getJenksBreaks(all_sf$testrate, 10) # get natural breaks
    bins <- map(.x = bins, round) %>% 
      unlist() # round
    
    # set color palette
    pal <- colorBin(palette = "YlGn",
                    bins = bins, 
                    domain = all_sf$testrate)
    
    # set labels
    labels <- sprintf(
      "<strong>%s</strong><br/>%g cases per 100,000 people",
      week_zcta()$MODZCTA, round(week_zcta()$testrate)) %>% 
      lapply(htmltools::HTML)
    
    week_zcta() %>% 
      st_transform(4326) %>% 
      leaflet() %>%
      setView(lng = -73.9,lat = 40.7,zoom = 10) %>% 
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
    bins <- getJenksBreaks(all_sf$percpos, 10) # get natural breaks
    bins <- map(.x = bins, round) %>% 
      unlist() # round
    
    # set color palette
    pal <- colorBin(palette = "OrRd",
                    bins = bins, 
                    domain = all_sf$percpos)
    
    # set labels
    labels <- sprintf(
      "<strong>%s</strong><br/>%g cases per 100,000 people",
      week_zcta()$MODZCTA, round(week_zcta()$percpos)) %>% 
      lapply(htmltools::HTML)
    
    week_zcta() %>% 
      st_transform(4326) %>% 
      leaflet() %>%
      setView(lng = -73.9,lat = 40.7,zoom = 10) %>% 
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
                title = "% Positive per 100,000",
                opacity = 0.7)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
