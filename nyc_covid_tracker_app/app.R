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
library(sf)
library(leaflet)
library(htmlwidgets)
library(BAMMtools)
library(htmltools)
library(scales)
library(rsconnect)

# # set working directory
# setwd("/Users/justinwilliams/projects/nyc_covid_tracker/nyc_covid_tracker_app/")

# read in df's
all_sf <- readRDS("all_sf.rds")
summary_totals <- readRDS("summary_totals.Rds")
casesbyday_cum <- as.data.frame(readRDS("casesbyday_cum.Rds"))

# define map start point
start_point <- as.Date("2022-01-01",
                       format = "%Y-%m-%d")

# obtain summary numbers
#case count
case_count <- 
  summary_totals[summary_totals$MEASURE == 
                   "NYC_TOTAL_CASE_COUNT",]$NUMBER_OF_NYC_RESIDENTS

# death count
death_count <- 
  summary_totals[summary_totals$MEASURE == 
                   "NYC_TOTAL_DEATH_COUNT",]$NUMBER_OF_NYC_RESIDENTS

# date
current_date <- 
  summary_totals[summary_totals$MEASURE == 
                   "DATE_UPDATED",]$NUMBER_OF_NYC_RESIDENTS


# Define UI for application 
ui <- fluidPage(
  
    # Application title
    titlePanel("COVID-19 NYC Tracker by Zip Code"),
    
    fluidRow(
      column(4,
          span(tags$i(h6("All data sourced from the NYC Department of Health\n
                            and Mental Hygiene (DOHMOH) ",
                    a(href="https://github.com/nychealth/coronavirus-data",
                                       "data repository.", target="_blank")))),
          span(h3(paste0(prettyNum(case_count, big.mark = ",")," cases")), 
             align = "right",style="color:#045a8d"),
          span(h4(paste0(prettyNum(death_count, big.mark = ",")," deaths")),  
             align = "right", style="color:#045a8d"),
          span(h6(prettyNum(current_date)), align = "right"),
          plotOutput("case_count", height="145px", width="100%"),
          plotOutput("cumulative", height="145px", width="100%"),
          sliderInput(inputId = "date",
                    label = "Select mapping date (week ending in):",
                    min = as.Date(min(all_sf$week_ending)),
                    max = as.Date(max(all_sf$week_ending)),
                    value = start_point,
                    step = 7,
                    timeFormat = "%b %d-'%y",
                    width = "100%",
                    animate = 
                      animationOptions(interval = 3000,
                                       playButton = c("Play"),
                                       pauseButton = c("Pause"),
                                       loop = T)),
span(tags$i(h6("*Mapping data is aggregated by week, seperated by week ending 
               date and begins August 2020"))),
        ),
        # define panels
      column(width =  8,
        mainPanel(
          tabsetPanel(
            tabPanel("Case Rate", 
                     column(12,leafletOutput(outputId = "cases",
                                             height = "80vh",
                                             width = "60vw"))),
            tabPanel("Test Rate", 
                     column(12,leafletOutput(outputId = "tests",
                                            height = "80vh",
                                             width = "60vw"))),
            tabPanel("Percent Positive", 
                     column(12,leafletOutput(outputId = "pctpos",
                                             height = "80vh",
                                             width = "60vw")))
          )
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
    
  output$case_count <- renderPlot({
    (case_count_plot <-  casesbyday_cum %>% 
      ggplot(aes(date, case_count)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      scale_y_continuous(labels = number_format(scale = .0001,
                                                suffix = "k")) +
      scale_x_continuous(labels = NULL) +
      labs(y = "case count", x = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
            axis.text = element_text(size = 8)))
  }, res = 96)
  
  output$cumulative <- renderPlot({
    (casesbyday_cum_plot <- casesbyday_cum %>% 
      ggplot(aes(date, case_cum)) +
      geom_line(lwd = 2, alpha = 0.8) +
      geom_point(aes(casesbyday_cum[casesbyday_cum$date == input$date,]$date,
                    casesbyday_cum[casesbyday_cum$date == input$date,]$case_cum),
                  color = "#A63446", size = 3, alpha = 0.8) +
      scale_y_continuous(labels = number_format(scale = .0001,
                                                suffix = "k")) +
      scale_x_date(date_breaks = "2 month",
                   date_labels = "%b-'%y") +
      labs(y = "cumulative cases", x = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
            axis.text = element_text(size = 8)))
    
  }, res = 96)

  output$cases <- renderLeaflet( {
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
    bins <- getJenksBreaks(all_sf$testrate, 10) # get natural breaks
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
      week_zcta()$label, round(week_zcta()$testrate)) %>% 
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
    bins <- getJenksBreaks(all_sf$percpos, 10) # get natural breaks
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
      week_zcta()$label, round(week_zcta()$percpos)) %>% 
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
#shinyApp(ui, server)
#deployApp(account="justinm0rgan")


