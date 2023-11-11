library(shiny)
library(bs4Dash)
library(leaflet)
library(leaflet.extras)
library(osrm)
library(sf)
library(sortable)
library(dplyr)
library(bslib)
library(bsicons)


source("modules/module_map.R")

shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(
      disable = TRUE,

      title = dashboardBrand(
        title = "Tour De Cafes",
        color = "primary",
        href = "",
        image = "bialetti.png"
      )
    ),
    sidebar = dashboardSidebar(collapsed = TRUE),
    body = dashboardBody(
      
      uiOutput("page")

    ),
    controlbar = dashboardControlbar(),
    title = "DashboardPage"
  ),
  server = function(input, output) {
    
    output$page <- renderUI({
      map_ui("map")
    })
    
    map_server("map")
  }
)