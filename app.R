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
library(googleAuthR)
library(config)

config <- config::get()

source("modules/module_map.R")


options(googleAuthR.webapp.client_id = config$google_client_id)


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
    sidebar = dashboardSidebar(collapsed = TRUE,
                               googleSignInUI("demo")),
    body = dashboardBody(
      
      uiOutput("page")

    ),
    controlbar = dashboardControlbar(),
    title = "DashboardPage"
  ),
  server = function(input, output) {
    
    
    sign_ins <- googleSignIn("demo")
    
    output$g_name = renderText({ sign_ins()$name })
    
    output$page <- renderUI({
      map_ui("map")
    })
    
    map_server("map")
  }
)