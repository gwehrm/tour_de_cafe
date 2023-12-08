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
library(shinyjs)
library(shinyMobile)
library(tibble)
library(apexcharter)
library(dplyr)
library(ggplot2)

library(ggmap)




config <- config::get()

source("modules/module_map.R")
source("modules/module_own_route.R")

library(shiny)
library(shinyMobile)
library(apexcharter)
library(shinyWidgets)




shinyApp(
  ui = f7Page(
    title = "Tour De Cafés",
    options = list(dark = FALSE),
    f7TabLayout(
      navbar = f7Navbar(
        title = "Tour De Cafés",
        hairline = TRUE,
        shadow = TRUE,
        leftPanel = FALSE,
        rightPanel = FALSE
      ),
      f7Tabs(
        animated = TRUE,
        f7Tab(
          tabName = "Tab1",
          icon = f7Icon("arrow_swap"),
          active = TRUE,
  
          map_ui("map")),
          
        f7Tab(
          tabName = "Tab2",
          icon = f7Icon("pencil"),
          active = FALSE,
          own_route_ui("own_route")
        )
      )
    )
  ),
  server = function(input, output, session) {
    map_server("map")
    own_route_server("own_route")
  }
)


