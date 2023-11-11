
# module uebersicht
locations_text <- c("Hub", "Orin's", "By George", "Public Grounds", "Tower", "Rotunda", "Evolutionary Grounds", "Microsoft", "Dawg Bites")

map_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      column(6,
             value_box(
               title = "Duration",
               value = textOutput(ns("time")),
               showcase = bs_icon("clock"))),
      column(6,
             value_box(
               title = "Distance",
               value = textOutput(ns("distance")),
               showcase = bs_icon("arrows"))
      )
    ),
    
    box(title = "Adjust your route (1st is starting point)",
        type = "success",
        width = 12,
        bucket_list(
          header = NULL,
          add_rank_list(
            text = "Cafes on Route",
            labels = locations_text,
            input_id = ns("cafe_routes")
          ),
          add_rank_list(
            text = "Ignored",
            labels = NULL
          )),
        collapsible = TRUE),

    box(title = "Map",
        type = "success",
        width = 12,
        collapsible = TRUE,
        leafletOutput(ns("trip_map"))
        )

           

    
  )

}

map_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    values <- reactiveValues()
    
  
    output$trip_map <- renderLeaflet({
      
      validate(need(length(input$cafe_routes) > 1, "choose more than one location"))
      
      locations <- rbind(c(-122.30509832425018, 47.65542988450092), # Hub
                         c(-122.30886785784372, 47.658680008298255), #Orins
                         c(-122.31138509668133, 47.6568385754671), #BG
                         c(-122.31047443011094, 47.657380583805704), # PG
                         c(-122.31580503427273, 47.66061103846286), # Tower
                         c(-122.31030121582047, 47.65047971081857), # Rotunda
                         c(-122.30974950271712, 47.65226358173898), #Evo
                         c(-122.30515784870903, 47.65305289588021), #MCSFT
                         c(-122.30252583500187, 47.65368750630508) # DB
      )
      
      rownames(locations) <- locations_text
      
      locations <- locations[input$cafe_routes, ]
      
      values$trips <- osrmTrip(loc = locations, osrm.profile = "car")
      
      trip <- values$trips[[1]]$trip
      
      trip_stops <- trip %>% 
        mutate(stop = paste0("Stop ", 1:n())) %>% 
        data.frame %>% 
        mutate(stop = case_when(stop == paste0("Stop ", n()) ~ paste0("Start / End"),
                                TRUE ~ stop)) %>% 
        dplyr::select(cafe = end, stop)
      
      locations_df <- locations %>% 
        as.data.frame() %>% 
        mutate(cafe = rownames(locations)) %>% 
        left_join(trip_stops, by = "cafe")
      
      map <- leaflet() %>% 
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addPolylines(data = trip) %>% 
        addCircleMarkers(lat = locations_df$V2,
                         lng = locations_df$V1,
                         color = "red",
                         label = locations_df$stop,
                         stroke = FALSE,
                         radius = 8,
                         fillOpacity = 0.8,
                         labelOptions = labelOptions(noHide = TRUE, offset=c(-12,0), textOnly = FALSE))

    })
    
    output$time <- renderText({
      validate(need(values$trips, ""))
      paste0("The trip takes ", round(values$trips[[1]]$summary$duration, 2), " mins to complete without stops")
      
    })
    
    output$distance <- renderText({
      validate(need(values$trips, ""))
      paste0("The trips distance is ", round(values$trips[[1]]$summary$distance, 2), " kilometers")
      
    })
    


  })
}

