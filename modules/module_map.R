
# module uebersicht
locations_text <- c("Hub", "Orin's", "By George", "Public Grounds", "Tower", "Rotunda", "Evo", "Microsoft", "Dawg Bites")

map_ui <- function(id) {
  ns <- NS(id)
  
  tagList(

    fluidRow(
      column(12,
             f7Card(
               title = "Optimal Route",
               textOutput(ns("time")),
               f7Gauge(ns("gauge_time"),
                       type = "semicircle",
                       value = 0,
                       labelText = "Time Savings"),
               f7Gauge(ns("gauge_distance"),
                       type = "semicircle",
                       value = 0,
                       labelText = "Distance Savings")
             )),
      
      column(12,
             f7Card(title = "Map",
                    leafletOutput(ns("trip_map"))
             )
      ),

      
      column(12,
             
            
    
             f7Card(title = "Select which Cafés you visit today (1st is starting point)",
        bucket_list(
          header = NULL,
          add_rank_list(
            text = "Cafes on Route",
            labels = locations_text,
            input_id = ns("cafe_routes")
          ),
          add_rank_list(
            text = "Cafes not on route",
            labels = NULL
          )
        )
)
      ),
column(12,
       f7Card(
         title = "Savings",
         textOutput(ns("savings"))
       )),
column(12,
       
       )


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
      
      values$selected_locations <- locations[input$cafe_routes, ]
      
      values$trips <- osrmTrip(loc = values$selected_locations, osrm.profile = "car")
      
      trip <- values$trips[[1]]$trip
      
      trip_stops <- trip %>% 
        mutate(stop = paste0("Stop ", 1:n())) %>% 
        data.frame %>% 
        mutate(stop = case_when(stop == paste0("Stop ", n()) ~ paste0("Start / End"),
                                TRUE ~ stop)) %>% 
        dplyr::select(cafe = end, stop)
      
      locations_df <- values$selected_locations %>% 
        as.data.frame() %>% 
        mutate(cafe = rownames(values$selected_locations)) %>% 
        left_join(trip_stops, by = "cafe")
      
      map <- leaflet() %>% 
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addPolylines(data = trip) %>% 
        addCircleMarkers(lat = locations_df$V2,
                         lng = locations_df$V1,
                         color = "red",
                         label = paste0(locations_df$cafe," \n ", locations_df$stop),
                         stroke = FALSE,
                         radius = 8,
                         fillOpacity = 0.8,
                         labelOptions = labelOptions(noHide = TRUE, offset=c(-12,0), textOnly = FALSE))

    })
    
    output$time <- renderText({
      validate(need(values$trips, ""))
      
      paste0("The new optimal route would take you ", round(values$trips[[1]]$summary$duration, 2), " minutes to complete by car. ",
             "The total distance is ", round(values$trips[[1]]$summary$distance, 2), " kilometers.")
      
    })
    
    output$savings <- renderText({
      validate(need(values$trips, ""))
      validate(need(values$total_trip, ""))
      
      saving_total_duration <- sum(values$total_trip$duration) - values$trips[[1]]$summary$duration
      values$saving_percentage_duration <- saving_total_duration/values$trips[[1]]$summary$duration * 100
      
      
      saving_total_distance <- sum(values$total_trip$distance) - values$trips[[1]]$summary$distance
      values$saving_percentage_distance <- saving_total_distance/values$trips[[1]]$summary$distance * 100

      paste0("This savings are estimated by comparing a tour that would go in the order as they appear in the box to the optimal route. ",
      "\nEstimated percentage in time saved: ", round(values$saving_percentage_duration, 2), "%",
      "\nEstimated percentage in distance saved: ", round(values$saving_percentage_distance, 2), "%")
      
    })
    
    observe({
      validate(need(values$selected_locations, message =""))
      
      total_trip <- data.frame("duration" = numeric(),
                               "distance" = numeric())
      for(i in 1:nrow(values$selected_locations)) {
        if (i == nrow(values$selected_locations)) {
          route <- osrmRoute(src = values$selected_locations[i,],
                             dst = values$selected_locations[1,])
        } else {
          route <- osrmRoute(src = values$selected_locations[i,],
                             dst = values$selected_locations[i + 1,]
          )
        }
        total_trip <- total_trip %>% 
          bind_rows(data.frame("duration" = route$duration,
                               "distance" = route$distance))
      }
      values$total_trip <- total_trip
    })
    
    observeEvent(values$saving_percentage_duration,{
      updateF7Gauge(id = "map-gauge_time",
      value = round(values$saving_percentage_duration, 2 ))
      
      updateF7Gauge(id = "map-gauge_distance",
                    value = round(values$saving_percentage_distance, 2))
      
    })

    
    

  })
}

