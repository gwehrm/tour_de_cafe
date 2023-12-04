
# module uebersicht
own_route_ui <- function(id) {
  ns <- NS(id)
  
    fluidRow(
      column(12,
             f7Card(
               id = "optimal_route_explanation",
               title = "Optimal Route",
               textOutput(ns("explanation"))
             )),
      column(12,
             f7Text(ns("lat_long"), label = "Latitude, Longitude", placeholder = "(e.g.: 47.61, -122.21)")
      ),
      column(12,
             f7Text(ns("label"), label = "Label", placeholder = "My favorite bar")
      ),
      column(12,
             f7Button(ns("add"), label = "Add Location")),
      br(),
      column(12,
             uiOutput(ns("list"))
             
      ),
      br(),
      column(3,
             f7Button(ns("reset"), label = "Reset", color = "red")),
      br(),
      column(12,
             uiOutput(ns("new_export_button"))
             ),
      br(),
      column(12,
             f7Card(title = "Map",
                    leafletOutput(ns("trip_map"))
             )
      )
          
                    )
  
}

own_route_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    values <- reactiveValues(location = c())
    
    observeEvent(input$add, {
      
      tryCatch(expr = {

        lat_long <- strsplit(input$lat_long, ",")
        lat <- as.numeric(lat_long[[1]][1])
        long <- as.numeric(lat_long[[1]][2])
        

        if (nchar(input$label) == 0) {
          f7Dialog(
            id = ns("check"),
            type ="alert",
            title = "Choose Label",
            text = "Please describe your stop."
          )
          
          
        } else {
          if(abs(as.numeric(lat)) > 90 |
             abs(as.numeric(long)) > 180) {
            
            f7Dialog(
              id = ns("check"),
              type ="alert",
              title = "Check Coordinates",
              text = "Your coordinates do not follow the required format. Check again."
            )
            
            
          } else {
            values$location <- rbind(values$location, 
                                     c(as.numeric(lat_long[[1]][2]), as.numeric(lat_long[[1]][1])))
            rownames(values$location)[nrow(values$location)] <- input$label
            
            
            updateF7Text("lat_long", value = "")
            updateF7Text("label", value = "", placeholder = "What is your next entry?")
          }
          
          
          
        }
      
        
       
       
        

        
        
      },
      error = function(cond) {
        f7Dialog(
          id = ns("check"),
          type ="alert",
          title = "Check Entries",
          text = "Your coordinates are wrong or you have no label. Please check again."
        )
        
      }
      )
      
    })
    
    observeEvent(input$reset, {
      f7Dialog(
        id = ns("really_reset"),
        type ="confirm",
        title = "Reset",
        text = "Do you want to reset your locations?"
      )
    })
    
    observeEvent(input$really_reset, {
      values$location <- c()
    })
    
    output$trip_map <- renderLeaflet({
      
      validate(need(nrow(values$location) > 1, "choose more than one location"))
      
      values$trips <- osrmTrip(loc = values$location, osrm.profile = "car")
      
      trip <- values$trips[[1]]$trip
      
      trip_stops <- trip %>% 
        mutate(stop = paste0("Stop ", 1:n())) %>% 
        data.frame %>% 
        mutate(stop = case_when(stop == paste0("Stop ", n()) ~ paste0("Start / End"),
                                TRUE ~ stop)) %>% 
        dplyr::select(cafe = end, stop)
      
      locations_df <- values$location %>% 
        as.data.frame() %>% 
        mutate(cafe = rownames(values$location)) %>% 
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
    
    output$list <- renderUI({
      validate(need(nrow(values$location) > 0, message = ""))

      tagList(
        "Your entries: ",
        f7List(
          lapply(1:nrow(values$location), function(j) {
            f7ListItem(paste0(rownames(values$location)[j])," (", values$location[j,][1], values$location[j,][2], ")")
          })
        )
      )
      
      
    })
    
    output$new_export_button <- renderUI({
      validate(need(values$trips, ""))
      
      order <- values$trips[[1]]$trip$start
      
      link <- "https://google.com/maps/dir/"
      
      
      for(stop in order) {
        link <- paste0(link,"/", values$location[stop,][2],",", values$location[stop,][1])
      }
      # back to start
      link <- paste0(link,"/", values$location[1,][2],",", values$location[1,][1])
      
      f7Button(label = "Export optimal Route to Google for Directions",
               href = link
               )
      
      
      
      
    })
    
    output$explanation <- renderText({
      validate(need(values$trips, ""))
      
      
      paste0("Add your own locations and calculate your optimal route! 
      
             
             The new optimal route would take you ", round(values$trips[[1]]$summary$duration, 2), " minutes to complete by car. ",
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
    
    # output$export_google <- renderText({
    #   link <- "https://google.com/maps/dir/"
    #   
    #   if(nrow(values$trips[[1]]$trip) == 0) {
    #     ""
    #   } else {
    #     browser()
    #     
    #     order <- values$trips[[1]]$trip$start
    #     
    #     for(stop in order) {
    #       link <- paste0(link,"/", values$location[stop,][2],",", values$location[stop,][1])
    #     }
    #     # back to start
    #     link <- paste0(link,"/", values$location[1,][2],",", values$location[1,][1])
    #     
    #     paste0('<a href="',link,'" target="_blank>Google Maps</a>')
    # }
    #   
    # })

    
    
    
    
  })
}

