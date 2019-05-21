###################
# server #
###################


# Server #### 
server <- shinyServer(function(input, output, session) {
  
  # Base map ####
  output$map<-renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  # data_to_map ####
  data_to_map=reactive({
    con_selected=switch(input$movements, 
                        "emigration" = EM,
                        "immigration" = IM,
                        "net_immigration" = Net_IM,
                        "net_emigration" = Net_EM,
                        "total_migration"=EM_IM)
    
    data_to_map=admin_poly
    data_to_map@data=collect(admin%>%
                               left_join(con_selected,
                                         by=c("ISO_NODE")))
    
    return(data_to_map)
  })
  
  # Map ####
  observe({
    data_to_map_collected=data_to_map()
    
    field_to_map=input$gender
    # leaflet(data_to_map)%>%
    #   addPolygons(weight=1,
    #               color = "#444444",
    #               smoothFactor = 1,
    #               fillOpacity = 1,
    #               fillColor = ~colorQuantile("Greens", get("total"))(get("total")),
    #               popup = ~get("total"))
    
    p<-leafletProxy("map",
                    data=data_to_map_collected)%>%
      addPolygons(weight=1,
                  color = "#444444",
                  smoothFactor = 1,
                  fillOpacity = 1,
                  fillColor = ~colorQuantile("Greens", get(field_to_map))(get(field_to_map)),
                  popup = ~get(field_to_map))
    return(p)
  })
})