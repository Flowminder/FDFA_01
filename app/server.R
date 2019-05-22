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
  
  # Map labels ####
  labels=reactive({ 
    
    data_to_map_collected=data_to_map()
    
    field_to_map=input$gender
    
    labels=sprintf( # label
      paste("<strong>%s</strong><br/>%s", field_to_map),
      
      unlist(data_to_map_collected@data[,"CONT"]),
      
      formatC(unlist(data_to_map_collected@data[,field_to_map]),
              format="f", 
              digits=0, 
              big.mark="'")
    ) %>% lapply(htmltools::HTML)
    
    return(labels)
    
  })
  
  # Map ####
  observe({
    data_to_map_collected=data_to_map()
    
    field_to_map=input$gender
    
    labels=labels()
    
    p<-leafletProxy("map")%>%
      addPolygons(data=data_to_map_collected,
                  weight=1,
                  color = "#444444",
                  fillOpacity = 1,
                  fillColor = ~colorQuantile("Greens", get(field_to_map))(get(field_to_map)),
                  layerId = 1:dim(data_to_map_collected)[1],
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
    
    return(p)
  })
  
  # Admin_clic ####
  # observeEvent(input$map_click,{
  #   print("hh")
  #   event <- input$map_click
  #   print(event)
  # })
  
  observeEvent(input$map_shape_click,{
    event <- input$map_shape_click
    print(event)
    if(is.null(event)){
      event<-data.frame("lng"=-58.50,
                        "lat"=-34.60)
    }
    
    coord_select<-data.frame("lon"=event$lng,
                             "lat"=event$lat)
    
    coordinates(coord_select)<-c("lon","lat")
    proj4string(coord_select) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    
    selected_adm<-sp::over(coord_select,
                           admin_poly)
    
    selected_adm<-c(as.character(selected_adm$ISO_NODE))
    
    
    print(selected_adm)
    
    #return(selected_adm)
  })
})