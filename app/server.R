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
  
  # data_to_map_tm ####
  data_to_map_tm=reactive({
    con_selected=switch(input$direction, 
                        "emigration" = EM,
                        "immigration" = IM,
                        "net_immigration" = Net_IM,
                        "net_emigration" = Net_EM,
                        "total_migration"=EM_IM)
    
    data_to_map_tm=admin_poly
    data_to_map_tm@data=collect(admin%>%
                                  left_join(con_selected,
                                            by=c("ISO_NODE")))
    
    return(data_to_map_tm)
  })
  
  # data_to_map_origin_dest ####
  data_to_map_od=reactive({
    ISO_NODE=switch(input$direction,  
                    "emigration" = "ISO_NODEI",
                    "immigration" = "ISO_NODEJ",
                    "net_immigration" = c("ISO_NODEI","ISO_NODEJ"),
                    "net_emigration" = c("ISO_NODEJ","ISO_NODEI"),
                    "total_migration"=c("ISO_NODEI","ISO_NODEJ"))
    
    event <- input$map_shape_click
      if(is.null(event)){
        event<-data.frame("id"=55)
      }
    
    layerID_selected=event$id
    
    ISO_NODE_clicked=admin_poly$ISO_NODE[layerID_selected]
    
  
    
    admin_data=gender_mig%>%
      filter((!!sym(ISO_NODE))==ISO_NODE_clicked)%>%
      select(females,males,total,females_perc,males_perc,ISO_NODEI,ISO_NODEJ)
    
    
    data_to_map_od=admin_poly
    
    data_to_map_od@data=admin%>%
      left_join(admin_data,
                by=c("ISO_NODE"=ISO_NODE))%>%
      collect()
    
    return(data_to_map_od)
  })
  
  # labels ####
  labels=reactive({
    
    data_to_map_collected=switch(input$movement,
                                 "movements"=data_to_map_tm(),
                                 "origin_destination"=data_to_map_od())
    
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
    data_to_map_collected=switch(input$movement,
           "movements"=data_to_map_tm(),
           "origin_destination"=data_to_map_od())
    
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
  
  
  # # Map destination ###
  # observeEvent(input$map_shape_click,{
  #   event <- input$map_shape_click
  #   
  #   print(event)
  #   
  # })
  # 
  
  
  
  # # get event ####
  # observeEvent(input$map_shape_click,{
  #   event <- input$map_shape_click
  #   print(event)
  #   if(is.null(event)){
  #     event<-data.frame("lng"=-58.50,
  #                       "lat"=-34.60)
  #   }
  #   
  #   coord_select<-data.frame("lon"=event$lng,
  #                            "lat"=event$lat)
  #   
  #   coordinates(coord_select)<-c("lon","lat")
  #   proj4string(coord_select) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  #   
  #   
  #   selected_adm<-sp::over(coord_select,
  #                          admin_poly)
  #   
  #   selected_adm<-c(as.character(selected_adm$ISO_NODE))
  #   
  #   
  #   print(selected_adm)
  #   
  #   #return(selected_adm)
  # })
})