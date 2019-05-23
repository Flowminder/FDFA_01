###################
# server #
###################


# Server #### 
server <- shinyServer(function(input, output, session) {
  
  # data_map1 ####
  data_map1=reactive({
    con_selected=switch(input$direction1, 
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
  
  # labels_map1 ####
  labels_map1=reactive({
    
    data_to_map_collected=data_map1()
    
    field_to_map=input$gender1
    
    labels=sprintf( 
      paste("<strong>%s</strong><br/>%s", field_to_map),
      
      unlist(data_to_map_collected@data[,"CONT"]),
      
      formatC(unlist(data_to_map_collected@data[,field_to_map]),
              format="f",
              digits=0,
              big.mark="'")
    ) %>% lapply(htmltools::HTML)
    
    return(labels)
    
  })
  
  # Map1: base map ####
  output$map1<-renderLeaflet({
    leaflet() %>%
      addTiles()
  })

  # Map1 ####
  observe({
    data_to_map_collected=data_map1()
    
    field_to_map=input$gender1
    
    labels=labels_map1()
    
    p<-leafletProxy("map1")%>%
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
  
  
  # data_map2 ####
  data_map2=reactive({
    ISO_NODE=switch(input$direction2,  
                    "emigration" = "ISO_NODEI",
                    "immigration" = "ISO_NODEJ",
                    "net_immigration" = c("ISO_NODEI","ISO_NODEJ"),
                    "net_emigration" = c("ISO_NODEJ","ISO_NODEI"),
                    "total_migration"=c("ISO_NODEI","ISO_NODEJ"))
    
    event <- input$map2_shape_click
    
    print(event)
  
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
  
  # labels_map2 ####
  labels_map2=reactive({
    
    data_to_map_collected=data_map2()
    
    field_to_map=input$gender2
    
    labels=sprintf( 
      paste("<strong>%s</strong><br/>%s", field_to_map),
      
      unlist(data_to_map_collected@data[,"CONT"]),
      
      formatC(unlist(data_to_map_collected@data[,field_to_map]),
              format="f",
              digits=0,
              big.mark="'")
    ) %>% lapply(htmltools::HTML)
    
    return(labels)
    
  })
  
  # Map2: base map ####
  output$map2<-renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  # Map2 ####
  observe({
    data_to_map_collected=data_map2()
    
    field_to_map=input$gender2
    
    labels=labels_map2()
    
    p<-leafletProxy("map2")%>%
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
  
  # movement_type ####
  
  # gender_pie ####
  
  # circle_od ####
  
  # od_bar ####
  
  # od_bar_title ####
  
  
})