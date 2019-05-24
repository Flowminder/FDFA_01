###################
# server #
###################


# Server #### 
server <- shinyServer(function(input, output, session) {
  
  # global_figures_data ####
  global_figures_data=reactive({
    
    global_figures_data=IM%>%
    summarise(sum_total=sum(total,na.rm = T),
              sum_females=sum(females,na.rm = T),
              sum_males=sum(males,na.rm = T))%>%
    collect()%>%
    mutate(females_perc=sum_females/sum_total,
           males_perc=sum_males/sum_total)%>%
    select(sum_total,females_perc,males_perc)
  
  return(global_figures_data)
  })
  # global_figures ####
  output$global_figures<-renderUI({
    
    global_figures_collected=global_figures_data()
    world_share_mig_collected=collect(world_share_mig)
    
    HTML(paste0("<h4>",
                "<strong>","Number of Migrants: ",
                "</strong>",
                formatC(global_figures_collected$sum_total, big.mark = ","),
                "</h4>",
                "<h4>",
                "<strong>","Proportion of migrants in the population: ",
                "</strong>",
                paste0(round(world_share_mig_collected$mig_share*100,1),"%"),
                "</h4>",
                "<h4>",
                "<strong>","Proportion of females among migrants: ",
                "</strong>",
                paste0(round(global_figures_collected$females_perc*100,1),"%"),
                "</h4>",
                sep = '<br/>'
    ))
  })
  
  # global_female_pie ####
  output$global_female_pie=renderPlotly({
    
    global_figures_data_collected=global_figures_data()
    
    data_pie=global_figures_data_collected%>%
      select(females_perc,males_perc)%>%
      gather(key=key)%>%
      mutate(labels=factor(2:1,
                           labels = c("Males","Females")))%>%
      rename("values"="value")%>%
      group_by(labels)
    
    plot_ly(data_pie,
            labels=~labels,
            values=~values,
            hoverinfo = "text",
            marker = list(colors = c(RColorBrewer::brewer.pal(9,"Greens")[7],
                                     RColorBrewer::brewer.pal(9,"Greens")[2]),
                          line = list(color = '#FFFFFF', width = 1)))%>%
      add_pie(hole = 0.6)%>%
      layout(title=paste0("Proportion Females among Migrants"))
    
  })
  
  
  # top_10_data ####
  top_10_data=reactive({
    top_mig_count=IM%>%
      group_by(ISO)%>%
      summarise(sum_total=sum(total,na.rm = T))%>%
      arrange(desc(sum_total))%>%
      collect(n=10)%>%
      mutate(ISO_f=factor(1:10,
                          labels=ISO))%>%
      rename("values"="sum_total")
    
    top_mig_perc=Top_perc_mig_ISO%>%
      collect()%>%
      mutate(ISO_f=factor(1:10, labels = ISO))%>%
      rename("values"="migrant_per_pop")
    
    top_10_data=switch(paste(input$top_10_total_perc),
                       "FALSE"=top_mig_count,
                       "TRUE"=top_mig_perc)
    
    return(top_10_data)
    
  })
  # top_10_total_bar ####
  output$top_10_total_bar=renderPlotly({
    
    top_10_data_collected=top_10_data()
    
    p=plot_ly(top_10_data_collected,
              x=~ISO_f,
              y=~values,
              type="bar")
    
    p
  })
  # top_10_females ####
  top_10_females_data=reactive({
    top_females_count=IM%>%
      group_by(ISO)%>%
      summarise(sum_females=sum(females,na.rm = T))%>%
      arrange(desc(sum_females))%>%
      collect(n=10)%>%
      mutate(ISO_f=factor(1:10,
                          labels=ISO))%>%
      rename("values"="sum_females")
    
    top_females_perc=IM%>%
      group_by(ISO)%>%
      summarise(sum_females=sum(females,na.rm = T),
                sum_total=sum(total,na.rm = T))%>%
      collect()%>%
      mutate(females_perc=sum_females/sum_total)%>%
      arrange(desc(females_perc))%>%
      top_n(10,females_perc)%>%
      mutate(ISO_f=factor(1:10,
                          labels=ISO))%>%
      rename("values"="females_perc")
    
    top_10_females_data=switch(paste(input$top_10_females_perc),
                               "FALSE"=top_females_count,
                               "TRUE"=top_females_perc)
    
    return(top_10_females_data)
    
  })
  # top_10_females_bar ####
  output$top_10_females_bar=renderPlotly({
    
    top_10_females_data_collected=top_10_females_data()
    
    p=plot_ly(top_10_females_data_collected,
              x=~ISO_f,
              y=~values,
              type="bar")
    
    p
  })
  
  # ISO_NODE_clicked1 ####
  layerID_clicked1=reactive({
    event <- input$map1_shape_click
    
    print(event)
    
    if(is.null(event)){
      event<-data.frame("id"=55)
    }
    
    layerID_clicked1=event$id
    return(layerID_clicked1)
  })
  
  # country_summary_data ####
  country_summary_data=reactive({
    
    layerID_clicked1_collected=layerID_clicked1()
    
    
    ISO_clicked1=admin_poly$ISO[layerID_clicked1_collected]
    
    
    country_summary_data=IM%>%
      filter(ISO==ISO_clicked1)%>%
      group_by(ISO)%>%
      summarise(sum_females=sum(females,na.rm = T),
                sum_total=sum(total,na.rm = T))%>%
      left_join(POP_ISO_NODE%>%
                  select(ISO,POP)%>%
                  filter(ISO==ISO_clicked1)%>%
                  group_by(ISO)%>%
                  summarise(POP=sum(POP,na.rm=T)),
                by="ISO")%>%
      collect()%>%
      mutate(females_perc=sum_females/sum_total,
             mig_perc=sum_total/POP)%>%
      select(sum_total,females_perc,mig_perc,ISO)
    
    return(country_summary_data)
  })
  
  # country_summary_UI ####
  output$country_summary=renderUI({
    
    country_summary_data_collected=country_summary_data()
    
    HTML(paste0("<h2>",
                "<strong>","Country Selected: ",
                "</strong>",
                country_summary_data_collected$ISO,
                "</h2>",
                "<h4>",
                "<strong>","Number of Migrants: ",
                "</strong>",
                formatC(country_summary_data_collected$sum_total, big.mark = ","),
                "</h4>",
                "<h4>",
                "<strong>","Proportion of migrants in the population: ",
                "</strong>",
                paste0(round(country_summary_data_collected$mig_perc*100,1),"%"),
                "</h4>",
                "<h4>",
                sep = '<br/>'
    ))
    
  })
  # country_female_pie ####
  output$country_female_pie=renderPlotly({
    
    country_summary_data_collected=country_summary_data()
    
    data_pie=country_summary_data_collected%>%
      mutate(males_perc=1-females_perc)%>%
      select(females_perc,males_perc)%>%
      gather(key=key)%>%
      mutate(labels=factor(2:1,
                           labels = c("Males","Females")))%>%
      rename("values"="value")%>%
      group_by(labels)
    
    plot_ly(data_pie,
            labels=~labels,
            values=~values,
            hoverinfo = "text",
            marker = list(colors = c(RColorBrewer::brewer.pal(9,"Greens")[7],
                                     RColorBrewer::brewer.pal(9,"Greens")[2]),
                          line = list(color = '#FFFFFF', width = 1)))%>%
      add_pie(hole = 0.6)%>%
      layout(title=paste0("Proportion Females among Migrants"))
    
  })
  
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
      addTiles()%>%
      setView(lng=0,lat=0,zoom = 2)
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
  
  # ISO_NODE_clicked2 ####
  ISO_NODE_clicked2=reactive({
    event <- input$map2_shape_click
    
    print(event)
    
    if(is.null(event)){
      event<-data.frame("id"=55)
    }
    
    layerID_selected=event$id
    
    ISO_NODE_clicked2=admin_poly$ISO_NODE[layerID_selected]
    return(ISO_NODE_clicked2)
  })
  
  # data_map2 ####
  data_map2=reactive({
    ISO_NODE=switch(input$direction2,  
                    "emigration" = "ISO_NODEI",
                    "immigration" = "ISO_NODEJ",
                    "net_immigration" = c("ISO_NODEI","ISO_NODEJ"),
                    "net_emigration" = c("ISO_NODEJ","ISO_NODEI"),
                    "total_migration"=c("ISO_NODEI","ISO_NODEJ"))
    
    ISO_NODE_clicked=ISO_NODE_clicked2()
    
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
      addTiles()%>%
      setView(lng=0,lat=0,zoom = 2)
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