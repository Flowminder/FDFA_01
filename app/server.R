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
      left_join(ISO,
                by="ISO")%>%
      arrange(desc(sum_total))%>%
      collect(n=10)%>%
      mutate(country_name_f=factor(1:10,
                                   labels=country_name))%>%
      rename("values"="sum_total")
    
    top_mig_perc=Top_perc_mig_ISO%>%
      left_join(ISO,
                by="ISO")%>%
      collect()%>%
      mutate(country_name_f=factor(1:10, labels = country_name),
             migrant_per_pop=migrant_per_pop*100)%>%
      rename("values"="migrant_per_pop")
    
    top_10_data=switch(paste(input$top_10_total_perc),
                       "FALSE"=top_mig_count,
                       "TRUE"=top_mig_perc)
    
    return(top_10_data)
    
  })
  # top_10_total_bar ####
  output$top_10_total_bar=renderPlotly({
    
    top_10_data_collected=top_10_data()
    
    title_s=switch(paste0(input$top_10_total_perc),
                   "FALSE"="number of migrants",
                   "TRUE"="proportion of migrants in the population")
    yaxis_title_s=switch(paste0(input$top_10_total_perc),
                         "FALSE"="count",
                         "TRUE"="%")
    
    p=plot_ly(top_10_data_collected,
              x=~country_name_f,
              y=~values,
              type="bar")%>%
      layout(title=paste0("Top 10 countries by\n",title_s),
             yaxis=list(title=yaxis_title_s),
             xaxis=list(title=""),
             margin=m)
    
    p
  })
  # top_10_females ####
  top_10_females_data=reactive({
    top_females_count=IM%>%
      group_by(ISO)%>%
      summarise(sum_females=sum(females,na.rm = T))%>%
      left_join(ISO,
                by="ISO")%>%
      arrange(desc(sum_females))%>%
      collect(n=10)%>%
      mutate(country_name_f=factor(1:10,
                                   labels=country_name))%>%
      rename("values"="sum_females")
    
    top_females_perc=IM%>%
      group_by(ISO)%>%
      summarise(sum_females=sum(females,na.rm = T),
                sum_total=sum(total,na.rm = T))%>%
      left_join(ISO,
                by="ISO")%>%
      collect()%>%
      mutate(females_perc=100*sum_females/sum_total)%>%
      arrange(desc(females_perc))%>%
      top_n(10,females_perc)%>%
      mutate(country_name_f=factor(1:10,
                                   labels=country_name))%>%
      rename("values"="females_perc")
    
    top_10_females_data=switch(paste(input$top_10_females_perc),
                               "FALSE"=top_females_count,
                               "TRUE"=top_females_perc)
    
    return(top_10_females_data)
    
  })
  # top_10_females_bar ####
  output$top_10_females_bar=renderPlotly({
    
    top_10_females_data_collected=top_10_females_data()
    
    title_s=switch(paste0(input$top_10_females_perc),
                   "FALSE"="number of female migrants",
                   "TRUE"="proportion of female among migrants")
    yaxis_title_s=switch(paste0(input$top_10_total_perc),
                         "FALSE"="count",
                         "TRUE"="%")
    
    p=plot_ly(top_10_females_data_collected,
              x=~country_name_f,
              y=~values,
              type="bar")%>%
      layout(title=paste0("Top 10 countries by\n",title_s),
             yaxis=list(title=yaxis_title_s),
             xaxis=list(title=""),
             margin=m)
    
    p
  })
  
  # layerID_clicked ####
  layerID_clicked=reactive({
    
    event=switch(input$tabs,
                 "global"=input$map1_shape_click,
                 "od"=input$map2_shape_click)
    
    if(is.null(event)){
      event<-data.frame("id"=55)
    }
    
    layerID_clicked=event$id
    return(layerID_clicked)
  })
  
  # country_summary_data ####
  country_summary_data=reactive({
    
    layerID_clicked_collected=layerID_clicked()
    
    
    ISO_clicked=admin_poly_modelled$ISO[layerID_clicked_collected]
    
    country_summary_data=IM%>%
      filter(ISO==ISO_clicked)%>%
      group_by(ISO)%>%
      summarise(sum_females=sum(females,na.rm = T),
                sum_total=sum(total,na.rm = T))%>%
      left_join(POP_ISO_NODE%>%
                  select(ISO,POP)%>%
                  filter(ISO==ISO_clicked)%>%
                  group_by(ISO)%>%
                  summarise(POP=sum(POP,na.rm=T)),
                by="ISO")%>%
      left_join(ISO,
                by="ISO")%>%
      collect()%>%
      mutate(females_perc=sum_females/sum_total,
             mig_perc=sum_total/POP)%>%
      select(sum_total,females_perc,mig_perc,ISO,country_name)
    
    return(country_summary_data)
  })
  
  # country_summary_UI ####
  output$country_summary=renderUI({
    
    country_summary_data_collected=country_summary_data()
    
    HTML(paste0("<h2>",
                "<strong>","Country Selected: ",
                "</strong>",
                country_summary_data_collected$country_name,
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
  
  # top_10_country_data ####
  top_10_country_data=reactive({
    
    layerID_clicked_collected=layerID_clicked()
    
    ISO_clicked=admin_poly_modelled$ISO[layerID_clicked_collected]
    
    
    gender_selected=input$gender3
    
    n_10_top=IM%>% # in order to control for countries with less than 10 admin units
      filter(ISO==ISO_clicked)%>%
      distinct(ISO_NODE)%>%
      summarise(n_to_show=n())%>%
      mutate(n_to_show=ifelse(n_to_show>10,10,n_to_show))%>%
      collect()
    
    top_mig_count=IM%>%
      filter(ISO==ISO_clicked)%>%
      arrange(desc((!!sym(gender_selected))))%>%
      collect(n=n_10_top$n_to_show)%>%
      mutate(ISO_NODE_f=factor(1:n_10_top$n_to_show,
                               labels=ISO_NODE))%>%
      rename("values"=gender_selected)
    
    top_mig_perc=IM%>%
      filter(ISO==ISO_clicked)%>%
      left_join(nick_mig%>%
                  filter(ISOJ==ISO_clicked)%>%
                  select(ISO_NODEJ, POPJ)%>%
                  group_by(ISO_NODEJ)%>%
                  summarise(POP=mean(POPJ,na.rm=T))%>%
                  rename("ISO_NODE"="ISO_NODEJ"),
                by="ISO_NODE")%>%
      collect()%>%
      mutate(prop_mig=100*get(gender_selected)/POP)%>%
      arrange(desc(prop_mig))%>%
      top_n(n_10_top$n_to_show,prop_mig)%>%
      mutate(ISO_NODE_f=factor(1:n_10_top$n_to_show,
                               labels=ISO_NODE))%>%
      rename("values"="prop_mig")
    
    top_10_data=switch(input$top_10_country_dropdown,
                       "number"=top_mig_count,
                       "perc"=top_mig_perc)
    
    return(top_10_data)
    
  })
  
  # top_10_country_bar ####
  output$top_10_country_bar=renderPlotly({
    
    top_10_country_data_collected=top_10_country_data()
    
    gender_selected=input$gender3
    gender_selected_s=switch(gender_selected,
                             "total"="\n(total: females + males)",
                             "females"="\n(females)",
                             "males"="\n(males)")
    
    
    title_s=switch(input$top_10_country_dropdown,
                   "number"="number of immigrants",
                   "perc"="proportion of immigrants in population at destination")
    
    yaxis_title_s=switch(input$top_10_country_dropdown,
                         "number"="count",
                         "perc"="%")
    
    p=plot_ly(top_10_country_data_collected,
              x=~ISO_NODE_f,
              y=~values,
              type="bar")%>%
      layout(title=paste0("Top regions according to\n",
                                     title_s,
                          gender_selected_s),
             yaxis=list(title=yaxis_title_s),
             xaxis=list(title=""),
             margin=m)
    
    
    return(p)
  })
  
  
  # top_10_country_EM_data ####
  top_10_country_EM_data=reactive({
    
    layerID_clicked_collected=layerID_clicked()
    
    ISO_clicked=admin_poly_modelled$ISO[layerID_clicked_collected]
    
    gender_selected=input$gender4
    
    n_10_top=EM%>%
      filter(ISO==ISO_clicked)%>%
      distinct(ISO_NODE)%>%
      summarise(n_to_show=n())%>%
      mutate(n_to_show=ifelse(n_to_show>10,10,n_to_show))%>%
      collect()
    
    top_mig_count=EM%>%
      filter(ISO==ISO_clicked)%>%
      arrange(desc((!!sym(gender_selected))))%>%
      collect(n=n_10_top$n_to_show)%>%
      mutate(ISO_NODE_f=factor(1:n_10_top$n_to_show,
                               labels=ISO_NODE))%>%
      rename("values"=gender_selected)
    
    top_mig_perc=EM%>%
      filter(ISO==ISO_clicked)%>%
      left_join(nick_mig%>%
                  filter(ISOI==ISO_clicked)%>%
                  select(ISO_NODEI, POPI)%>%
                  group_by(ISO_NODEI)%>%
                  summarise(POP=mean(POPI,na.rm=T))%>%
                  rename("ISO_NODE"="ISO_NODEI"),
                by="ISO_NODE")%>%
      collect()%>%
      mutate(prop_mig=get(gender_selected)/POP)%>%
      arrange(desc(prop_mig))%>%
      top_n(n_10_top$n_to_show,prop_mig)%>%
      mutate(ISO_NODE_f=factor(1:n_10_top$n_to_show,
                               labels=ISO_NODE))%>%
      rename("values"="prop_mig")
    
    top_10_data=switch(input$top_10_country_EM_dropdown,
                       "number"=top_mig_count,
                       "perc"=top_mig_perc)
    
    return(top_10_data)
    
  })
  
  # top_10_country_EM_bar ####
  output$top_10_country_EM_bar=renderPlotly({
    
    top_10_country_EM_data_collected=top_10_country_EM_data()
    
    gender_selected=input$gender4
    gender_selected_s=switch(gender_selected,
                             "total"="\n(total: females + males)",
                             "females"="\n(females)",
                             "males"="\n(males)")
    
    
    title_s=switch(input$top_10_country_dropdown,
                   "number"="number of emigrants",
                   "perc"="proportion of emigrants in population from origin")
    
    yaxis_title_s=switch(input$top_10_country_dropdown,
                         "number"="count",
                         "perc"="%")
    
    p=plot_ly(top_10_country_EM_data_collected,
              x=~ISO_NODE_f,
              y=~values,
              type="bar")%>%
      layout(title=paste0("Top regions according to\n",
                          title_s,
                          gender_selected_s),
             yaxis=list(title=yaxis_title_s),
             xaxis=list(title=""),
             margin=m)
    return(p)
  })
  
  
  # data_map1 ####
  data_map1=reactive({
    con_selected=switch(input$direction1, 
                        "emigration" = EM,
                        "immigration" = IM,
                        "net_immigration" = Net_IM,
                        "net_emigration" = Net_EM,
                        "total_migration"=EM_IM)
    
    data_to_map_tm=admin_poly_modelled
    data_to_map_tm@data=admin_modelled%>%
      left_join(con_selected%>%
                  select(ISO_NODE,total,females,males,females_perc,males_perc),
                by=c("ISO_NODE"))%>%
      left_join(ISO,
                by="ISO")%>%
      left_join(admin1_names%>%
                  select(ISO_NODE,NODE_NAME),
                by="ISO_NODE")%>%
      mutate(females_perc=females_perc*100,
             males_perc=males_perc*100)%>%
      collect()
    
    return(data_to_map_tm)
  })
  
  # labels_map1 ####
  labels_map1=reactive({
    
    data_to_map_collected=data_map1()
    
    field_to_map=input$gender1
    
    labels=sprintf( 
      paste("<strong>%s</strong><br/><i>%s</i><br/>%s", field_to_map),
      
      unlist(data_to_map_collected@data[,"country_name"]),
      
      unlist(data_to_map_collected@data[,"NODE_NAME"]),
      
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
      clearShapes() %>%
      addPolygons(data=data_to_map_collected,
                  weight=1,
                  color = "#444444",
                  fillOpacity = 1,
                  fillColor = ~colorQuantile("Greens", get(field_to_map), na.color = "transparent")(get(field_to_map)),
                  layerId = 1:dim(data_to_map_collected)[1],
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))%>%
      addPolylines(data=country_poly_modelled,
                   weight=2,
                   opacity = 1,
                   color = "black",
                   layerId = 1:dim(data_to_map_collected)[1])
    
    return(p)
  })
  
  # ISO_NODE_clicked2 ####
  ISO_NODE_clicked2=reactive({
    
    event <- input$map2_shape_click
    
    if(is.null(event)){
      event<-data.frame("id"=55)
    }
    
    layerID_selected=event$id
    
    ISO_NODE_clicked2=admin_poly_modelled$ISO_NODE[layerID_selected]
    
    
    
    return(ISO_NODE_clicked2)
  })
  
  # data_map2 ####
  data_map2=reactive({
    ISO_NODE=switch(input$direction2,
                    "emigration" = "ISO_NODEI",
                    "immigration" = "ISO_NODEJ")

    ISO_NODE_clicked=ISO_NODE_clicked2()
    
    ISO_selected=substring(ISO_NODE_clicked,1,3)
    
    admin_data=gender_mig%>%
      filter((!!sym(ISO_NODE))==ISO_NODE_clicked)%>%
      select(females,males,total,females_perc,males_perc,ISO_NODEI,ISO_NODEJ)%>%
      mutate(females_perc=females_perc*100,
             males_perc=males_perc*100)
    
    data_to_map_od=admin_poly_modelled
    data_to_map_od=data_to_map_od[data_to_map_od$ISO==ISO_selected,]
    
    data_to_map_od@data=data_to_map_od@data%>%
      mutate(ISO=as.character(ISO),
             ISO_NODE=as.character(ISO_NODE))%>%
      left_join(ISO%>%
                  collect(),
                by="ISO")%>%
      left_join(admin_data%>%
                  collect(),
                by=c("ISO_NODE"=ISO_NODE))
    
    return(data_to_map_od)
  })
  
  
  # data_map2 ####
  data_selected_admin=reactive({
    ISO_NODE=switch(input$direction2,
                    "emigration" = "ISO_NODEI",
                    "immigration" = "ISO_NODEJ")
    
    ISO_NODE_clicked=ISO_NODE_clicked2()
    
    admin_data=gender_mig%>%
      filter((!!sym(ISO_NODE))==ISO_NODE_clicked)%>%
      select(females,males,total,females_perc,males_perc,ISO_NODEI,ISO_NODEJ)%>%
      mutate(females_perc=females_perc*100,
             males_perc=males_perc*100)
    
    data_to_map_od=admin_poly_modelled
    data_to_map_od=data_to_map_od[data_to_map_od$ISO_NODE==ISO_NODE_clicked,]
    
    data_to_map_od@data=data_to_map_od@data%>%
      mutate(ISO=as.character(ISO),
             ISO_NODE=as.character(ISO_NODE))%>%
      left_join(ISO%>%
                  collect(),
                by="ISO")%>%
      left_join(admin_data%>%
                  collect(),
                by=c("ISO_NODE"=ISO_NODE))
    
    return(data_to_map_od)
  })
  
  
  # labels_map2 ####
  labels_map2=reactive({
    
    data_to_map_collected=data_map2()

    field_to_map=input$gender2

    ISO_NODE_lab=switch(input$direction2,
                        "emigration"=unlist(data_to_map_collected@data[,"ISO_NODEJ"]),
                        "immigration"=unlist(data_to_map_collected@data[,"ISO_NODEI"]))

    labels=sprintf(
      paste("<strong>%s</strong><br/><i>%s</i><br/>%s", field_to_map),

      unlist(data_to_map_collected@data[,"CONT"]),

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
    data_to_map_collected1=data_map1()
    data_selected_admin=data_selected_admin()
    field_to_map=input$gender2
    
    labels=labels_map2()

    p<-leafletProxy("map2")%>%
      clearShapes() %>%
      addPolygons(data=data_to_map_collected,
                  weight=1,
                  color = "#444444",
                  fillOpacity = 1,
                  fillColor = ~colorQuantile("Greens", get(field_to_map), na.color = "transparent")(get(field_to_map)),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))%>%
      addPolygons(data=admin_poly_modelled,
                  weight=1,
                  color = "black",
                  fillColor = "transparent",
                  label =~ISO_NODE,
                  layerId = 1:dim(data_to_map_collected1)[1])%>%
      addPolygons(data=data_selected_admin,
                  weight=1,
                  color = "#444444",
                  fillOpacity = 1,
                  fillColor = "grey")
      

    return(p)
  })
  
  # chorddiagOutput_1 ####
  output$chorddiagOutput_1=renderChorddiag({
    
    layerID_clicked_collected=layerID_clicked()
    
    
    ISO_clicked=admin_poly_modelled$ISO[layerID_clicked_collected]
    
    gender_selected=input$gender5
    
    arg_db=gender_mig%>%
      filter(ISOI==ISO_clicked)%>%
      select(ISO_NODEI,ISO_NODEJ,gender_selected)%>%
      collect()
    
    mig_data_filter<-as.matrix(as_adjacency_matrix(as_tbl_graph(arg_db),
                                                   attr = gender_selected))
    chord<-chorddiag(data = mig_data_filter,
                     groupnamePadding = 30,
                     groupPadding = 3,
                     # groupColors = c("#ffffe5","#fff7bc","#fee391","#fec44f","#fe9929","#ec7014","#cc4c02","#8c2d04"),
                     groupnameFontsize = 13 ,
                     showTicks = FALSE,
                     margin=150,
                     tooltipGroupConnector = "    &#x25B6;    ",
                     chordedgeColor = "#B3B6B7"
    )
    chord
  })
  # od_bar ####
  
  # od_bar_title ####
  
  
})