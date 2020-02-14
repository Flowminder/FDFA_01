function_poly=function(mig_db,topo_correct_st,simple_countries_st,simple_countries_df,deck_proxy,country_clicked,admin_clicked,focus,type,direction,sex,aggregation,n_slice,top_n_per_country){
  
  aggregation_f=aggregation
  if(focus=="admin"){aggregation_f="admin"}
  
  # filter and collect data 1 ####
  data_col_1_collected=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,focus,type,direction,sex,aggregation_f)
  
  # filter and collect data 2 ####
  if(focus%in%c("country","admin")){
    if(aggregation_f=="admin"){
      data_col_2_collected=col_slicer_2_admin(mig_db,country_clicked,admin_clicked,focus,type,direction,sex,aggregation_f,n_slice)
    }
    if(aggregation_f=="country"&type=="int"){
      data_col_2_collected=col_slicer_2_country(mig_db,country_clicked,admin_clicked,focus,type,direction,sex,aggregation_f,n_slice)
    }
  }
  
  # polys
  if(focus=="admin"){
    topo_in_selected=unique(c(data_col_1_collected$JOIN_ID))     # selected admin in
    topo_in_selected_sub=subset(topo_correct_st,
                                as.character(topo_correct_st$JOIN_ID)%in%topo_in_selected)
    
    topo_in_all=unique(c(data_col_1_collected$ISO))               # non selected admin from in country
    topo_in_all_sub=subset(topo_correct_st,
                           as.character(topo_correct_st$ISO)%in%topo_in_all)
    
    topo_out=unique(c(data_col_2_collected$ID_group_by))        # selected admin from out country
    topo_out_sub=subset(topo_correct_st,
                        as.character(topo_correct_st$JOIN_ID)%in%topo_out)
  }
  
  if(focus=="country"){
    topo_in_selected=unique(c(data_col_1_collected$ISO))
    topo_in_selected_sub=subset(topo_correct_st,
                                as.character(topo_correct_st$ISO)%in%topo_in_selected)
    
    topo_out=unique(c(data_col_2_collected$ID_group_by))
    topo_out_sub=subset(topo_correct_st,
                        as.character(topo_correct_st$JOIN_ID)%in%topo_out)
  }
  
  if(aggregation_f=="country"&focus%in%c("admin","country")){
    topo_in=unique(c(data_col_1_collected$ISO))
    topo_in_sub=subset(simple_countries_st,
                       as.character(simple_countries_st$id)%in%topo_in)
    
    topo_out=unique(c(data_col_2_collected$ISO_group_by))
    topo_out_sub=subset(simple_countries_st,
                        as.character(simple_countries_st$id)%in%topo_out)
  }  
  # color
  color_in=switch(type,
                  "nat"=c(0, 9, 255),
                  "int"=c(157,140,112))
  color_out=switch(type,
                   "nat"=c(110, 113, 202),
                   "int"=c(186, 222, 189))
  # deck
  deck_object=deck_proxy%>%
    add_polygon_layer(data=simple_countries_st, # poly base
                      getPolygon = ~geometry,
                      opacity = 0,
                      autoHighlight=TRUE,
                      highlightColor=c(255,128,0),
                      stroked = FALSE,
                      filled = TRUE,
                      extruded = TRUE,
                      wireframe = TRUE,
                      fp64 = TRUE,
                      pickable=TRUE,
                      id="poly_base")
    
  
  if(focus!="global"){
    
    deck_object=deck_object%>%
      add_polygon_layer(data=topo_in_selected_sub, # admin areas of selected IN
                        getPolygon = ~geometry,
                        opacity = 0.2,
                        getFillColor = color_in,
                        autoHighlight=TRUE,
                        stroked = FALSE,
                        filled = TRUE,
                        extruded = TRUE,
                        wireframe = TRUE,
                        fp64 = TRUE,
                        pickable=TRUE,
                        legend = TRUE,
                        id="selected_admin_in")
    
    if(focus%in%c("admin","country")&type=="int" |
       focus=="admin"&type=="nat"){
      deck_object=deck_object%>%
        add_polygon_layer(data=topo_out_sub, # admin areas of selected OUT
                          getPolygon = ~geometry,
                          opacity = 0.2,
                          getFillColor = color_out,
                          autoHighlight=TRUE,
                          stroked = FALSE,
                          filled = TRUE,
                          extruded = TRUE,
                          wireframe = TRUE,
                          fp64 = TRUE,
                          pickable=TRUE,
                          id="selected_admin_out")
    }
    
    if(focus=="admin"){
      deck_object=deck_object%>%
        add_polygon_layer(data=topo_in_all_sub, # admin areas of selected country not selected
                          getPolygon = ~geometry,
                          opacity = 0,
                          # getFillColor = color_out,
                          autoHighlight=TRUE,
                          stroked = FALSE,
                          filled = TRUE,
                          extruded = TRUE,
                          wireframe = TRUE,
                          fp64 = TRUE,
                          pickable=TRUE,
                          id="non_selected_admin_in")
    }

  }
  return(deck_object)

}