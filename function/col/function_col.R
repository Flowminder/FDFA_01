function_col=function(mig_db,simple_countries_df,deck_proxy,country_clicked,admin_clicked,focus,type,direction,sex,aggregation,n_slice,scale){
  # country_clicked,admin_clicked,focus,type,direction,sex,aggregation,n_slice,top_n_per_country
  # db_list_tables(mig_db)
  
  # print(country_clicked)
  # print(admin_clicked)
  coverage=1
  opacity=1
  # country_clicked="AFG"
  # admin_clicked="13"
  aggregation_f=aggregation
  if(focus=="admin"){aggregation_f="admin"}
  if(is.null(aggregation)){aggregation_f="admin"}
  
  JOIN_ID_f=paste(c(country_clicked,admin_clicked),collapse="_")

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

  # label 1 ####
  gender_label=switch(sex,
                      "M"="Male",
                      "F"="Female",
                      "all"="Female + male")
  direction_label="emigration from"
  if(direction=="M"){direction_label="immigration to"}
  
  type_label="international"
  if(type=="nat"){type_label="internal"}
  
  
  if(aggregation_f=="country"){
    name_label="${data.COUNTRY_NAME}"
  }else{
    name_label="${data.ADMIN_NAME} (${data.COUNTRY_NAME})"
  }
  
  label_1=paste("data => `",gender_label,type_label,direction_label,"<br/>",name_label, "<br/>Count: ${Math.round(data.move)}`")
  
  # label 2
  direction_label="migration to"
  if(direction=="M"){direction_label="migration from"}
  if(aggregation_f=="country"){
    name_label="${data.COUNTRY_NAME}"
  }else{
    name_label="${data.ADMIN_NAME} (${data.COUNTRY_NAME})"
  }
  
  label_2=paste("data => `",gender_label,type_label,direction_label,"<br/>",name_label, "<br/>Count: ${Math.round(data.move)}`")
  
  # colour 
  col_bar=switch(type,
                 "int"=switch(direction,
                              "X"=c(157,140,112),
                              "M"=c(235, 163, 45)),
                 "nat"=switch(direction,
                              "X"=c(0, 9, 255),
                              "M"=c(125, 129, 232))
  )
  # offset
  offset=switch(type,
                "int"=switch(direction,
                             "X"=c(0,0),
                             "M"=c(0,2)),
                "nat"=switch(direction,
                             "X"=c(2,0),
                             "M"=c(2,2))
  )
  
  # deck 
  deck_object=deck_proxy%>%
    add_column_layer( # column level 1 : IN
      data = data_col_1_collected,
      autoHighlight=TRUE,
      diskResolution = 10,
      radius = 20000,
      coverage=coverage,
      opacity=opacity,
      offset=offset,
      elevationScale=scale,
      getPosition = get_position("lat", "lon"),
      getElevation = get_property("move"),
      getFillColor = col_bar,
      extruded = TRUE,
      getTooltip=JS(label_1), 
      id="level_1",
      pickable=TRUE)
  
  if(focus%in%c("admin","country")&type=="int" |
     focus=="admin"&type=="nat"){
    deck_object=deck_object%>%
      add_column_layer( # column level 2: out
        data = data_col_2_collected,
        autoHighlight=TRUE,
        diskResolution = 4,
        radius = 20000,
        coverage=coverage,
        opacity=opacity,
        offset=offset,
        elevationScale=scale,
        getPosition = get_position("lat", "lon"),
        getElevation = get_property("move"),
        getFillColor = col_bar,
        extruded = TRUE,
        legend = TRUE,
        getTooltip=JS(label_2), 
        id="level_2",
        pickable=TRUE)
  }
  return(deck_object)
  
}
