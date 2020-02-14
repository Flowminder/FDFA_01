mapdeck_col=function(mig_db,simple_countries_df,country_clicked,admin_clicked,focus,type,direction,sex,aggregation,n_slice,scale){
  
  # null parameters
  if(is.null(country_clicked)){country_clicked="AFG"}
  if(is.null(admin_clicked)){admin_clicked="13"}
  if(is.null(focus)){focus="global"}
  if(is.null(type)){type="int"}
  if(is.null(direction)){direction="X"}
  if(is.null(sex)){sex="F"}
  if(is.null(aggregation)){aggregation="admin"}
  if(is.null(n_slice)){n_slice="3"}
  # if(is.null(top_n_per_country)){top_n_per_country=FALSE}
  
  # country_clicked="AFG"
  # admin_clicked="13"
  opacity=1
  coverage=1
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
    name_label=data_col_1_collected$COUNTRY_NAME
  }else{
    name_label=paste(data_col_1_collected$ADMIN_NAME, paste0("(",data_col_1_collected$COUNTRY_NAME,")"))
  }
  
  label_1_html=paste(gender_label,type_label,direction_label,
                     "<br/>",name_label,
                     "<br/>","Count: ",
                     sapply(data_col_1_collected$move,format_n))

  data_col_1_collected$label_1_html=label_1_html
  
  
  # label 2
  if(focus%in%c("country","admin")){
    direction_label="migration to"
    if(direction=="M"){direction_label="migration from"}
    
    if(aggregation_f=="country"){
      name_label=data_col_2_collected$COUNTRY_NAME
    }else{
      name_label=paste(data_col_2_collected$ADMIN_NAME, paste0("(",data_col_2_collected$COUNTRY_NAME,")"))
    }
    
    label_2=paste("data => `",gender_label,type_label,direction_label,"<br/>",name_label, "<br/>Count: ${Math.round(data.move)}`")
    
    label_2_html=paste(gender_label,type_label,direction_label,
                       "<br/>",name_label,
                       "<br/>","Count: ",
                       sapply(data_col_2_collected$move,format_n))
    data_col_2_collected$label_2_html=label_2_html
  }
  
  # colours 
  col_bar_1=switch(type,
                   "int"=switch(direction,
                                "X"="#9d8c70",
                                "M"="#eba32d"),
                   "nat"=switch(direction,
                                "X"="#0009ff",
                                "M"="#7d81e8")
  )
  
  col_bar_2=switch(type,
                   "int"=switch(direction,
                                "X"="#eba32d",
                                "M"="#9d8c70"),
                   "nat"=switch(direction,
                                "X"="#7d81e8",
                                "M"="#0009ff")
  )
  
  # deck
  # deck_object=deck_proxy=mapdeck(token = api_token,
  #                     zoom=2,
  #                     location = c(14.39697, 29.22912),
  #                     pitch = 35)
  deck_object=mapdeck_update(map_id = "deck") 
  deck_object=deck_object%>%
    add_column(data =data_col_1_collected 
               , lon="lon"
               , lat="lat"
               , elevation="move"
               , disk_resolution = 20
               , radius = 20000
               , coverage=coverage
               , auto_highlight=T
               , fill_opacity=opacity
               , tooltip = "label_1_html"
               , elevation_scale =scale
               , fill_colour=col_bar_1
               , focus_layer=FALSE
               , update_view = FALSE
               , layer_id="level_1"
               , transitions=list(lon = 700, lat = 700, elevation = 700))
  
  if(focus%in%c("admin","country")&type=="int" |
     focus=="admin"&type=="nat"){
    
    deck_object=deck_object%>%
      add_column(data =data_col_2_collected 
                 , lon="lon"
                 , lat="lat"
                 , elevation="move"
                 , disk_resolution = 4
                 , radius = 20000
                 , coverage=coverage
                 , auto_highlight=T
                 , fill_opacity=0.5
                 , tooltip = "label_2_html"
                 , elevation_scale =scale
                 , fill_colour=col_bar_2
                 , update_view = FALSE
                 , focus_layer=FALSE
                 , layer_id="level_2"
                 , transitions=list(lon = 700, lat = 700, elevation = 700))
    
  }else{
    deck_object=deck_object%>%
      clear_column(layer_id = "level_2")
  }

  return(deck_object)
}
