# function_arc ####
mapdeck_arc=function(mig_db,country_clicked,admin_clicked,focus,type,direction,sex,aggregation,n_slice,top_n_per_country){
  
  # null parameters
  if(is.null(country_clicked)){country_clicked="AFG"}
  if(is.null(admin_clicked)){admin_clicked="13"}
  if(is.null(focus)){focus="global"}
  if(is.null(type)){type="int"}
  if(is.null(direction)){direction="X"}
  if(is.null(sex)){sex="F"}
  if(is.null(aggregation)){aggregation="admin"}
  if(is.null(n_slice)){n_slice="3"}
  if(is.null(top_n_per_country)){top_n_per_country=FALSE}

  
  aggregation_f=aggregation
  if(focus=="admin"){aggregation_f="admin"}  
  
  # filter and collect the data
  data_arc_collected=arc_slicer(mig_db,country_clicked,admin_clicked,focus,type,direction,sex,aggregation_f,n_slice,top_n_per_country)
  data_arc_collected=switch(type,
                            "int"=data_arc_collected%>%
                              rename("move"="pred_seed1"),
                            "nat"=data_arc_collected%>%
                              rename("move"="sex_nat"))
  
  color_arc_source=switch(type,
                          "nat"="#0009ff",
                          "int"="#9d8c70")
  color_arc_target=switch(type,
                          "nat"="#6e71ca",
                          "int"="#badebd")
  # labels
  gender_label=switch(sex,
                      "M"="Male",
                      "F"="Female",
                      "all"="Female + male")
  direction_label="emigration from"
  if(direction=="M"){direction_label="immigration to"}
  
  type_label="international"
  if(type=="nat"){type_label="internal"}
  
  if(aggregation_f=="country"){
    name_label=paste("from", data_arc_collected$COUNTRY_NAME_I,
                     "to", data_arc_collected$COUNTRY_NAME_J)
  }else{
    name_label=paste("from", data_arc_collected$ADMIN_NAME_I, paste0("(",data_arc_collected$COUNTRY_NAME_I,")"),
                     "to", data_arc_collected$ADMIN_NAME_J, paste0("(",data_arc_collected$COUNTRY_NAME_J,")"))
  }
  
  
  label_arc=paste(gender_label,type_label,"migration",
                  "<br/>",name_label,
                  "<br/>Count:", sapply(data_arc_collected$move,format_n))
  
  data_arc_collected$label_arc=label_arc
  
  # width
  if(nrow(data_arc_collected)>20){
    data_arc_collected$width=as.numeric(cut(data_arc_collected$move,
                                            unique(quantile(data_arc_collected$move,
                                                     c(seq(0,0.9,0.1),seq(0.91,1,0.01))))))
    
    data_arc_collected=data_arc_collected%>%
      mutate(width=ifelse(width<=19,width,width^2))
  }else{
    data_arc_collected$width=10*rank(data_arc_collected$move)/max(rank(data_arc_collected$move))
  }
  
  # legend
  legend_arc <- legend_element(
    title = "Arc legend"
    ,variables = c("Origin", "Destination")
    , colours = c(color_arc_source, color_arc_target) 
    , colour_type = "fill"
    , variable_type = "category"
    ,css = "zoom: 1;"
  )
  legend_arc_js = mapdeck_legend(legend_arc)
  
  # deck object
  deck_object=mapdeck_update(map_id = "deck")
  deck_object=deck_object%>%
    add_arc(data =data_arc_collected 
            , origin=c("x_from","y_from")
            , destination=c("x_to","y_to")
            , tooltip="label_arc"
            , stroke_from=color_arc_source
            , stroke_to=color_arc_target
            , stroke_width="width"
            , tilt="tilt"
            , layer_id="arc"
            , auto_highlight=TRUE
            , focus_layer=FALSE
            , update_view = FALSE
            , legend = legend_arc_js
            , transitions= list( origin = 700, destination = 700, stroke_from = 700, stroke_to = 700, stroke_width = 700)
    )
  return(deck_object)
}  
# aggregation="admin"
# n_slice="10"
# function_arc(mig_db,deck_proxy,country_clicked,admin_clicked,focus,type,direction,sex,aggregation,n_slice,top_n_per_country)

