# function_arc ####
function_arc=function(mig_db,deck_proxy,country_clicked,admin_clicked,focus,type,direction,sex,aggregation,n_slice,top_n_per_country){
  
  aggregation_f=aggregation
  if(focus=="admin"){aggregation_f="admin"}
  
  arc_transp=1
  arc_width=1
  
  # filter and collect the data
  data_arc_collected=arc_slicer(mig_db,country_clicked,admin_clicked,focus,type,direction,sex,aggregation_f,n_slice,top_n_per_country)
  
  color_arc_source=switch(type,
                          "nat"=c(0, 9, 255),
                          "int"=c(157,140,112))
  color_arc_target=switch(type,
                          "nat"=c(110, 113, 202),
                          "int"=c(186, 222, 189))
  # deck object
  deck_object=deck_proxy%>%
    add_arc_layer( # arc 
      data = data_arc_collected,   
      autoHighlight=TRUE,
      opacity=1,
      getHeight=1,
      getWidth=get_property("width"),
      getSourcePosition=get_position("y_from", "x_from"),
      getTargetPosition=get_position("y_to", "x_to"),
      getSourceColor = color_arc_source,
      getTargetColor = color_arc_target, 
      getTooltip=JS("data => `International migration movements <br/>from ${data.ADMIN_NAME_I} (${data.COUNTRY_NAME_I}) to ${data.ADMIN_NAME_J} (${data.COUNTRY_NAME_J}) <br/>Count: ${Math.round(data.pred_seed1)}`"),
      getTilt=get_property("tilt"),
      id="arc",
      legend = TRUE,
      pickable=TRUE)
  return(deck_object)
}  

