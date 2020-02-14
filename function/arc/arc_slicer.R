arc_slicer=function(mig_db,country_clicked,admin_clicked,focus,type,direction,sex,aggregation_f,n_slice,top_n_per_country){
  
  selected_country=country_clicked
  selected_admin=paste(c(country_clicked,admin_clicked),collapse = "_")
  
  
  # select table 
  data_arc=switch(type,
                  "int"=tbl(mig_db,paste0(c("GLOBAL_mig",type,direction,n_slice,sex,aggregation_f),collapse="_")),
                  "nat"=tbl(mig_db,paste0(c("GLOBAL_mig",type,direction,n_slice,sex,"admin"),collapse="_")))
  # filter data
  data_arc_focused=switch(focus,
                          "global"=switch(direction,
                                          "X"=data_arc%>%
                                            rename("ISO"="ISOI"),
                                          "M"=data_arc%>%
                                            rename("ISO"="ISOJ")),
                          "country"=switch(direction,
                                           "X"=data_arc%>%
                                             filter(ISOI==selected_country)%>%
                                             rename("ISO"="ISOI"),
                                           "M"=data_arc%>%
                                             filter(ISOJ==selected_country)%>%
                                             rename("ISO"="ISOJ")),
                          "admin"=switch(direction,
                                         "X"=data_arc%>%
                                           filter(JOIN_ID_I==selected_admin)%>%
                                           rename("ISO"="ISOI"),
                                         "M"=data_arc%>%
                                           filter(JOIN_ID_J==selected_admin)%>%
                                           rename("ISO"="ISOJ"))
  )
  
  #collect data
  data_arc_collected=collect(data_arc_focused,n=Inf)
  
  # Further slice if top_n_per_country=T
  if(top_n_per_country==T){
    data_arc_collected=switch(type,
                              "int"=data_arc_collected%>%
                                group_by(ISO)%>%
                                top_n(as.numeric(n_slice),pred_seed1),
                              "nat"=data_arc_collected%>%
                                group_by(ISO)%>%
                                top_n(as.numeric(n_slice),sex_nat))
  }
  return(data_arc_collected)
}