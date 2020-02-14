col_slicer_2_country=function(mig_db,country_clicked,admin_clicked,focus,type,direction,sex,aggregation,n_slice){
  
  # filter and collect data 2 ####
  admin_selected=paste0(country_clicked,"_",
                        admin_clicked)
  data_col_2=switch(type,
                    "int"=tbl(mig_db,paste0(c("GLOBAL_mig",type,direction,n_slice,sex,aggregation),collapse="_")),
                    "nat"=tbl(mig_db,paste0(c("GLOBAL_mig",type,direction,n_slice,sex,"admin"),collapse="_")))
  # rename data
  move_name=switch(type,
                   "int"="pred_seed1",
                   "nat"="sex_nat")
  
  data_col_relab=switch(direction,
                        "X"=data_col_2%>%
                          rename("in_ID"="ISOI",
                                 "out_ID"="ISOJ",
                                 "x_in"="x_from",
                                 "y_in"="y_from",
                                 "x_out"="x_to",
                                 "y_out"="y_to",
                                 "COUNTRY_NAME_in"="COUNTRY_NAME_I",
                                 "COUNTRY_NAME_out"="COUNTRY_NAME_J",
                                 "move"=move_name)%>%
                          mutate(ISO_filter=in_ID,
                                 ISO_group_by=out_ID),
                        "M"=data_col_2%>%
                          rename("in_ID"="ISOJ",
                                 "out_ID"="ISOI",
                                 "x_in"="x_to",
                                 "y_in"="y_to",
                                 "x_out"="x_from",
                                 "y_out"="y_from",
                                 "COUNTRY_NAME_in"="COUNTRY_NAME_J",
                                 "COUNTRY_NAME_out"="COUNTRY_NAME_I",
                                 "move"=move_name)%>%
                          mutate(ISO_filter=in_ID,
                                 ISO_group_by=out_ID)
  )
  
  # filter the data ####
  data_col_filtered=switch(focus,
                           "global"=data_col_relab,
                           "country"=data_col_relab%>%
                             filter(ISO_filter==country_clicked)
  )
  
  # collect ####
  data_col_collected=collect(data_col_filtered, n=Inf)  
  
  # get total at outination/origin
  data_col_aggregated=data_col_collected%>%
    group_by(ISO_group_by)%>%
    summarise(move=sum(move),  # variable
              lon=mean(x_out),  # x_to
              lat=mean(y_out),
              COUNTRY_NAME=unique(COUNTRY_NAME_out))
  
  return(data_col_aggregated)
}