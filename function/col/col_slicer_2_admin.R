col_slicer_2_admin=function(mig_db,country_clicked,admin_clicked,focus,type,direction,sex,aggregation,n_slice){

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
                          rename("in_ID"="JOIN_ID_I",
                                 "out_ID"="JOIN_ID_J",
                                 "x_in"="x_from",
                                 "y_in"="y_from",
                                 "x_out"="x_to",
                                 "y_out"="y_to",
                                 "COUNTRY_NAME_in"="COUNTRY_NAME_I",
                                 "ADMIN_NAME_in"="ADMIN_NAME_I",
                                 "COUNTRY_NAME_out"="COUNTRY_NAME_J",
                                 "ADMIN_NAME_out"="ADMIN_NAME_J",
                                 "move"=move_name)%>%
                          mutate(ID_filter=in_ID,
                                 ISO_filter=substr(ID_filter,1,3),
                                 ID_group_by=out_ID,
                                 ISO_group_by=substr(ID_group_by,1,3)),
                        "M"=data_col_2%>%
                          rename("in_ID"="JOIN_ID_J",
                                 "out_ID"="JOIN_ID_I",
                                 "x_in"="x_to",
                                 "y_in"="y_to",
                                 "x_out"="x_from",
                                 "y_out"="y_from",
                                 "COUNTRY_NAME_in"="COUNTRY_NAME_J",
                                 "ADMIN_NAME_in"="ADMIN_NAME_J",
                                 "COUNTRY_NAME_out"="COUNTRY_NAME_I",
                                 "ADMIN_NAME_out"="ADMIN_NAME_I",
                                 "move"=move_name)%>%
                          mutate(ID_filter=in_ID,
                                 ISO_filter=substr(ID_filter,1,3),
                                 ID_group_by=out_ID,
                                 ISO_group_by=substr(ID_group_by,1,3))
  )
  
  # filter the data ####
  data_col_filtered=switch(focus,
                        "global"=data_col_relab,
                        "country"=data_col_relab%>%
                          filter(ISO_filter==country_clicked),
                        "admin"=data_col_relab%>%
                          filter(ID_filter==admin_selected)
  )
  
  # collect ####
  data_col_collected=collect(data_col_filtered, n=Inf)  
  # get total at outination/origin
  
  data_col_aggregated=data_col_collected%>%
    group_by(ID_group_by)%>%
    summarise(move=sum(move),  # variable
              lon=mean(x_out),  # x_to
              lat=mean(y_out),
              COUNTRY_NAME=unique(COUNTRY_NAME_out), #_name
              ADMIN_NAME=unique(ADMIN_NAME_out))
  
  return(data_col_aggregated)
}