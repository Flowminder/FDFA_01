col_slicer_1=function(mig_db,simple_countries_df,country_clicked,admin_clicked,focus,type,direction,sex,aggregation){
  
  aggregation_f=aggregation
  if(focus=="admin"){aggregation_f="admin"}
  JOIN_ID_f=paste(c(country_clicked,admin_clicked),collapse="_")
  sex_f=sex
  # filter and collect data 1 ####
  data_col_1=switch(direction,
                    "X"=switch(type,
                               "int"=tbl(mig_db,"EM_int"),
                               "nat"=tbl(mig_db,"EM_nat")),
                    "M"=switch(type,
                               "int"=tbl(mig_db,"IM_int"),
                               "nat"=tbl(mig_db,"IM_nat"))
  )
  data_col_1=switch(focus,
                    "global"=data_col_1%>%
                      filter(sex==sex_f),
                    "country"=data_col_1%>%
                      filter(sex==sex_f,
                             ISO==country_clicked),
                    "admin"=data_col_1%>%
                      filter(sex==sex_f,
                             JOIN_ID==JOIN_ID_f)
  )
  
  data_col_1_collected=collect(data_col_1,n=Inf)
  
  if(aggregation_f=="country"){
    data_col_1_collected=data_col_1_collected%>%
      group_by(ISO)%>%
      summarise(move=sum(move,na.rm=T))%>%
      left_join(simple_countries_df,
                by="ISO")
  }
  return(data_col_1_collected)
}
