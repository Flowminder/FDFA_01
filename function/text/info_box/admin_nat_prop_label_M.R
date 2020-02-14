admin_nat_prop_label_M=function(mig_db,simple_countries_df,country_clicked,admin_clicked,sex){
  
  if(sex=="all"){
  # null parameters
  if(is.null(country_clicked)){country_clicked="AFG"}
  if(is.null(admin_clicked)){admin_clicked="13"}

  data_col_1_collected_all=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,"admin","nat","M","all","admin")
  data_col_1_collected_F=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,"admin","nat","M","F","admin")
  
  
  format_perc=function(x){
    paste0(format(round(x*100,0), trim = TRUE), "%")
  }
  nat_prop_M=paste0("(", format_perc(data_col_1_collected_F$move/data_col_1_collected_all$move)," female)")
  
  }
  if(sex=="F"){
    data_col_1_collected_all=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,"admin","nat","M","all","admin")
    data_col_1_collected_F=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,"admin","nat","M","F","admin")
    
    
    format_perc=function(x){
      paste0(format(round(x*100,0), trim = TRUE), "%")
    }
    nat_prop_M=paste0("(", format_perc(data_col_1_collected_F$move/data_col_1_collected_all$move)," of total)")
    
  }
  
  if(sex=="M"){
    data_col_1_collected_all=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,"admin","nat","M","all","admin")
    data_col_1_collected_M=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,"admin","nat","M","M","admin")
    
    
    format_perc=function(x){
      paste0(format(round(x*100,0), trim = TRUE), "%")
    }
    nat_prop_M=paste0("(", format_perc(data_col_1_collected_M$move/data_col_1_collected_all$move)," of total)")
    
  }
  return(nat_prop_M)
}