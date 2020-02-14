admin_prop_label_int_X=function(mig_db,simple_countries_df,country_clicked,admin_clicked,sex_f){
  
  if(sex_f=="all"){
  # null parameters
  if(is.null(country_clicked)){country_clicked="AFG"}
  if(is.null(admin_clicked)){admin_clicked="13"}

  data_col_1_collected_all=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,"admin","int","X","all","admin")
  
  data_col_1_collected_F=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,"admin","int","X","F","admin")
  

  format_perc=function(x){
    paste0(format(round(x*100,0), trim = TRUE), "%")
  }
  
  int_prop_X=paste0("(", format_perc(data_col_1_collected_F$move/data_col_1_collected_all$move)," female)")
  }
  
  if(sex_f=="F"){
    # null parameters
    if(is.null(country_clicked)){country_clicked="AFG"}
    if(is.null(admin_clicked)){admin_clicked="13"}
    
    data_col_1_collected_all=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,"admin","int","X","all","admin")
    
    data_col_1_collected_F=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,"admin","int","X","F","admin")
    if(nrow(data_col_1_collected_F)==0){data_col_1_collected_F=data.frame(move=0)}
    
    
    format_perc=function(x){
      paste0(format(round(x*100,0), trim = TRUE), "%")
    }
    
    int_prop_X=paste0("(", format_perc(data_col_1_collected_F$move/data_col_1_collected_all$move)," of total)")
  }
  
  if(sex_f=="M"){
    # null parameters
    if(is.null(country_clicked)){country_clicked="AFG"}
    if(is.null(admin_clicked)){admin_clicked="13"}
    
    data_col_1_collected_all=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,"admin","int","X","all","admin")
    
    data_col_1_collected_M=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,"admin","int","X","M","admin")
    
    
    format_perc=function(x){
      paste0(format(round(x*100,0), trim = TRUE), "%")
    }
    
    int_prop_X=paste0("(", format_perc(data_col_1_collected_M$move/data_col_1_collected_all$move)," of total)")
  }
  return(int_prop_X)
  
}