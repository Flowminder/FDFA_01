admin_int_M_txt=function(mig_db,simple_countries_df,country_clicked,admin_clicked,sex_f){
  
  # null parameters
  if(is.null(country_clicked)){country_clicked="AFG"}
  if(is.null(admin_clicked)){admin_clicked="13"}

  data_col_1_collected=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,"admin","int","M",sex_f,"admin")
  int_M_sex=data_col_1_collected%>%
    collect(n=Inf)%>%
    select(move)
  if(nrow(int_M_sex)==0){int_M_sex=data.frame(move=0)}
  

  format_perc=function(x){
    paste0(format(round(x*100,0), trim = TRUE), "%")
  }
  
  int_mig_M=format_n(int_M_sex$move) #paste0(format_n(int_X_all$move)," (", format_perc(int_X_F$move/int_X_all$move)," female)")
  
  return(int_mig_M)
  
}