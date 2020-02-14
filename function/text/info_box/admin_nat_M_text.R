admin_nat_M_text=function(mig_db,simple_countries_df,country_clicked,admin_clicked,sex_f){
  
  # null parameters
  if(is.null(country_clicked)){country_clicked="AFG"}
  if(is.null(admin_clicked)){admin_clicked="13"}

  data_col_1_collected=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,"admin","nat","M",sex_f,"admin")
  nat_M_sex=data_col_1_collected%>%
    select(move)

  nat_mig_M=format_n(nat_M_sex$move) #paste0(format_k(nat_M_all$move)," (", format_perc(nat_M_F$move/nat_M_all$move)," female)")
  return(nat_mig_M)
}