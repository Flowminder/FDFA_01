nat_int_M_txt=function(mig_db,country_clicked,sex_f){
  # null parameters
  if(is.null(country_clicked)){country_clicked="AFG"}
  
  nat_fig=tbl(mig_db,"nat_fig")%>%
    collect(n=Inf)%>%
    filter(ISO==country_clicked,
           sex==sex_f)%>%
    select(int_number_M)
  
  format_perc=function(x){
    paste0(format(round(x*100,0), trim = TRUE), "%")
  }
  
  int_mig_M=format_n(nat_fig$int_number_M) #paste0(format_m(nat_fig$int_number_X[1])," (", format_perc(nat_fig$int_percent_X[2])," female)")
  
  return(int_mig_M)
}