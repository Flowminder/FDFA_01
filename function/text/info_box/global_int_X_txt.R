global_int_X_txt=function(mig_db, sex_f){
  global_fig=tbl(mig_db,"global_fig")%>%
    collect(n=Inf)
  
  format_perc=function(x){
    paste0(format(round(x*100,0), trim = TRUE), "%")
  }
  
  int_mig_sex=global_fig%>%
    filter(sex==sex_f)%>%
    select(int_number)
  
  int_mig=format_n(int_mig_sex$int_number) #paste0(format_n(global_fig$int_number[1])," (", format_perc(global_fig$int_percent[2])," female)")
  return(int_mig)
}


