global_nat_X_txt=function(mig_db,sex_f){
  global_fig=tbl(mig_db,"global_fig")%>%
    collect(n=Inf)%>%
    filter(sex==sex_f)%>%
    select(nat_number)
  
  format_perc=function(x){
    paste0(format(round(x*100,0), trim = TRUE), "%")
  }
  
  nat_mig=format_n(global_fig$nat_number) #paste0(format_n(global_fig$nat_number[1])," (", format_perc(global_fig$nat_percent[2])," female)")
  return(nat_mig)
}