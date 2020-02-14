nat_nat_X_text=function(mig_db,country_clicked,sex_f){
  # null parameters
  if(is.null(country_clicked)){country_clicked="AFG"}

  nat_fig=tbl(mig_db,"nat_fig")%>%
    collect(n=Inf)%>%
    filter(ISO==country_clicked,
           sex==sex_f)%>%
    select(nat_number)
  
  format_perc=function(x){
    paste0(format(round(x*100,0), trim = TRUE), "%")
  }

  nat_mig=format_n(nat_fig$nat_number) #paste0(format_k(nat_fig$nat_number[1])," (", format_perc(nat_fig$nat_percent[2])," female)")
  return(nat_mig)
}