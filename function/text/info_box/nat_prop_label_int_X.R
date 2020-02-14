nat_prop_label_int_X=function(mig_db,country_clicked,sex_f){
  if(sex_f=="all"){
  # null parameters
  if(is.null(country_clicked)){country_clicked="AFG"}
  
  nat_fig=tbl(mig_db,"nat_fig")%>%
    collect(n=Inf)%>%
    filter(ISO==country_clicked,
           sex=="F")%>%
    select(int_percent_X)
  
  format_perc=function(x){
    paste0(format(round(x*100,0), trim = TRUE), "%")
  }
  
  int_prop_X=paste0("(", format_perc(nat_fig)," female)")
  }else{
    # null parameters
    if(is.null(country_clicked)){country_clicked="AFG"}
    
    nat_fig=tbl(mig_db,"nat_fig")%>%
      collect(n=Inf)%>%
      filter(ISO==country_clicked,
             sex==sex_f)%>%
      select(int_percent_X)
    
    format_perc=function(x){
      paste0(format(round(x*100,0), trim = TRUE), "%")
    }
    
    int_prop_X=paste0("(", format_perc(nat_fig)," of total)")
  }
  
  return(int_prop_X)
}