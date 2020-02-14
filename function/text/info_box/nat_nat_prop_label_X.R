nat_nat_prop_label_X=function(mig_db,country_clicked,sex_f){
  if(sex_f=="all"){
    # null parameters
    if(is.null(country_clicked)){country_clicked="AFG"}
    
    nat_fig=tbl(mig_db,"nat_fig")%>%
      collect(n=Inf)%>%
      filter(ISO==country_clicked,
             sex=="F")%>%
      select(nat_percent)
    
    format_perc=function(x){
      paste0(format(round(x*100,0), trim = TRUE), "%")
    }
    
    nat_prop=paste0("(", format_perc(nat_fig)," female)")
  }else{
    # null parameters
    if(is.null(country_clicked)){country_clicked="AFG"}
    
    nat_fig=tbl(mig_db,"nat_fig")%>%
      collect(n=Inf)%>%
      filter(ISO==country_clicked,
             sex==sex_f)%>%
      select(nat_percent)
    
    format_perc=function(x){
      paste0(format(round(x*100,0), trim = TRUE), "%")
    }
    
    nat_prop=paste0("(", format_perc(nat_fig)," of total)")
  }
  return(nat_prop)
}