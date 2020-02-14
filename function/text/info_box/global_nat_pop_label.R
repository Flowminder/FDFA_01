global_nat_pop_label=function(mig_db,sex_f){
  
  if(sex_f=="all"){
    global_fig=tbl(mig_db,"global_fig")%>%
      collect(n=Inf)%>%
      filter(sex=="F")%>%
      select(nat_percent)
    
    format_perc=function(x){
      paste0(format(round(x*100,0), trim = TRUE), "%")
    }
    
    nat_prop=paste0("(", format_perc(global_fig)," female)")
  }else{
    global_fig=tbl(mig_db,"global_fig")%>%
      collect(n=Inf)%>%
      filter(sex==sex_f)%>%
      select(nat_percent)
    
    format_perc=function(x){
      paste0(format(round(x*100,0), trim = TRUE), "%")
    }
    
    nat_prop=paste0("(", format_perc(global_fig)," of total)")
  }
  
  
  return(nat_prop)
}


