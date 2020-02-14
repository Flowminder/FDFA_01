country_text=function(mig_db,country_clicked){
  
  if(is.null(country_clicked)){country_clicked="AFG"}
  
  COUNTRY_TEXT=tbl(mig_db,"admin_names")%>%
    collect(n=Inf)%>%
    filter(ISO==country_clicked)%>%
    distinct(COUNTRY_NAME)%>%
    top_n(1)
  
  return(COUNTRY_TEXT$COUNTRY_NAME)
}