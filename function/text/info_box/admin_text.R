admin_text=function(mig_db,country_clicked,admin_clicked){
  
  if(is.null(country_clicked)){country_clicked="AFG"}
  if(is.null(admin_clicked)){admin_clicked="13"}

  ADMIN_TEXT=tbl(mig_db,"admin_names")%>%
    collect(n=Inf)%>%
    filter(JOIN_ID==paste0(c(country_clicked,admin_clicked),collapse = "_"))%>%
    distinct(ADMIN_NAME)%>%
    top_n(1)
  
  return(ADMIN_TEXT)
}