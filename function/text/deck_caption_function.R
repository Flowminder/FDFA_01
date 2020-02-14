deck_caption_function=function(mig_db,country_clicked,admin_clicked,graph_type,focus,type,direction,sex,aggregation,n_slice){ 
  
  # null parameters
  if(is.null(country_clicked)){country_clicked="AFG"}
  if(is.null(admin_clicked)){admin_clicked="13"}
  if(is.null(focus)){focus="global"}
  if(is.null(type)){type="int"}
  if(is.null(direction)){direction="X"}
  if(is.null(sex)){sex="F"}
  if(is.null(aggregation)){aggregation="admin"}
  if(is.null(n_slice)){n_slice="3"}
  
  if(type[1]!="NULL"&direction[1]!="NULL"&graph_type[1]!="NULL"){
    sex_text=switch(sex,
                    "F"="Female",
                    "M"="Male",
                    "all"="Female and male")
    
    if(all(c("int","nat")%in%type)){int_nat_text="internal and international"}
    if(all(type==c("int"))){int_nat_text="international"}
    if(all(type==c("nat"))){int_nat_text="internal"}
    
    if(all(c("X","M")%in%direction)){direction_text="emigration and immigration"}
    if(all(direction==c("X"))){direction_text="emigration"}
    if(all(direction==c("M"))){direction_text="immigration"}
    
    # admin_names_tbl=tbl(mig_db,"admin_names")
    
    # country_text=admin_names_tbl%>%
    #   filter(ISO==country_clicked)%>%
    #   distinct(COUNTRY_NAME)%>%
    #   collect()%>%
    #   top_n(1)
    
    # admin_text=admin_names_tbl%>%
    #   collect(n=Inf)%>%
    #   filter(JOIN_ID==paste0(country_clicked,"_",admin_clicked))%>%
    #   distinct(ADMIN_NAME)
    
    # location_text=switch(focus,
    #                      "global"="",
    #                      "country"=paste("in",country_text),
    #                      "admin"=paste0("in ",admin_text," (",country_text,")")
    # )
    
    # n_text=paste0(": top ",n_slice, " relations per administrative unit")
    # if(aggregation=="country"){
    #   n_text=paste0(": top ",n_slice, " relations per country")
    # }
  
  text_to_show=paste(sex_text,int_nat_text,direction_text) #,n_text #,location_text
  }else{
    text_to_show="Tick a visualisation box below the map"
  }
  
  return(text_to_show)
}
