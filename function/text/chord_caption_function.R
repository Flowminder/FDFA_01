chord_caption_function=function(mig_db,country_clicked,focus,type,direction,sex,n_slice){ 
  
  # null parameters
  if(is.null(country_clicked)){country_clicked="AFG"}
  if(is.null(focus)){focus="global"}
  if(is.null(type)){type="int"}
  if(is.null(direction)){direction="X"}
  if(is.null(sex)){sex="F"}
  # if(is.null(aggregation)){aggregation="admin"}
  if(is.null(n_slice)){n_slice="3"}
  # if(is.null(top_n_per_country)){top_n_per_country=FALSE}
  
  sex_text=switch(sex,
                  "F"="Female",
                  "M"="Male",
                  "all"="Female and male")
  
  if(type=="int"){int_nat_text="international"}
  if(type=="nat"){int_nat_text="national"}
  
  if(direction=="X"){direction_text="emigration"}
  if(direction=="M"){direction_text="immigration"}
  
  admin_names_tbl=tbl(mig_db,"admin_names")
  
  country_text=admin_names_tbl%>%
    filter(ISO==country_clicked)%>%
    distinct(COUNTRY_NAME)%>%
    collect()%>%
    top_n(1)
  
  location_text=switch(focus,
                       "global"="in the Global South",
                       "country"=paste("in",country_text)
  )
  
  country_admin=switch(type,
                       "int"="country",
                       "nat"="administrative unit")
  
  Title=paste(sex_text,int_nat_text,"migration movements",location_text)
  
  # SubTitle=paste0("(filtered according to top ",n_slice," ",int_nat_text, " ",direction_text," movements per ",country_admin,")")
  
  HTML("<h3>",
       "<p>",Title,"</p>",
       # "<p><i>",SubTitle,"</i></p>",
       "<h3/>")
}
