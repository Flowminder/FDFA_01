function_bar=function(mig_db,simple_countries_df,country_clicked,admin_clicked,focus,type,direction,sex,aggregation,n_slice){
 
  # null parameters
  if(is.null(country_clicked)){country_clicked="AFG"}
  if(is.null(admin_clicked)){admin_clicked="13"}
  if(is.null(focus)){focus="global"}
  if(is.null(type)){type="int"}
  if(is.null(direction)){direction="X"}
  if(is.null(sex)){sex="F"}
  if(is.null(aggregation)){aggregation="admin"}
  if(is.null(n_slice)){n_slice="3"}
  # if(is.null(top_n_per_country)){top_n_per_country=FALSE}
  
  aggregation_f=aggregation
  if(focus=="admin"){aggregation_f="admin"}
  if(focus=="country"){aggregation_f="admin"}
  
  
  n_slice_f=as.numeric(n_slice)
  if(n_slice_f<3){n_slice_f==3}
  
  JOIN_ID_f=paste(c(country_clicked,admin_clicked),collapse="_")
  # filter and collect data ####
  data_bar_collected=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,focus,type,direction,sex,aggregation_f)
  
  if(focus=="admin"){
    data_bar_collected=col_slicer_2_admin(mig_db,country_clicked,admin_clicked,focus,type,direction,sex,aggregation_f,n_slice)
  }
  
  ALL_GOOD=T
  if(nrow(data_bar_collected)==0){
    data_bar_collected=data.frame(move=array(0,10),
                                  ADMIN_NAME=array("NA",10),
                                  COUNTRY_NAME=array("NA",10))
    ALL_GOOD=F
  }
  if(aggregation_f=="country"){
    data_bar_top=data_bar_collected%>%
      arrange(desc(move))%>%
      top_n(as.numeric(n_slice_f),move)%>%
      mutate(order=1:n(),
             name_f=factor(order,
                           labels=name))    
  }
  
  if(aggregation_f=="admin"){
    data_bar_top=data_bar_collected%>%
      arrange(desc(move))%>%
      top_n(as.numeric(n_slice_f),move)
    
    if(focus=="global"){
      data_bar_top=data_bar_top%>%
        mutate(order=1:n(),
               name_f=factor(order,
                             labels=ADMIN_NAME)) #paste0(ADMIN_NAME, " (",COUNTRY_NAME,")")
    }else{
      data_bar_top=data_bar_top%>%
        mutate(order=1:n(),
               name_f=factor(order,
                             labels=ADMIN_NAME))
    }
    
  }
  
  
  # labels ####
  aggregation_label=switch(aggregation_f,
                           "country"="Countries",
                           "admin"="Administrative Units")
  
  sex_label=switch(sex,
                   "F"="Female",
                   "M"="Male",
                   "all"="Female and Male")
  
  type_label=switch(type,
                    "int"="International",
                    "nat"="Internal")
  
  direction_label=switch(direction,
                         "X"="Emigration",
                         "M"="Immigration")
  if(focus=="admin"){
    dest_or_label=switch(direction,
                         "X"="Destinations",
                         "M"="Origins")
    from_to_label=switch(direction,
                         "X"="from",
                         "M"="to")
    
  }
  if(type=="nat"&focus=="global"){direction_label="Migration"}
  
  
  location_text=switch(focus,
                       "global"="",
                       "country"=paste0("\n(",tbl(mig_db,"admin_names")%>%
                                          collect(n=Inf)%>%
                                          filter(ISO==country_clicked)%>%
                                          distinct(COUNTRY_NAME)%>%
                                          top_n(1),")"),
                       "admin"=tbl(mig_db,"admin_names")%>%
                         collect(n=Inf)%>%
                         filter(JOIN_ID==JOIN_ID_f)%>%
                         distinct(ADMIN_NAME)%>%
                         top_n(1))
  
  title_s=paste0("Top ", aggregation_label, " by\n",sex_label," ",type_label, " ",direction_label,location_text)
  
  if(focus=="admin"){
    title_s=paste0("Top ", dest_or_label, " of\n",sex_label," ",type_label, " ",direction_label," ","\n", from_to_label," ",location_text)
    
  }
  if(ALL_GOOD==F){
    title_s="ZERO MIGRATION ESTIMATED"
  }
  # plot parameters #####
  m <- list( # plot margins
    l = 50,
    r = 50,
    b = 50,
    t = 100,
    pad = 4
  )
  
  # plot #####
  p=plot_ly(data_bar_top,
            x=~name_f,
            y = ~move,
            type="bar")%>%
    layout(title=title_s,
           yaxis=list(title=""),
           xaxis=list(title=""),
           margin=m)
  
  if(focus%in%c("admin","global")&type=="int"&aggregation_f!="country"){
    p=plot_ly(data_bar_top,
              x=~name_f,
              y= ~move,
              color =  ~factor(COUNTRY_NAME),
              colors = 'Dark2',
              type="bar")%>%
      layout(title=title_s,
             yaxis=list(title=""),
             xaxis=list(title="",tickangle = 45,showticklabels=T),
             margin=m)%>%
      layout(showlegend = T)
  }
    
  return(p)
}