admin_fig_function_topline=function(mig_db,simple_countries_df,country_clicked,admin_clicked){
  
  # null parameters
  if(is.null(country_clicked)){country_clicked="AFG"}
  if(is.null(admin_clicked)){admin_clicked="13"}
  # if(is.null(focus)){focus="global"}
  # if(is.null(type)){type="int"}
  # if(is.null(direction)){direction="X"}
  # if(is.null(sex)){sex="F"}
  # if(is.null(aggregation)){aggregation="admin"}
  # if(is.null(n_slice)){n_slice="3"}
  # if(is.null(top_n_per_country)){top_n_per_country=FALSE}
  
  COUNTRY_TEXT=tbl(mig_db,"admin_names")%>%
    collect(n=Inf)%>%
    filter(JOIN_ID==paste0(c(country_clicked,admin_clicked),collapse = "_"))%>%
    distinct(COUNTRY_NAME)%>%
    top_n(1)
  ADMIN_TEXT=tbl(mig_db,"admin_names")%>%
    collect(n=Inf)%>%
    filter(JOIN_ID==paste0(c(country_clicked,admin_clicked),collapse = "_"))%>%
    distinct(ADMIN_NAME)%>%
    top_n(1)
                                  
  data_col_1_collected=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,"admin","int","X","all","admin")
  int_X_all=data_col_1_collected%>%
    filter(sex=="all")
  int_X_F=data_col_1_collected%>%
    filter(sex=="F")
  int_X_M=data_col_1_collected%>%
    filter(sex=="M")
  
  data_col_1_collected=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,"admin","nat","X","all","admin")
  nat_X_all=data_col_1_collected%>%
    filter(sex=="all")
  nat_X_F=data_col_1_collected%>%
    filter(sex=="F")
  nat_X_M=data_col_1_collected%>%
    filter(sex=="M")
  
  data_col_1_collected=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,"admin","int","M","all","admin")
  int_M_all=data_col_1_collected%>%
    filter(sex=="all")
  int_M_F=data_col_1_collected%>%
    filter(sex=="F")
  int_M_M=data_col_1_collected%>%
    filter(sex=="M")
  
  data_col_1_collected=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,"admin","nat","M","all","admin")
  nat_M_all=data_col_1_collected%>%
    filter(sex=="all")
  nat_M_F=data_col_1_collected%>%
    filter(sex=="F")
  nat_M_M=data_col_1_collected%>%
    filter(sex=="M")
  
  format_k=function(x){
    paste(format(round(x / 1e3, 0), trim = TRUE), "k")
  }
  format_perc=function(x){
    paste0(format(round(x*100,0), trim = TRUE), "%")
  }
  
  int_mig_X=paste0(format_k(int_X_all$move)," (", format_perc(int_X_F$move/int_X_all$move)," female)")
  int_mig_M=paste0(format_k(int_M_all$move)," (", format_perc(int_M_F$move/int_M_all$move)," female)")
  
  nat_mig_X=paste0(format_k(nat_X_all$move)," (", format_perc(nat_X_F$move/nat_X_all$move)," female)")
  nat_mig_M=paste0(format_k(nat_M_all$move)," (", format_perc(nat_M_F$move/nat_M_all$move)," female)")
  head_text=paste0(ADMIN_TEXT$ADMIN_NAME," (",COUNTRY_TEXT$COUNTRY_NAME,")")

  HTML("<p><h3><b>",head_text," Summary</b></h3></p>",
       "<h4>",
       "<p><b> International emigrants:</b>",int_mig_X,"</p>",
       "<p><b> International immigrants:</b>",int_mig_M,"</p>",
       "<br>",
       "<p><b> Internal emigrants:</b>",nat_mig_X,"</p>",
       "<p><b> Internal immigrants:</b>",nat_mig_M,"</p>",
       "</h4>")
}