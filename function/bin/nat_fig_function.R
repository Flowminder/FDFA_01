nat_fig_function=function(country_clicked){
  
  
  COUNTRY_TEXT=tbl(mig_db,"admin_names")%>%
    collect(n=Inf)%>%
    filter(ISO==country_clicked)%>%
    distinct(COUNTRY_NAME)%>%
    top_n(1)
  
  nat_fig=tbl(mig_db,"nat_fig")%>%
    collect(n=Inf)%>%
    filter(ISO==country_clicked)
    
  format_k=function(x){
    paste(format(round(x / 1e3, 0), trim = TRUE), "k")
  }
  format_perc=function(x){
    paste0(format(round(x*100,0), trim = TRUE), "%")
  }
  
  int_mig_X=paste0(format_k(nat_fig$int_number_X[1])," (", format_perc(nat_fig$int_percent_X[2])," female)")
  int_mig_M=paste0(format_k(nat_fig$int_number_M[1])," (", format_perc(nat_fig$int_percent_M[2])," female)")
  
  nat_mig=paste0(format_k(nat_fig$nat_number[1])," (", format_perc(nat_fig$nat_percent[2])," female)")

  dest_1_int=paste0("1. ",nat_fig$int_top_dest_1_name[1], ": ",format_k(nat_fig$int_top_dest_1_number[1]),", ",format_perc(nat_fig$int_top_dest_1_percent[1])," of total, ",format_perc(nat_fig$int_top_dest_1_number[2]/nat_fig$int_top_dest_1_number[1]), " are women")
  dest_2_int=paste0("2. ",nat_fig$int_top_dest_2_name[1], ": ",format_k(nat_fig$int_top_dest_2_number[2]),", ",format_perc(nat_fig$int_top_dest_2_percent[2])," of total, ",format_perc(nat_fig$int_top_dest_2_number[2]/nat_fig$int_top_dest_2_number[1]), " are women")
  dest_3_int=paste0("3. ",nat_fig$int_top_dest_3_name[1], ": ",format_k(nat_fig$int_top_dest_3_number[2]),", ",format_perc(nat_fig$int_top_dest_3_percent[2])," of total, ",format_perc(nat_fig$int_top_dest_3_number[2]/nat_fig$int_top_dest_3_number[1]), " are women")
  
  source_1_int=paste0("1. ",nat_fig$int_top_source_1_name[1], ": ",format_k(nat_fig$int_top_source_1_number[1]),", ",format_perc(nat_fig$int_top_source_1_percent[1])," of total, ",format_perc(nat_fig$int_top_source_1_number[2]/nat_fig$int_top_source_1_number[1]), " are women")
  source_2_int=paste0("2. ",nat_fig$int_top_source_2_name[1], ": ",format_k(nat_fig$int_top_source_2_number[2]),", ",format_perc(nat_fig$int_top_source_2_percent[2])," of total, ",format_perc(nat_fig$int_top_source_2_number[2]/nat_fig$int_top_source_2_number[1]), " are women")
  source_3_int=paste0("3. ",nat_fig$int_top_source_3_name[1], ": ",format_k(nat_fig$int_top_source_3_number[2]),", ",format_perc(nat_fig$int_top_source_3_percent[2])," of total, ",format_perc(nat_fig$int_top_source_3_number[2]/nat_fig$int_top_source_3_number[1]), " are women")
  

  dest_1_nat=paste0("1. ",nat_fig$nat_top_dest_1_name[1], ": ",format_k(nat_fig$nat_top_dest_1_number[1]),", ",format_perc(nat_fig$nat_top_dest_1_percent[1])," of total, ",format_perc(nat_fig$nat_top_dest_1_number[2]/nat_fig$nat_top_dest_1_number[1]), " are women")
  dest_2_nat=paste0("2. ",nat_fig$nat_top_dest_2_name[1], ": ",format_k(nat_fig$nat_top_dest_2_number[2]),", ",format_perc(nat_fig$nat_top_dest_2_percent[2])," of total, ",format_perc(nat_fig$nat_top_dest_2_number[2]/nat_fig$nat_top_dest_2_number[1]), " are women")
  dest_3_nat=paste0("3. ",nat_fig$nat_top_dest_3_name[1], ": ",format_k(nat_fig$nat_top_dest_3_number[2]),", ",format_perc(nat_fig$nat_top_dest_3_percent[2])," of total, ",format_perc(nat_fig$nat_top_dest_3_number[2]/nat_fig$nat_top_dest_3_number[1]), " are women")
  
  
  source_1_nat=paste0("1. ",nat_fig$nat_top_source_1_name[1], ": ",format_k(nat_fig$nat_top_source_1_number[1]),", ",format_perc(nat_fig$nat_top_source_1_percent[1])," of total, ",format_perc(nat_fig$nat_top_source_1_number[2]/nat_fig$nat_top_source_1_number[1]), " are women")
  source_2_nat=paste0("2. ",nat_fig$nat_top_source_2_name[1], ": ",format_k(nat_fig$nat_top_source_2_number[2]),", ",format_perc(nat_fig$nat_top_source_2_percent[2])," of total, ",format_perc(nat_fig$nat_top_source_2_number[2]/nat_fig$nat_top_source_2_number[1]), " are women")
  source_3_nat=paste0("3. ",nat_fig$nat_top_source_3_name[1], ": ",format_k(nat_fig$nat_top_source_3_number[2]),", ",format_perc(nat_fig$nat_top_source_3_percent[2])," of total, ",format_perc(nat_fig$nat_top_source_3_number[2]/nat_fig$nat_top_source_3_number[1]), " are women")
  
  
  HTML("<h3><b>",COUNTRY_TEXT$COUNTRY_NAME,"</b></h3>",
       "<h4>",
       "<p><b> International emigrants:</b>",int_mig_X,"</p>",
       "<p><b> International immigrants:</b>",int_mig_M,"</p>",
       "<p><b> Internal migrants:</b>",nat_mig,"</p>",
       "<br>",
       "<p><b>Top destinations of international migration:</b></p>", "<p>",dest_1_int,"</p>","<p>",dest_2_int,"</p>","<p>",dest_3_int,"</p>",
       "<br>",
       "<p><b>Top origins of international migration:</b></p>", "<p>",source_1_int,"</p>","<p>",source_2_int,"</p>","<p>",source_3_int,"</p>",
       "<br>",
       "<p><b>Top destinations of internal migration:</b>", "<p>",dest_1_nat,"</p>","<p>",dest_2_nat,"</p>","<p>",dest_3_nat,"</p>",
       "<br>",
       "<p><b>Top origins of internal migration:</b></p>", "<p>",source_1_nat,"</p>","<p>",source_2_nat,"</p>","<p>",source_3_nat,"</p>",
       "</h4>")
}