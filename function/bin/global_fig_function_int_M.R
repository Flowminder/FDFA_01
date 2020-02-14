global_fig_function_int_M=function(){
  global_fig=tbl(mig_db,"global_fig")%>%
    collect(n=Inf)
  
  format_mio=function(x){
    paste(format(round(x / 1e6, 1), trim = TRUE), "M")
  }
  format_perc=function(x){
    paste0(format(round(x*100,0), trim = TRUE), "%")
  }
  
  int_mig=paste0(format_mio(global_fig$int_number[1])," (", format_perc(global_fig$int_percent[2])," female)")
  nat_mig=paste0(format_mio(global_fig$nat_number[1])," (", format_perc(global_fig$nat_percent[2])," female)")
  
  dest_1_int=paste0("1. ",global_fig$int_top_dest_1_name[1], ": ",format_mio(global_fig$int_top_dest_1_number[1]),", ",format_perc(global_fig$int_top_dest_1_percent[1])," of total, ",format_perc(global_fig$int_top_dest_1_number[2]/global_fig$int_top_dest_1_number[1]), " are women")
  dest_2_int=paste0("2. ",global_fig$int_top_dest_2_name[1], ": ",format_mio(global_fig$int_top_dest_2_number[2]),", ",format_perc(global_fig$int_top_dest_2_percent[2])," of total, ",format_perc(global_fig$int_top_dest_2_number[2]/global_fig$int_top_dest_2_number[1]), " are women")
  dest_3_int=paste0("3. ",global_fig$int_top_dest_3_name[1], ": ",format_mio(global_fig$int_top_dest_3_number[2]),", ",format_perc(global_fig$int_top_dest_3_percent[2])," of total, ",format_perc(global_fig$int_top_dest_3_number[2]/global_fig$int_top_dest_3_number[1]), " are women")
  
  source_1_int=paste0("1. ",global_fig$int_top_source_1_name[1], ": ",format_mio(global_fig$int_top_source_1_number[1]),", ",format_perc(global_fig$int_top_source_1_percent[1])," of total, ",format_perc(global_fig$int_top_source_1_number[2]/global_fig$int_top_source_1_number[1]), " are women")
  source_2_int=paste0("2. ",global_fig$int_top_source_2_name[1], ": ",format_mio(global_fig$int_top_source_2_number[2]),", ",format_perc(global_fig$int_top_source_2_percent[2])," of total, ",format_perc(global_fig$int_top_source_2_number[2]/global_fig$int_top_source_2_number[1]), " are women")
  source_3_int=paste0("3. ",global_fig$int_top_source_3_name[1], ": ",format_mio(global_fig$int_top_source_3_number[2]),", ",format_perc(global_fig$int_top_source_3_percent[2])," of total, ",format_perc(global_fig$int_top_source_3_number[2]/global_fig$int_top_source_3_number[1]), " are women")
  
  top_1_nat=paste0("1. ",global_fig$top_nat_1_name[1], ": ",format_mio(global_fig$top_nat_1_number[1]),", ",format_perc(global_fig$top_nat_1_percent[1])," of total, ",format_perc(global_fig$top_nat_1_number[2]/global_fig$top_nat_1_number[1]), " are women")
  top_2_nat=paste0("2. ",global_fig$top_nat_2_name[1], ": ",format_mio(global_fig$top_nat_2_number[2]),", ",format_perc(global_fig$top_nat_2_percent[2])," of total, ",format_perc(global_fig$top_nat_2_number[2]/global_fig$top_nat_2_number[1]), " are women")
  top_3_nat=paste0("3. ",global_fig$top_nat_3_name[1], ": ",format_mio(global_fig$top_nat_3_number[2]),", ",format_perc(global_fig$top_nat_3_percent[2])," of total, ",format_perc(global_fig$top_nat_3_number[2]/global_fig$top_nat_3_number[1]), " are women")
  
  
  HTML("<h4>",
       "<p><b>Top destinations of international migration</b></p>", "<p>",dest_1_int,"</p>","<p>",dest_2_int,"</p>","<p>",dest_3_int,"</p>",
       "</h4>")
}