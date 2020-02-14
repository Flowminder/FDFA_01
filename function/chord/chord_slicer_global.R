chord_slicer_global=function(mig_db,direction,sex,n_slice,slice_chord){
  
  aggregation_f="country"
  type="int"

  # select and collect data 
  data_chord=switch(type,
                    "int"=tbl(mig_db,paste0(c("GLOBAL_mig",type,direction,"50",sex,aggregation_f),collapse="_")),
                    "nat"=tbl(mig_db,paste0(c("GLOBAL_mig",type,direction,"50",sex,"admin"),collapse="_")))
  
  data_chord=data_chord%>%
    collect(n=Inf)
  
  # relabel data
  group_by_var_origin=switch(direction, # ID the top move origin: emigration=I, immigration=J
                             "X"="ISOI",
                             "M"="ISOJ")
  group_by_var_dest=switch(direction, # ID the top move destination: emigration=I, immigration=J
                           "X"="ISOJ",
                           "M"="ISOI")
  
  move_name=switch(type,
                   "int"="pred_seed1",
                   "nat"="sex_nat")

  # identify origins of top move
  top_countries=data_chord%>% # top move
    rename("move"=move_name,
           "group_by_var_origin"=group_by_var_origin)%>%
    group_by(group_by_var_origin)%>%
    summarise(move=sum(move,na.rm = T))%>%
    arrange(desc(move))%>%
    top_n(slice_chord)
  
  # get the movements between these top origins and label non-top origins as "Other"
  chord_top_move=data_chord%>%
    rename("move"=move_name,
           "group_by_var_origin"=group_by_var_origin,
           "group_by_var_dest"=group_by_var_dest)%>%
    filter(group_by_var_origin%in%top_countries$group_by_var_origin)%>% # use only top sources of emigration/immigration
    group_by(group_by_var_origin,group_by_var_dest)%>% # group by source, then destintation...  
    summarise(move=sum(move,na.rm = T))%>% # ...and get total move
    filter(move>0)%>%
    ungroup()%>%
    mutate(is_top_origin=ifelse(group_by_var_origin%in%top_countries$group_by_var_origin,1,0), # Focus on top sources by identifying top source...
           top_origin=ifelse(is_top_origin==1,as.character(group_by_var_origin),"Other"), # and relabelling non-top source as "Other".
           is_top_dest_and_top_origin=ifelse(group_by_var_dest%in%top_countries$group_by_var_origin,1,0), # Focus on destination which are top source...
           top_dest=ifelse(is_top_dest_and_top_origin==1,as.character(group_by_var_dest),"Other"))%>% #  and relabel destination whuich are not top source as "Other".
    group_by(top_origin,top_dest)%>% # group by top source and destination
    summarise(move=sum(move,na.rm = T)) # get the sum
  
  # admin names: MAKE AMANEABLE TO JOIN_ID
  units_selected=c(top_countries$group_by_var_origin)
  
  admin_names_tbl=tbl(mig_db,"admin_names")

  admin_names_collected=admin_names_tbl%>%
      filter(ISO%in%units_selected)%>%
      distinct(ISO,COUNTRY_NAME)%>%
      collect(n=Inf)
  
  # JOINID WITH SWITCH TO AGGREGATION ###
  JOIN="ISO"
  NAME="COUNTRY_NAME"
  
  chord_top_move=chord_top_move%>%
    left_join(admin_names_collected%>%
                rename("COUNTRY_NAME_origin"=NAME),
              by=c("top_origin"=JOIN))%>%
    left_join(admin_names_collected%>%
                rename("COUNTRY_NAME_dest"=NAME),
              by=c("top_dest"=JOIN))%>%
    mutate(COUNTRY_NAME_dest=ifelse(is.na(COUNTRY_NAME_dest),"Other",COUNTRY_NAME_dest))%>%
    ungroup()%>%
    select(COUNTRY_NAME_origin,COUNTRY_NAME_dest,move)
  
  # adjagency matrix
  chord_data_plot=as.matrix(as_adjacency_matrix(as_tbl_graph(chord_top_move),
                                                attr = "move"))
  
  return(chord_data_plot)
}
