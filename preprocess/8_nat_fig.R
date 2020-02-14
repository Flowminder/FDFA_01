# national figure summaries ####
EM_int=tbl(mig_db,"EM_int")
EM_nat=tbl(mig_db,"EM_nat")
IM_int=tbl(mig_db,"IM_int")
IM_nat=tbl(mig_db,"IM_nat")

admin_names=tbl(mig_db,"admin_names")%>%
  collect(n=Inf)

COUNTRIES=EM_int%>%
  distinct(ISO)%>%
  collect(n=Inf)
COUNTRIES=COUNTRIES$ISO

df=data.frame()
country_clicked="AFG"
for(country_clicked in COUNTRIES){
  
  # EM_int number & % #####
  EM_int_mig_data=EM_int%>%
    filter(ISO==country_clicked)%>%
    group_by(sex)%>%
    summarise(move=sum(move))%>%
    collect()
  
  EM_int_mig_all=EM_int_mig_data%>%
    filter(sex=="all")
  EM_int_mig_female=EM_int_mig_data%>%
    filter(sex=="F")
  
  if(dim(EM_int_mig_female)[1]==0){
    EM_int_mig_female=data.frame(sex="F",
                                 move=NA)
  }
  
  EM_int_mig_male=EM_int_mig_data%>%
    filter(sex=="M")
  
  EM_int_mig_female_perc=EM_int_mig_female$move/EM_int_mig_all$move
  EM_int_mig_male_perc=EM_int_mig_male$move/EM_int_mig_all$move
  
  # EM_nat number & % #####
  EM_nat_mig_data=EM_nat%>%
    filter(ISO==country_clicked)%>%
    group_by(sex)%>%
    summarise(move=sum(move))%>%
    collect()
  
  EM_nat_mig_all=EM_nat_mig_data%>%
    filter(sex=="all")
  EM_nat_mig_female=EM_nat_mig_data%>%
    filter(sex=="F")
  EM_nat_mig_male=EM_nat_mig_data%>%
    filter(sex=="M")
  
  EM_nat_mig_female_perc=EM_nat_mig_female$move/EM_nat_mig_all$move
  EM_nat_mig_male_perc=EM_nat_mig_male$move/EM_nat_mig_all$move
  
  
  # IM_int number & % #####
  IM_int_mig_data=IM_int%>%
    filter(ISO==country_clicked)%>%
    group_by(sex)%>%
    summarise(move=sum(move))%>%
    collect()
  
  IM_int_mig_all=IM_int_mig_data%>%
    filter(sex=="all")
  IM_int_mig_female=IM_int_mig_data%>%
    filter(sex=="F")
  IM_int_mig_male=IM_int_mig_data%>%
    filter(sex=="M")
  
  IM_int_mig_female_perc=IM_int_mig_female$move/IM_int_mig_all$move
  IM_int_mig_male_perc=IM_int_mig_male$move/IM_int_mig_all$move
  
  # IM_nat number & % #####
  IM_nat_mig_data=IM_nat%>%
    filter(ISO==country_clicked)%>%
    group_by(sex)%>%
    summarise(move=sum(move))%>%
    collect()
  
  IM_nat_mig_all=IM_nat_mig_data%>%
    filter(sex=="all")
  IM_nat_mig_female=IM_nat_mig_data%>%
    filter(sex=="F")
  IM_nat_mig_male=IM_nat_mig_data%>%
    filter(sex=="M")
  
  IM_nat_mig_female_perc=IM_nat_mig_female$move/IM_nat_mig_all$move
  IM_nat_mig_male_perc=IM_nat_mig_male$move/IM_nat_mig_all$move
  
  # top destination countries of international emigration #####
  data_dest_all=tbl(mig_db,"GLOBAL_mig_int_X_50_all_country")
  
  top_destination_all=data_dest_all%>%
    filter(ISOI==country_clicked)%>%
    collect()%>%
    group_by(ISOJ)%>%
    summarise(move=sum(pred_seed1))%>%
    arrange(desc(move))%>%
    top_n(3,move)%>%
    mutate(total=EM_int_mig_all$move,
           perc=move/total)%>%
    left_join(admin_names%>%
                distinct(ISO,COUNTRY_NAME),
              by=c("ISOJ"="ISO"))
  
  data_dest_female=tbl(mig_db,"GLOBAL_mig_int_X_50_F_country")
  top_destination_female=data_dest_female%>%
    filter(ISOI==country_clicked)%>%
    collect()%>%
    group_by(ISOJ)%>%
    summarise(move=sum(pred_seed1))%>%
    arrange(desc(move))%>%
    top_n(3,move)%>%
    mutate(total=EM_int_mig_female$move,
           perc=move/total)%>%
    left_join(admin_names%>%
                distinct(ISO,COUNTRY_NAME),
              by=c("ISOJ"="ISO"))
  
  data_dest_male=tbl(mig_db,"GLOBAL_mig_int_X_50_M_country")
  top_destination_male=data_dest_male%>%
    filter(ISOI==country_clicked)%>%
    collect()%>%
    group_by(ISOJ)%>%
    summarise(move=sum(pred_seed1))%>%
    arrange(desc(move))%>%
    top_n(3,move)%>%
    mutate(total=EM_int_mig_male$move,
           perc=move/total)%>%
    left_join(admin_names%>%
                distinct(ISO,COUNTRY_NAME),
              by=c("ISOJ"="ISO"))
  
  # top source countries of international immigration ####
  data_source_all=tbl(mig_db,"GLOBAL_mig_int_M_50_all_country")
  
  top_source_all=data_source_all%>%
    filter(ISOJ==country_clicked)%>%
    collect()%>%
    group_by(ISOI)%>%
    summarise(move=sum(pred_seed1))%>%
    arrange(desc(move))%>%
    top_n(3,move)%>%
    mutate(total=IM_int_mig_all$move,
           perc=move/total)%>%
    left_join(admin_names%>%
                distinct(ISO,COUNTRY_NAME),
              by=c("ISOI"="ISO"))
  
  
  data_source_female=tbl(mig_db,"GLOBAL_mig_int_M_50_F_country")
  
  top_source_female=data_source_female%>%
    filter(ISOJ==country_clicked)%>%
    collect()%>%
    group_by(ISOI)%>%
    summarise(move=sum(pred_seed1))%>%
    arrange(desc(move))%>%
    top_n(3,move)%>%
    mutate(total=IM_int_mig_female$move,
           perc=move/total)%>%
    left_join(admin_names%>%
                distinct(ISO,COUNTRY_NAME),
              by=c("ISOI"="ISO"))
  
  data_source_male=tbl(mig_db,"GLOBAL_mig_int_M_50_M_country")
  
  top_source_male=data_source_male%>%
    filter(ISOJ==country_clicked)%>%
    collect()%>%
    group_by(ISOI)%>%
    summarise(move=sum(pred_seed1))%>%
    arrange(desc(move))%>%
    top_n(3,move)%>%
    mutate(total=IM_int_mig_male$move,
           perc=move/total)%>%
    left_join(admin_names%>%
                distinct(ISO,COUNTRY_NAME),
              by=c("ISOI"="ISO"))  
  
  # top destination admin of internal emigration #####
  data_dest_all=tbl(mig_db,"GLOBAL_mig_nat_X_50_all_admin")
  
  top_destination_all_nat=data_dest_all%>%
    filter(ISOI==country_clicked)%>%
    collect()%>%
    group_by(JOIN_ID_J)%>%
    summarise(move=sum(sex_nat))%>%
    arrange(desc(move))%>%
    top_n(3,move)%>%
    mutate(total=EM_nat_mig_all$move,
           perc=move/total)%>%
    left_join(admin_names%>%
                filter(ISO==country_clicked)%>%
                distinct(JOIN_ID,COUNTRY_NAME,ADMIN_NAME),
              by=c("JOIN_ID_J"="JOIN_ID"))
  
  
  data_dest_female=tbl(mig_db,"GLOBAL_mig_nat_X_50_F_admin")
  
  top_destination_female_nat=data_dest_female%>%
    filter(ISOI==country_clicked)%>%
    collect()%>%
    group_by(JOIN_ID_J)%>%
    summarise(move=sum(sex_nat))%>%
    arrange(desc(move))%>%
    top_n(3,move)%>%
    mutate(total=EM_nat_mig_female$move,
           perc=move/total)%>%
    left_join(admin_names%>%
                filter(ISO==country_clicked)%>%
                distinct(JOIN_ID,COUNTRY_NAME,ADMIN_NAME),
              by=c("JOIN_ID_J"="JOIN_ID"))
  
  
  data_dest_male=tbl(mig_db,"GLOBAL_mig_nat_X_50_M_admin")
  
  top_destination_male_nat=data_dest_male%>%
    filter(ISOI==country_clicked)%>%
    collect()%>%
    group_by(JOIN_ID_J)%>%
    summarise(move=sum(sex_nat))%>%
    arrange(desc(move))%>%
    top_n(3,move)%>%
    mutate(total=EM_nat_mig_male$move,
           perc=move/total)%>%
    left_join(admin_names%>%
                filter(ISO==country_clicked)%>%
                distinct(JOIN_ID,COUNTRY_NAME,ADMIN_NAME),
              by=c("JOIN_ID_J"="JOIN_ID"))
  
  
  # top source admin of internal immigration #####
  data_source_all=tbl(mig_db,"GLOBAL_mig_nat_M_50_all_admin")
  
  top_source_all_nat=data_source_all%>%
    filter(ISOI==country_clicked)%>%
    collect()%>%
    group_by(JOIN_ID_I)%>%
    summarise(move=sum(sex_nat))%>%
    arrange(desc(move))%>%
    top_n(3,move)%>%
    mutate(total=EM_nat_mig_all$move,
           perc=move/total)%>%
    left_join(admin_names%>%
                filter(ISO==country_clicked)%>%
                distinct(JOIN_ID,COUNTRY_NAME,ADMIN_NAME),
              by=c("JOIN_ID_I"="JOIN_ID"))
  
  
  data_source_female=tbl(mig_db,"GLOBAL_mig_nat_M_50_F_admin")
  
  top_source_female_nat=data_source_female%>%
    filter(ISOI==country_clicked)%>%
    collect()%>%
    group_by(JOIN_ID_I)%>%
    summarise(move=sum(sex_nat))%>%
    arrange(desc(move))%>%
    top_n(3,move)%>%
    mutate(total=EM_nat_mig_female$move,
           perc=move/total)%>%
    left_join(admin_names%>%
                filter(ISO==country_clicked)%>%
                distinct(JOIN_ID,COUNTRY_NAME,ADMIN_NAME),
              by=c("JOIN_ID_I"="JOIN_ID"))
  
  
  data_source_male=tbl(mig_db,"GLOBAL_mig_nat_M_50_M_admin")
  
  top_source_male_nat=data_source_male%>%
    filter(ISOI==country_clicked)%>%
    collect()%>%
    group_by(JOIN_ID_I)%>%
    summarise(move=sum(sex_nat))%>%
    arrange(desc(move))%>%
    top_n(3,move)%>%
    mutate(total=EM_nat_mig_male$move,
           perc=move/total)%>%
    left_join(admin_names%>%
                filter(ISO==country_clicked)%>%
                distinct(JOIN_ID,COUNTRY_NAME,ADMIN_NAME),
              by=c("JOIN_ID_I"="JOIN_ID"))
  
  # correct missing variables #####
  variables=c("EM_int_mig_all",
              "EM_int_mig_female",
              "EM_int_mig_male",
              "EM_int_mig_female_perc",
              "EM_int_mig_male_perc",
              "IM_int_mig_all",
              "IM_int_mig_female",
              "IM_int_mig_male",
              "IM_int_mig_female_perc",
              "IM_int_mig_male_perc",
              "EM_nat_mig_all",
              "EM_nat_mig_female_perc",
              "EM_nat_mig_male_perc",
              "top_destination_all","top_destination_female","top_destination_male",
              "top_source_all","top_source_female","top_source_male",
              "top_destination_all_nat","top_destination_female_nat","top_destination_male_nat",
              "top_source_all_nat","top_source_female_nat","top_source_male_nat")
  for(VAR in variables){
    var_test=get(VAR)
    var_test=as.data.frame(var_test)
    if(nrow(var_test)==0){
      NAMES=names(var_test)
      var_test=data.frame(data.frame(t(array(NA,ncol(var_test)))))
      names(var_test)=NAMES
      assign(VAR,
             var_test)
    }
  }
  # data frame #####
  nat_fig=data.frame(ISO=c(country_clicked,country_clicked,country_clicked),
                     sex=c("all","F","M"),
                     int_number_X=c(EM_int_mig_all$move,EM_int_mig_female$move,EM_int_mig_male$move),
                     int_percent_X=c(1,EM_int_mig_female_perc,EM_int_mig_male_perc),
                     int_number_M=c(IM_int_mig_all$move,IM_int_mig_female$move,IM_int_mig_male$move),
                     int_percent_M=unlist(c(1,IM_int_mig_female_perc,IM_int_mig_male_perc)),
                     nat_number=c(EM_nat_mig_all$move,EM_nat_mig_female$move,EM_nat_mig_male$move),
                     nat_percent=c(1,EM_nat_mig_female_perc,EM_nat_mig_male_perc),
                     
                     int_top_dest_1_name=c(top_destination_all$COUNTRY_NAME[1],top_destination_female$COUNTRY_NAME[1],top_destination_male$COUNTRY_NAME[1]),
                     int_top_dest_1_number=c(top_destination_all$move[1],top_destination_female$move[1],top_destination_male$move[1]),
                     int_top_dest_1_percent=c(top_destination_all$perc[1],top_destination_female$perc[1],top_destination_male$perc[1]),
                     
                     int_top_dest_2_name=c(top_destination_all$COUNTRY_NAME[2],top_destination_female$COUNTRY_NAME[2],top_destination_male$COUNTRY_NAME[2]),
                     int_top_dest_2_number=c(top_destination_all$move[2],top_destination_female$move[2],top_destination_male$move[2]),
                     int_top_dest_2_percent=c(top_destination_all$perc[2],top_destination_female$perc[2],top_destination_male$perc[2]),
                     
                     int_top_dest_3_name=c(top_destination_all$COUNTRY_NAME[3],top_destination_female$COUNTRY_NAME[3],top_destination_male$COUNTRY_NAME[3]),
                     int_top_dest_3_number=c(top_destination_all$move[3],top_destination_female$move[3],top_destination_male$move[3]),
                     int_top_dest_3_percent=c(top_destination_all$perc[3],top_destination_female$perc[3],top_destination_male$perc[3]),
                     
                     int_top_source_1_name=c(top_source_all$COUNTRY_NAME[1],top_source_female$COUNTRY_NAME[1],top_source_male$COUNTRY_NAME[1]),
                     int_top_source_1_number=c(top_source_all$move[1],top_source_female$move[1],top_source_male$move[1]),
                     int_top_source_1_percent=c(top_source_all$perc[1],top_source_female$perc[1],top_source_male$perc[1]),
                     
                     int_top_source_2_name=c(top_source_all$COUNTRY_NAME[2],top_source_female$COUNTRY_NAME[2],top_source_male$COUNTRY_NAME[2]),
                     int_top_source_2_number=c(top_source_all$move[2],top_source_female$move[2],top_source_male$move[2]),
                     int_top_source_2_percent=c(top_source_all$perc[2],top_source_female$perc[2],top_source_male$perc[2]),
                     
                     int_top_source_3_name=c(top_source_all$COUNTRY_NAME[3],top_source_female$COUNTRY_NAME[3],top_source_male$COUNTRY_NAME[3]),
                     int_top_source_3_number=c(top_source_all$move[3],top_source_female$move[3],top_source_male$move[3]),
                     int_top_source_3_percent=c(top_source_all$perc[3],top_source_female$perc[3],top_source_male$perc[3]),
                     
                     nat_top_dest_1_name=c(top_destination_all_nat$ADMIN_NAME[1],top_destination_female_nat$ADMIN_NAME[1],top_destination_male_nat$ADMIN_NAME[1]),
                     nat_top_dest_1_number=c(top_destination_all_nat$move[1],top_destination_female_nat$move[1],top_destination_male_nat$move[1]),
                     nat_top_dest_1_percent=c(top_destination_all_nat$perc[1],top_destination_female_nat$perc[1],top_destination_male_nat$perc[1]),
                     
                     nat_top_dest_2_name=c(top_destination_all_nat$ADMIN_NAME[2],top_destination_female_nat$ADMIN_NAME[2],top_destination_male_nat$ADMIN_NAME[2]),
                     nat_top_dest_2_number=c(top_destination_all_nat$move[2],top_destination_female_nat$move[2],top_destination_male_nat$move[2]),
                     nat_top_dest_2_percent=c(top_destination_all_nat$perc[2],top_destination_female_nat$perc[2],top_destination_male_nat$perc[2]),
                     
                     nat_top_dest_3_name=c(top_destination_all_nat$ADMIN_NAME[3],top_destination_female_nat$ADMIN_NAME[3],top_destination_male_nat$ADMIN_NAME[3]),
                     nat_top_dest_3_number=c(top_destination_all_nat$move[3],top_destination_female_nat$move[3],top_destination_male_nat$move[3]),
                     nat_top_dest_3_percent=c(top_destination_all_nat$perc[3],top_destination_female_nat$perc[3],top_destination_male_nat$perc[3]),
                     
                     nat_top_source_1_name=c(top_source_all_nat$ADMIN_NAME[1],top_source_female_nat$ADMIN_NAME[1],top_source_male_nat$ADMIN_NAME[1]),
                     nat_top_source_1_number=c(top_source_all_nat$move[1],top_source_female_nat$move[1],top_source_male_nat$move[1]),
                     nat_top_source_1_percent=c(top_source_all_nat$perc[1],top_source_female_nat$perc[1],top_source_male_nat$perc[1]),
                     
                     nat_top_source_2_name=c(top_source_all_nat$ADMIN_NAME[2],top_source_female_nat$ADMIN_NAME[2],top_source_male_nat$ADMIN_NAME[2]),
                     nat_top_source_2_number=c(top_source_all_nat$move[2],top_source_female_nat$move[2],top_source_male_nat$move[2]),
                     nat_top_source_2_percent=c(top_source_all_nat$perc[2],top_source_female_nat$perc[2],top_source_male_nat$perc[2]),
                     
                     nat_top_source_3_name=c(top_source_all_nat$ADMIN_NAME[3],top_source_female_nat$ADMIN_NAME[3],top_source_male_nat$ADMIN_NAME[3]),
                     nat_top_source_3_number=c(top_source_all_nat$move[3],top_source_female_nat$move[3],top_source_male_nat$move[3]),
                     nat_top_source_3_percent=c(top_source_all_nat$perc[3],top_source_female_nat$perc[3],top_source_male_nat$perc[3])
  )
  if(country_clicked=="AFG"){
    clean_names=names(nat_fig)  
  }
  
  df=df%>%
    bind_rows(nat_fig%>%
                select(clean_names))
  print(country_clicked)
  
}


# copy to DB #####
copy_to(mig_db,
        df,
        name="nat_fig",
        temporary = FALSE,
        indexes = list("ISO","sex"),
        overwrite = T)
