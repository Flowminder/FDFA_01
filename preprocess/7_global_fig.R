# global figure summaries ####
EM_int=tbl(mig_db,"EM_int")
EM_nat=tbl(mig_db,"EM_nat")
IM_int=tbl(mig_db,"IM_int")
IM_nat=tbl(mig_db,"IM_nat")

admin_names=tbl(mig_db,"admin_names")%>%
  collect(n=Inf)

# EM_int number & % #####
EM_int_mig_data=EM_int%>%
  group_by(sex)%>%
  summarise(move=sum(move))%>%
  collect()

EM_int_mig_all=EM_int_mig_data%>%
  filter(sex=="all")
EM_int_mig_female=EM_int_mig_data%>%
  filter(sex=="F")
EM_int_mig_male=EM_int_mig_data%>%
  filter(sex=="M")

EM_int_mig_female_perc=EM_int_mig_female$move/EM_int_mig_all$move
EM_int_mig_male_perc=EM_int_mig_male$move/EM_int_mig_all$move

# EM_nat number & % #####
EM_nat_mig_data=EM_nat%>%
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

# top destination of international #####
top_destination_all=IM_int%>%
  filter(sex=="all")%>%
  group_by(ISO)%>%
  summarise(move=sum(move))%>%
  collect()%>%
  arrange(desc(move))%>%
  top_n(3,move)%>%
  mutate(total=EM_int_mig_all$move,
         perc=move/total)%>%
  left_join(admin_names%>%
              distinct(ISO,COUNTRY_NAME),
            by="ISO")

top_destination_female=IM_int%>%
  filter(sex=="F")%>%
  group_by(ISO)%>%
  summarise(move=sum(move))%>%
  collect()%>%
  arrange(desc(move))%>%
  top_n(3,move)%>%
  mutate(total=EM_int_mig_female$move,
         perc=move/total)%>%
  left_join(admin_names%>%
              distinct(ISO,COUNTRY_NAME),
            by="ISO")

top_destination_male=IM_int%>%
  filter(sex=="M")%>%
  group_by(ISO)%>%
  summarise(move=sum(move))%>%
  collect()%>%
  arrange(desc(move))%>%
  top_n(3,move)%>%
  mutate(total=EM_int_mig_female$move,
         perc=move/total)%>%
  left_join(admin_names%>%
              distinct(ISO,COUNTRY_NAME),
            by="ISO")

# top source of international ####
top_source_all=EM_int%>%
  filter(sex=="all")%>%
  group_by(ISO)%>%
  summarise(move=sum(move))%>%
  collect()%>%
  arrange(desc(move))%>%
  top_n(3,move)%>%
  mutate(total=EM_int_mig_all$move,
         perc=move/total)%>%
  left_join(admin_names%>%
              distinct(ISO,COUNTRY_NAME),
            by="ISO")

top_source_female=EM_int%>%
  filter(sex=="F")%>%
  group_by(ISO)%>%
  summarise(move=sum(move))%>%
  collect()%>%
  arrange(desc(move))%>%
  top_n(3,move)%>%
  mutate(total=EM_int_mig_female$move,
         perc=move/total)%>%
  left_join(admin_names%>%
              distinct(ISO,COUNTRY_NAME),
            by="ISO")

top_source_male=EM_int%>%
  filter(sex=="M")%>%
  group_by(ISO)%>%
  summarise(move=sum(move))%>%
  collect()%>%
  arrange(desc(move))%>%
  top_n(3,move)%>%
  mutate(total=EM_int_mig_male$move,
         perc=move/total)%>%
  left_join(admin_names%>%
              distinct(ISO,COUNTRY_NAME),
            by="ISO")

# top countries for internal migration ####
top_countries_nat_all=EM_nat%>%
  filter(sex=="all")%>%
  group_by(ISO)%>%
  summarise(move=sum(move))%>%
  collect()%>%
  arrange(desc(move))%>%
  top_n(3,move)%>%
  mutate(total=EM_nat_mig_all$move,
         perc=move/total)%>%
  left_join(admin_names%>%
              distinct(ISO,COUNTRY_NAME),
            by="ISO")

top_countries_nat_female=EM_nat%>%
  filter(sex=="F")%>%
  group_by(ISO)%>%
  summarise(move=sum(move))%>%
  collect()%>%
  arrange(desc(move))%>%
  top_n(3,move)%>%
  mutate(total=EM_nat_mig_female$move,
         perc=move/total)%>%
  left_join(admin_names%>%
              distinct(ISO,COUNTRY_NAME),
            by="ISO")

top_countries_nat_male=EM_nat%>%
  filter(sex=="M")%>%
  group_by(ISO)%>%
  summarise(move=sum(move))%>%
  collect()%>%
  arrange(desc(move))%>%
  top_n(3,move)%>%
  mutate(total=EM_nat_mig_male$move,
         perc=move/total)%>%
  left_join(admin_names%>%
              distinct(ISO,COUNTRY_NAME),
            by="ISO")

# data frame #####
global_fig=data.frame(sex=c("all","F","M"),
                      int_number=c(EM_int_mig_all$move,EM_int_mig_female$move,EM_int_mig_male$move),
                      int_percent=c(1,EM_int_mig_female_perc,EM_int_mig_male_perc),
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
                      
                      top_nat_1_name=c(top_countries_nat_all$COUNTRY_NAME[1],top_countries_nat_female$COUNTRY_NAME[1],top_countries_nat_male$COUNTRY_NAME[1]),
                      top_nat_1_number=c(top_countries_nat_all$move[1],top_countries_nat_female$move[1],top_countries_nat_male$move[1]),
                      top_nat_1_percent=c(top_countries_nat_all$perc[1],top_countries_nat_female$perc[1],top_countries_nat_male$perc[1]),
                      
                      top_nat_2_name=c(top_countries_nat_all$COUNTRY_NAME[2],top_countries_nat_female$COUNTRY_NAME[2],top_countries_nat_male$COUNTRY_NAME[2]),
                      top_nat_2_number=c(top_countries_nat_all$move[2],top_countries_nat_female$move[2],top_countries_nat_male$move[2]),
                      top_nat_2_percent=c(top_countries_nat_all$perc[2],top_countries_nat_female$perc[2],top_countries_nat_male$perc[2]),
                      
                      top_nat_3_name=c(top_countries_nat_all$COUNTRY_NAME[3],top_countries_nat_female$COUNTRY_NAME[3],top_countries_nat_male$COUNTRY_NAME[3]),
                      top_nat_3_number=c(top_countries_nat_all$move[3],top_countries_nat_female$move[3],top_countries_nat_male$move[3]),
                      top_nat_3_percent=c(top_countries_nat_all$perc[3],top_countries_nat_female$perc[3],top_countries_nat_male$perc[3])
)

# copy to DB #####
copy_to(mig_db,
        global_fig,
        name="global_fig",
        temporary = FALSE,
        indexes = list("sex"),
        overwrite = T)