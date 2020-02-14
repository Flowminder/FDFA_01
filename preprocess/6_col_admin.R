library(rgdal)
library(sf)
# set the directories ####
Centroids=readOGR(paste0(dir_data_input,"data/SpatialData/Centroids.shp"))

admin_names=tbl(mig_db,"admin_names")%>%
  collect(n=Inf)

table_gender_mig_nat=read.csv(paste0(dir_data_input,"data/MigrationEstimates/MigrEst_internal_v4.csv"))
table_gender_mig_inter_10=read.csv(paste0(dir_data_input,
                                          "data/MigrationEstimates/MigrEst_international_v7_light.csv")) # flow below 10 people removed to save space

Centroids_df=Centroids@data
Centroids_df$lon=coordinates(Centroids)[,1]
Centroids_df$lat=coordinates(Centroids)[,2]

# international emigration ####
EM_int=table_gender_mig_inter_10%>%
  group_by(ISOI,NODEI,sex)%>%
  summarise(move=sum(pred_seed1,na.rm=T))%>%
  mutate(JOIN_ID=paste0(ISOI,"_",NODEI))%>%
  left_join(Centroids_df,
            by="JOIN_ID")%>%
  left_join(admin_names%>%
              select(JOIN_ID,COUNTRY_NAME,ADMIN_NAME),
            by="JOIN_ID")%>%
  ungroup()%>%
  select(ISO,JOIN_ID,lon,lat,COUNTRY_NAME,ADMIN_NAME,move,sex)

EM_int_all=EM_int%>%
  group_by(JOIN_ID)%>%
  summarise(move=sum(move,na.rm = T))%>%
  mutate(sex="all")%>%
  left_join(Centroids_df,
            by="JOIN_ID")%>%
  left_join(admin_names%>%
              select(JOIN_ID,COUNTRY_NAME,ADMIN_NAME),
            by="JOIN_ID")%>%
  mutate(ISO=substr(JOIN_ID,1,3))%>%
  ungroup()%>%
  select(ISO,JOIN_ID,lon,lat,COUNTRY_NAME,ADMIN_NAME,move,sex)

EM_int_complete=EM_int%>%
  bind_rows(EM_int_all)

# copy to DB 
copy_to(mig_db,
        EM_int_complete,
        name="EM_int",
        temporary = FALSE,
        indexes = list("JOIN_ID","ISO"),
        overwrite = T)

# national emigration ####
EM_nat_M=table_gender_mig_nat%>%
  group_by(ISOI,NODEI)%>%
  summarise(move=sum(MIGIJ_Mest))%>%
  mutate(JOIN_ID=paste0(ISOI,"_",NODEI))%>%
  left_join(Centroids_df,
            by="JOIN_ID")%>%
  left_join(admin_names%>%
              select(JOIN_ID,COUNTRY_NAME,ADMIN_NAME),
            by="JOIN_ID")%>%
  mutate(sex="M")%>%
  ungroup()%>%
  select(ISO,JOIN_ID,lon,lat,COUNTRY_NAME,ADMIN_NAME,move,sex)

EM_nat_F=table_gender_mig_nat%>%
  group_by(ISOI,NODEI)%>%
  summarise(move=sum(MIGIJ_Fest))%>%
  mutate(JOIN_ID=paste0(ISOI,"_",NODEI))%>%
  left_join(Centroids_df,
            by="JOIN_ID")%>%
  left_join(admin_names%>%
              select(JOIN_ID,COUNTRY_NAME,ADMIN_NAME),
            by="JOIN_ID")%>%
  mutate(sex="F")%>%
  ungroup()%>%
  select(ISO,JOIN_ID,lon,lat,COUNTRY_NAME,ADMIN_NAME,move,sex)

EM_nat_all=EM_nat_F%>%
  bind_rows(EM_nat_M)%>%
  group_by(JOIN_ID)%>%
  summarise(move=sum(move,na.rm = T))%>%
  left_join(Centroids_df,
            by="JOIN_ID")%>%
  left_join(admin_names%>%
              select(JOIN_ID,COUNTRY_NAME,ADMIN_NAME),
            by="JOIN_ID")%>%
  mutate(sex="all")%>%
  ungroup()%>%
  select(ISO,JOIN_ID,lon,lat,COUNTRY_NAME,ADMIN_NAME,move,sex)
  
EM_nat_complete=EM_nat_all%>%
  bind_rows(EM_nat_F)%>%
  bind_rows(EM_nat_M)


# copy to DB 
copy_to(mig_db,
        EM_nat_complete,
        name="EM_nat",
        temporary = FALSE,
        indexes = list("JOIN_ID","ISO"),
        overwrite = T)

# international immigration ####
IM_int=table_gender_mig_inter_10%>%
  group_by(ISOJ,NODEJ,sex)%>%
  summarise(move=sum(pred_seed1))%>%
  mutate(JOIN_ID=paste0(ISOJ,"_",NODEJ))%>%
  left_join(Centroids_df,
            by="JOIN_ID")%>%
  left_join(admin_names%>%
              select(JOIN_ID,COUNTRY_NAME,ADMIN_NAME),
            by="JOIN_ID")%>%
  ungroup()%>%
  select(ISO,JOIN_ID,lon,lat,COUNTRY_NAME,ADMIN_NAME,move,sex)

IM_int_all=IM_int%>%
  group_by(JOIN_ID)%>%
  summarise(move=sum(move,na.rm = T))%>%
  mutate(sex="all")%>%
  left_join(Centroids_df,
            by="JOIN_ID")%>%
  left_join(admin_names%>%
              select(JOIN_ID,COUNTRY_NAME,ADMIN_NAME),
            by="JOIN_ID")%>%
  mutate(ISO=substr(JOIN_ID,1,3))%>%
  ungroup()%>%
  select(ISO,JOIN_ID,lon,lat,COUNTRY_NAME,ADMIN_NAME,move,sex)

IM_int_complete=IM_int%>%
  bind_rows(IM_int_all)

# copy to db 
copy_to(mig_db,
        IM_int_complete,
        name="IM_int",
        temporary = FALSE,
        indexes = list("JOIN_ID","ISO"),
        overwrite = T)

# National immigration ####
IM_nat_M=table_gender_mig_nat%>%
  group_by(ISOJ,NODEJ)%>%
  summarise(move=sum(MIGIJ_Mest))%>%
  mutate(JOIN_ID=paste0(ISOJ,"_",NODEJ))%>%
  left_join(Centroids_df,
            by="JOIN_ID")%>%
  left_join(admin_names%>%
              select(JOIN_ID,COUNTRY_NAME,ADMIN_NAME),
            by="JOIN_ID")%>%
  mutate(sex="M")%>%
  ungroup()%>%
  select(ISO,JOIN_ID,lon,lat,COUNTRY_NAME,ADMIN_NAME,move,sex)

IM_nat_F=table_gender_mig_nat%>%
  group_by(ISOJ,NODEJ)%>%
  summarise(move=sum(MIGIJ_Fest))%>%
  mutate(JOIN_ID=paste0(ISOJ,"_",NODEJ))%>%
  left_join(Centroids_df,
            by="JOIN_ID")%>%
  left_join(admin_names%>%
              select(JOIN_ID,COUNTRY_NAME,ADMIN_NAME),
            by="JOIN_ID")%>%
  mutate(sex="F")%>%
  ungroup()%>%
  select(ISO,JOIN_ID,lon,lat,COUNTRY_NAME,ADMIN_NAME,move,sex)


IM_nat_all=IM_nat_F%>%
  bind_rows(IM_nat_M)%>%
  group_by(JOIN_ID)%>%
  summarise(move=sum(move,na.rm = T))%>%
  left_join(Centroids_df,
            by="JOIN_ID")%>%
  left_join(admin_names%>%
              select(JOIN_ID,COUNTRY_NAME,ADMIN_NAME),
            by="JOIN_ID")%>%
  mutate(sex="all")%>%
  ungroup()%>%
  select(ISO,JOIN_ID,lon,lat,COUNTRY_NAME,ADMIN_NAME,move,sex)

IM_nat_complete=IM_nat_all%>%
  bind_rows(IM_nat_F)%>%
  bind_rows(IM_nat_M)

# copy to db 
copy_to(mig_db,
        IM_nat_complete,
        name="IM_nat",
        temporary = FALSE,
        indexes = list("JOIN_ID","ISO"),
        overwrite = T)

