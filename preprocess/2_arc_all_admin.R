library(dplyr)
library(rgdal)
library(purrr)
library(tidyverse)
# set the directories ####

table_gender_mig_inter_10=read.csv(paste0(dir_data_input,
                                          "data/MigrationEstimates/MigrEst_international_v7_light.csv")) # flow below 10 people removed to save space

table_gender_mig_nat=read.csv(paste0(dir_data_input,"data/MigrationEstimates/MigrEst_internal_v4.csv"))

Centroids=readOGR(paste0(dir_data_input,"data/SpatialData/Centroids.shp"))
Centroids_df=Centroids@data
Centroids_df$lon=coordinates(Centroids)[,1]
Centroids_df$lat=coordinates(Centroids)[,2]

# countries_name=read.csv("data/others/ISO.csv",header=T) #source: https://unstats.un.org/unsd/methodology/m49/
# sudan=data.frame("ISO"="SUD",
#                  "country_name"="Sudan")
# countries_name=countries_name%>%
#   bind_rows(sudan)
# 
admin_names=tbl(mig_db,"admin_names")%>%
  collect(n=Inf)


# EMIGRATION ####
Sex_nat=c("MIGIJ_Fest","MIGIJ_Mest")
Slice_n=c(1,3,10,20,50)

slice_n=1

for(slice_n in Slice_n){
  
  # International data ####
  GLOBAL_mig_int=table_gender_mig_inter_10%>%
    mutate(JOIN_ID_I=paste0(ISOI,"_",NODEI),
           JOIN_ID_J=paste0(ISOJ,"_",NODEJ))%>%
    group_by(JOIN_ID_I,JOIN_ID_J)%>%
    summarise(pred_seed1=sum(pred_seed1))%>%
    ungroup()%>%
    group_by(JOIN_ID_I)%>%
    top_n(slice_n,pred_seed1)%>%
    ungroup()%>%
    left_join(Centroids_df%>% # coordinates of I
                select(JOIN_ID,lon,lat)%>%
                rename("x_from"="lon",
                       "y_from"="lat"),
              by=c("JOIN_ID_I"="JOIN_ID"))%>% 
    left_join(admin_names%>% # name of admin I
                select(JOIN_ID,ADMIN_NAME,COUNTRY_NAME)%>%
                rename("ADMIN_NAME_I"="ADMIN_NAME",
                       "COUNTRY_NAME_I"="COUNTRY_NAME"),
              by=c("JOIN_ID_I"="JOIN_ID"))%>%
    left_join(Centroids_df%>% # coordinates of J
                select(JOIN_ID,lon,lat)%>%
                rename("x_to"="lon",
                       "y_to"="lat"),
              by=c("JOIN_ID_J"="JOIN_ID"))%>% 
    left_join(admin_names%>% # name of admin J
                select(JOIN_ID,ADMIN_NAME,COUNTRY_NAME)%>%
                rename("ADMIN_NAME_J"="ADMIN_NAME",
                       "COUNTRY_NAME_J"="COUNTRY_NAME"),
              by=c("JOIN_ID_J"="JOIN_ID"))%>%
    select(JOIN_ID_I,JOIN_ID_J,x_from,y_from, x_to,y_to,pred_seed1,COUNTRY_NAME_I,ADMIN_NAME_I,COUNTRY_NAME_J,ADMIN_NAME_J)%>%
    mutate(ISOI=substr(JOIN_ID_I,1,3),
           ISOJ=substr(JOIN_ID_J,1,3))
  
  # Tilt for AB and BA relationships ####
  GLOBAL_mig_int=GLOBAL_mig_int%>%
    arrange(desc(pred_seed1))%>%
    mutate(S = map2_chr(JOIN_ID_I, JOIN_ID_J, ~str_flatten(sort(c(.x,.y))) ) )%>% #identify pairs (e.g. AB=BA)
    ungroup()%>%
    group_by(S)%>%
    mutate(n_count=1,
           tilt=(cumsum(n_count)-1)*45)%>% # tilt the pairs to avoid overlap
    select(-c(n_count))
  
  # copy to DB  ####
  copy_to(mig_db,
          GLOBAL_mig_int,
          name=paste0(c("GLOBAL_mig_int_X",slice_n,"all_admin"),collapse = "_"),
          temporary = FALSE,
          indexes = list("JOIN_ID_I","JOIN_ID_J","ISOI","ISOJ"),
          overwrite = T)

  # national data ####
  GLOBAL_mig_nat=table_gender_mig_nat%>%
    # rename("sex_nat"=sex_nat)%>%
    mutate(JOIN_ID_I=paste0(ISOI,"_",NODEI),
           JOIN_ID_J=paste0(ISOI,"_",NODEJ),
           sex_nat=MIGIJ_Fest+MIGIJ_Mest)%>%
    group_by(JOIN_ID_I)%>%
    top_n(slice_n,sex_nat)%>%
    ungroup()%>%
    left_join(Centroids_df%>% # coordinates of I
                select(JOIN_ID,lon,lat)%>%
                rename("x_from"="lon",
                       "y_from"="lat"),
              by=c("JOIN_ID_I"="JOIN_ID"))%>% 
    left_join(admin_names%>% # name of admin I
                select(JOIN_ID,ADMIN_NAME,COUNTRY_NAME)%>%
                rename("ADMIN_NAME_I"="ADMIN_NAME",
                       "COUNTRY_NAME_I"="COUNTRY_NAME"),
              by=c("JOIN_ID_I"="JOIN_ID"))%>%
    left_join(Centroids_df%>% # coordinates of J
                select(JOIN_ID,lon,lat)%>%
                rename("x_to"="lon",
                       "y_to"="lat"),
              by=c("JOIN_ID_J"="JOIN_ID"))%>% 
    left_join(admin_names%>% # name of admin J
                select(JOIN_ID,ADMIN_NAME,COUNTRY_NAME)%>%
                rename("ADMIN_NAME_J"="ADMIN_NAME",
                       "COUNTRY_NAME_J"="COUNTRY_NAME"),
              by=c("JOIN_ID_J"="JOIN_ID"))%>%
    select(JOIN_ID_I,JOIN_ID_J,x_from,y_from, x_to,y_to,sex_nat,COUNTRY_NAME_I,ADMIN_NAME_I,COUNTRY_NAME_J,ADMIN_NAME_J)%>%
    mutate(ISOI=substr(JOIN_ID_I,1,3),
           ISOJ=substr(JOIN_ID_J,1,3))
  
  
  # Tilt for AB and BA relationships ####
  GLOBAL_mig_nat=GLOBAL_mig_nat%>%
    group_by(JOIN_ID_I)%>%
    mutate( S = map2_chr(JOIN_ID_I, JOIN_ID_J, ~str_flatten(sort(c(.x,.y))) ) )%>% #identify pairs (e.g. AB=BA)
    ungroup()%>%
    group_by(S)%>%
    mutate(n_count=1,
           tilt=(cumsum(n_count)-1)*45)%>% # tilt the pairs to avoid overlap ####
  select(-c(n_count))
  
  # copy to DB ####
  copy_to(mig_db,
          GLOBAL_mig_nat,
          name=paste0(c("GLOBAL_mig_nat_X",slice_n,"all_admin"),collapse = "_"),
          temporary = FALSE,
          indexes = list("JOIN_ID_I","JOIN_ID_J","ISOI","ISOJ"),
          overwrite = T)
  
  # print ###
  print(paste(slice_n))
  
}

# IMMIGRATION ####
for(slice_n in Slice_n){
  # International data ####
  GLOBAL_mig_int=table_gender_mig_inter_10%>%
    mutate(JOIN_ID_I=paste0(ISOI,"_",NODEI),
           JOIN_ID_J=paste0(ISOJ,"_",NODEJ))%>%
    group_by(JOIN_ID_J,JOIN_ID_I)%>%
    summarise(pred_seed1=sum(pred_seed1))%>%
    ungroup()%>%
    group_by(JOIN_ID_J)%>%
    top_n(slice_n,pred_seed1)%>%
    ungroup()%>%
    left_join(Centroids_df%>% # coordinates of I
                select(JOIN_ID,lon,lat)%>%
                rename("x_from"="lon",
                       "y_from"="lat"),
              by=c("JOIN_ID_I"="JOIN_ID"))%>% 
    left_join(admin_names%>% # name of admin I
                select(JOIN_ID,ADMIN_NAME,COUNTRY_NAME)%>%
                rename("ADMIN_NAME_I"="ADMIN_NAME",
                       "COUNTRY_NAME_I"="COUNTRY_NAME"),
              by=c("JOIN_ID_I"="JOIN_ID"))%>%
    left_join(Centroids_df%>% # coordinates of J
                select(JOIN_ID,lon,lat)%>%
                rename("x_to"="lon",
                       "y_to"="lat"),
              by=c("JOIN_ID_J"="JOIN_ID"))%>% 
    left_join(admin_names%>% # name of admin J
                select(JOIN_ID,ADMIN_NAME,COUNTRY_NAME)%>%
                rename("ADMIN_NAME_J"="ADMIN_NAME",
                       "COUNTRY_NAME_J"="COUNTRY_NAME"),
              by=c("JOIN_ID_J"="JOIN_ID"))%>%
    select(JOIN_ID_I,JOIN_ID_J,x_from,y_from, x_to,y_to,pred_seed1,COUNTRY_NAME_I,ADMIN_NAME_I,COUNTRY_NAME_J,ADMIN_NAME_J)%>%
    mutate(ISOI=substr(JOIN_ID_I,1,3),
           ISOJ=substr(JOIN_ID_J,1,3))
  
  # Tilt for AB and BA relationships ####
  GLOBAL_mig_int=GLOBAL_mig_int%>%
    arrange(desc(pred_seed1))%>%
    mutate( S = map2_chr(JOIN_ID_I, JOIN_ID_J, ~str_flatten(sort(c(.x,.y))) ) )%>% #identify pairs (e.g. AB=BA)
    ungroup()%>%
    group_by(S)%>%
    mutate(n_count=1,
           tilt=(cumsum(n_count)-1)*45)%>% # tilt the pairs to avoid overlap
    select(-c(n_count))
  
  # copy to DB  ####
  copy_to(mig_db,
          GLOBAL_mig_int,
          name=paste0(c("GLOBAL_mig_int_M",slice_n,"all_admin"),collapse = "_"),
          temporary = FALSE,
          indexes = list("JOIN_ID_I","JOIN_ID_J","ISOI","ISOJ"),
          overwrite = T)
  
  # national data ####
  GLOBAL_mig_nat=table_gender_mig_nat%>%
    mutate(JOIN_ID_I=paste0(ISOI,"_",NODEI),
           JOIN_ID_J=paste0(ISOI,"_",NODEJ),
           sex_nat=MIGIJ_Fest+MIGIJ_Mest)%>%
    group_by(JOIN_ID_J)%>%
    top_n(slice_n,sex_nat)%>%
    ungroup()%>%
    left_join(Centroids_df%>% # coordinates of I
                select(JOIN_ID,lon,lat)%>%
                rename("x_from"="lon",
                       "y_from"="lat"),
              by=c("JOIN_ID_I"="JOIN_ID"))%>% 
    left_join(admin_names%>% # name of admin I
                select(JOIN_ID,ADMIN_NAME, COUNTRY_NAME)%>%
                rename("ADMIN_NAME_I"="ADMIN_NAME",
                       "COUNTRY_NAME_I"="COUNTRY_NAME"),
              by=c("JOIN_ID_I"="JOIN_ID"))%>%
    left_join(Centroids_df%>% # coordinates of J
                select(JOIN_ID,lon,lat)%>%
                rename("x_to"="lon",
                       "y_to"="lat"),
              by=c("JOIN_ID_J"="JOIN_ID"))%>% 
    left_join(admin_names%>% # name of admin J
                select(JOIN_ID,ADMIN_NAME,COUNTRY_NAME)%>%
                rename("ADMIN_NAME_J"="ADMIN_NAME",
                       "COUNTRY_NAME_J"="COUNTRY_NAME"),
              by=c("JOIN_ID_J"="JOIN_ID"))%>%
    select(JOIN_ID_I,JOIN_ID_J,x_from,y_from, x_to,y_to,sex_nat,COUNTRY_NAME_I,ADMIN_NAME_I,COUNTRY_NAME_J,ADMIN_NAME_J)%>%
    mutate(ISOI=substr(JOIN_ID_I,1,3),
           ISOJ=substr(JOIN_ID_J,1,3))
  
  
  # Tilt for AB and BA relationships ####
  GLOBAL_mig_nat=GLOBAL_mig_nat%>%
    group_by(JOIN_ID_I)%>%
    mutate( S = map2_chr(JOIN_ID_I, JOIN_ID_J, ~str_flatten(sort(c(.x,.y))) ) )%>% #identify pairs (e.g. AB=BA)
    ungroup()%>%
    group_by(S)%>%
    mutate(n_count=1,
           tilt=(cumsum(n_count)-1)*45)%>% # tilt the pairs to avoid overlap ####
  select(-c(n_count))
  
  # copy to DB ####
  copy_to(mig_db,
          GLOBAL_mig_nat,
          name=paste0(c("GLOBAL_mig_nat_M",slice_n,"all_admin"),collapse = "_"),
          temporary = FALSE,
          indexes = list("JOIN_ID_I","JOIN_ID_J","ISOI","ISOJ"),
          overwrite = T)
  
  # print ###
  print(paste(slice_n))
}



