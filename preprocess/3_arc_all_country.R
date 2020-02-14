library(dplyr)
library(rgdal)
library(purrr)
library(tidyverse)
# set the directories ####
table_gender_mig_inter_10=read.csv(paste0(dir_data_input,
                                          "data/MigrationEstimates/MigrEst_international_v7_light.csv")) # flow below 10 people removed to save space

table_gender_mig_nat=read.csv(paste0(dir_data_input,
                                     "data/MigrationEstimates/MigrEst_internal_v4.csv"))

simple_countries=readOGR(paste0(dir_data_input,"data/others/simple_countries.shp")) #"https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
simple_countries@data$id=as.character(simple_countries@data$id)
simple_countries@data$id[simple_countries@data$id=="SSD"]="SUD"
simple_countries@data$id[simple_countries@data$id=="SDN"]="SUD"
simple_countries$lon=coordinates(simple_countries)[,1]
simple_countries$lat=coordinates(simple_countries)[,2]
simple_countries_df=simple_countries@data

topo_correct_ogr = readOGR(paste0(dir_data_input,"data/others/AdminUnits_TopoCorrect_light.shp"))
topo_correct_ogr_df=topo_correct_ogr@data
topo_correct_ogr_df$lon=coordinates(topo_correct_ogr)[,1]
topo_correct_ogr_df$lat=coordinates(topo_correct_ogr)[,2]

topo_correct_ogr_df_s=topo_correct_ogr_df%>%
  group_by(ISO)%>%
  arrange(NODE)%>%
  top_n(1)

admin_names_complete=read.csv(paste0(dir_data_input,"data/others/admin_names_final.csv"))
topo_correct_ogr_df_anti=topo_correct_ogr_df_s%>%
  anti_join(simple_countries_df,
            by=c("ISO"="id"))%>%
  left_join(admin_names_complete%>%
              distinct(ISO,COUNTRY_NAME),
            by="ISO")%>%
  select(ISO,COUNTRY_NAME,lon,lat)%>%
  rename("id"="ISO",
         "name"="COUNTRY_NAME")

simple_countries_df=simple_countries_df%>%
  bind_rows(topo_correct_ogr_df_anti)

# countries_name=read.csv("data/others/ISO.csv",header=T) #source: https://unstats.un.org/unsd/methodology/m49/
# sudan=data.frame("ISO"="SUD",
#                  "country_name"="Sudan")
# countries_name=countries_name%>%
#   bind_rows(sudan)
# 
admin_names=tbl(mig_db,"admin_names")%>%
  collect(n=Inf)


# EMIGRATION ####
Slice_n=c(1,3,10,20,50)

slice_n=1

for(slice_n in Slice_n){
  
  # International data ####
  GLOBAL_mig_int=table_gender_mig_inter_10%>%
    group_by(ISOI,ISOJ)%>%
    summarise(pred_seed1=sum(pred_seed1))%>%
    group_by(ISOI)%>%
    top_n(slice_n,pred_seed1)%>%
    ungroup()%>%
    left_join(simple_countries_df%>% # coordinates of I
                select(id,lon,lat)%>%
                rename("x_from"="lon",
                       "y_from"="lat"),
              by=c("ISOI"="id"))%>%
    left_join(admin_names%>%
                distinct(ISO,COUNTRY_NAME)%>%
                rename("COUNTRY_NAME_I"="COUNTRY_NAME"),
              by=c("ISOI"="ISO"))%>%
    left_join(simple_countries_df%>% # coordinates of J
                select(id,lon,lat)%>%
                rename("x_to"="lon",
                       "y_to"="lat"),
              by=c("ISOJ"="id"))%>%
    left_join(admin_names%>%
                distinct(ISO,COUNTRY_NAME)%>%
                rename("COUNTRY_NAME_J"="COUNTRY_NAME"),
              by=c("ISOJ"="ISO"))%>%
    select(ISOI,ISOJ,x_from,y_from, x_to,y_to,pred_seed1,COUNTRY_NAME_I,COUNTRY_NAME_J)
  
  # Tilt for AB and BA relationships ####
  GLOBAL_mig_int=GLOBAL_mig_int%>%
    arrange(desc(pred_seed1))%>%
    mutate(S = map2_chr(ISOI, ISOJ, ~str_flatten(sort(c(.x,.y))) ) )%>% #identify pairs (e.g. AB=BA)
    ungroup()%>%
    group_by(S)%>%
    mutate(n_count=1,
           tilt=(cumsum(n_count)-1)*45)%>% # tilt the pairs to avoid overlap
    select(-c(n_count))
  
  # copy to DB  ####
  copy_to(mig_db,
          GLOBAL_mig_int,
          name=paste0(c("GLOBAL_mig_int_X",slice_n,"all_country"),collapse = "_"),
          temporary = FALSE,
          indexes = list("ISOI","ISOJ"),
          overwrite = T)
  # print ###
  print(paste(slice_n))
  
}

# IMMIGRATION ####
for(slice_n in Slice_n){
  # International data ####
  GLOBAL_mig_int=table_gender_mig_inter_10%>%
    group_by(ISOJ,ISOI)%>%
    summarise(pred_seed1=sum(pred_seed1))%>%
    group_by(ISOJ)%>%
    top_n(slice_n,pred_seed1)%>%
    ungroup()%>%
    left_join(simple_countries_df%>% # coordinates of I
                select(id,lon,lat)%>%
                rename("x_from"="lon",
                       "y_from"="lat"),
              by=c("ISOI"="id"))%>%
    left_join(admin_names%>%
                distinct(ISO,COUNTRY_NAME)%>%
                rename("COUNTRY_NAME_I"="COUNTRY_NAME"),
              by=c("ISOI"="ISO"))%>%
    left_join(simple_countries_df%>% # coordinates of J
                select(id,lon,lat)%>%
                rename("x_to"="lon",
                       "y_to"="lat"),
              by=c("ISOJ"="id"))%>%
    left_join(admin_names%>%
                distinct(ISO,COUNTRY_NAME)%>%
                rename("COUNTRY_NAME_J"="COUNTRY_NAME"),
              by=c("ISOJ"="ISO"))%>%
    select(ISOI,ISOJ,x_from,y_from, x_to,y_to,pred_seed1,COUNTRY_NAME_I,COUNTRY_NAME_J)
  
  # Tilt for AB and BA relationships ####
  GLOBAL_mig_int=GLOBAL_mig_int%>%
    arrange(desc(pred_seed1))%>%
    mutate( S = map2_chr(ISOI, ISOJ, ~str_flatten(sort(c(.x,.y))) ) )%>% #identify pairs (e.g. AB=BA)
    ungroup()%>%
    group_by(S)%>%
    mutate(n_count=1,
           tilt=(cumsum(n_count)-1)*45)%>% # tilt the pairs to avoid overlap
    select(-c(n_count))
  
  # copy to DB  ####
  copy_to(mig_db,
          GLOBAL_mig_int,
          name=paste0(c("GLOBAL_mig_int_M",slice_n,"all_country"),collapse = "_"),
          temporary = FALSE,
          indexes = list("ISOI","ISOJ"),
          overwrite = T)
  
  print(paste(slice_n))
}



