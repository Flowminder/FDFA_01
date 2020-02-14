############################
# get state names ##########
############################
library(tictoc)

Centroids=readOGR(paste0(dir_data_input,
                         "data/SpatialData/Centroids.shp"))
Centroids_df=Centroids@data
Centroids_df$lon=coordinates(Centroids)[,1]
Centroids_df$lat=coordinates(Centroids)[,2]

Centroids_df$country=NA
Centroids_df$state=NA
Centroids_df[1:100,]

# tic()
# for(i in 2713:nrow(Centroids_df)){
#   tic()
#   lon_q=Centroids_df$lon[i]
#   lat_q=Centroids_df$lat[i]
#   
#   querry_i=paste0("https://nominatim.openstreetmap.org/reverse?format=jsonv2&lat=",
#                   lat_q,
#                   "&lon=",
#                   lon_q,
#                   "&zoom=5&addressdetails=1&accept-language=en")
#   querry_i_result=httr::GET(querry_i)
#   if(querry_i_result$status_code==200){
#     content=rawToChar(querry_i_result$content)
#     content_parsed=jsonlite::fromJSON(content)
#     
#     if(is.null(content_parsed$address$state)==F){
#       Centroids_df$country[i]=content_parsed$address$country
#     }else{
#       Centroids_df$country[i]=NA
#       print("no_country")
#     }
#     if(is.null(content_parsed$address$state)==F){
#       Centroids_df$state[i]=content_parsed$address$state
#     }else{
#       Centroids_df$state[i]=NA    
#       print("no_state")
#     }
#   }else{
#     Centroids_df$country[i]=NA
#     Centroids_df$state[i]=NA
#     print(paste(querry_i_result$status_code,lon_q,lat_q))
#   }
#   print(i)
#   toc()
# }
# toc()
# 
# write.csv(Centroids_df,
#           "data/SpatialData/Centroids_osm_names.csv",
#           row.names = F)

# merge with existing names #####
library(dplyr)
library(dbplyr)
mig_db = src_sqlite("C:/Users/Xavier Vollenweider/Documents/Flowminder/Migration_FDFA/code/FDFA_01_prod/data/table/mig_db.sqlite3")
admin_names_tbl=tbl(mig_db,"admin_names")
admin_names_c=collect(admin_names_tbl,n=Inf)
admin_names_c=admin_names_c%>%
  mutate_all(as.character)

Centroids_osm_names=read.csv(paste0(dir_data_input,"data/SpatialData/Centroids_osm_names.csv"))
Centroids_osm_names=Centroids_osm_names%>%
  mutate_all(as.character)

admin_names_others=read.csv(paste0(dir_data_input,"data/others/admin_names.csv")) # bind between NonIPUMSIcountries_AdminUnitNames.dbf.xml and IPUMSIcountries_AdminUnitNames.csv
admin_names_others=admin_names_others%>%
  mutate_all(as.character)

admin_names_nona=admin_names_c%>%
  filter(is.na(NODE_NAME)==F)

osm_update_names=Centroids_osm_names%>%
  left_join(admin_names_c,
            by=c("JOIN_ID"="ISO_NODE"))
names(Centroids_osm_names)
names(admin_names_c)
osm_update_names=osm_update_names%>%
  mutate(COUNTRY_NAME=country,
         COUNTRY_NAME=ifelse(is.na(COUNTRY_NAME),
                             country_name,COUNTRY_NAME),
         ADMIN_NAME=state,
         ADMIN_NAME=ifelse(is.na(ADMIN_NAME),
                           NODE_NAME,ADMIN_NAME))

osm_update_names=osm_update_names%>%
  select(ISO.x,CONT,JOIN_ID,COUNTRY_NAME,ADMIN_NAME)%>%
  rename("ISO"="ISO.x")

osm_update_names=osm_update_names%>%
  left_join(admin_names_others,
            by="JOIN_ID")

names(osm_update_names)
osm_update_names=osm_update_names%>%
  mutate(ADMIN_NAME=ADMIN_NAME.x,
         ADMIN_NAME=ifelse(is.na(ADMIN_NAME),
                           ADMIN_NAME.y,ADMIN_NAME))%>%
  select(ISO.x,CONT,JOIN_ID,COUNTRY_NAME,ADMIN_NAME)%>%
  rename("ISO"="ISO.x")


countries_name=read.csv(paste0(dir_data_input,"data/others/ISO.csv",header=T)) #source: https://unstats.un.org/unsd/methodology/m49/
countries_name=countries_name%>%
  mutate_all(as.character)

osm_update_names=osm_update_names%>%
  left_join(countries_name,
            by="ISO")
osm_update_names=osm_update_names%>%
  mutate(COUNTRY_NAME=ifelse(is.na(COUNTRY_NAME),
                             country_name,COUNTRY_NAME))%>%
  select(ISO,CONT,JOIN_ID,COUNTRY_NAME,ADMIN_NAME)

# write files ####
write.csv(osm_update_names,
          paste0(dir_data_out,"data/others/admin_names_complete.csv"),
          row.names = F)