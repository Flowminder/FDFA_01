########################
# data inspection ####
########################

rm(list=ls())
library(dplyr)
library(RPostgreSQL)
library(leaflet)
# set the directory ####
setwd("C:/Users/Xavier Vollenweider/Dropbox/FDFA_01/data/")

# load the data ####
table_nick_mig=read.csv("table/combined_df_20180122.csv")

table_gender_mig=read.csv("table/IntMigrEst.csv")

all_admin_light=rgdal::readOGR("spatial/All_AdminUnits_final_simplified/all_admin_simplified.geojson")

# Nick's model of internal migration ####
#  Select destination data Dar es Salaam 
dar_data=table_nick_mig%>%
  filter(ISOJ=="TZA" & NODEJ==7,
         is.na(movein)==F,
         pred>0)%>%
  mutate(ISO_NODEJ=paste(ISOJ,as.integer(NODEJ),sep="_"),
         ISO_NODEI=paste(ISOI,as.integer(NODEI),sep="_"))%>%
  arrange(ISO_NODEI)%>%
  select(pred,ISO_NODEI,ISO_NODEJ)

# show on leaflet 
data_to_plot=all_admin_light

data_to_plot@data=data_to_plot@data%>%
  mutate(ISO_NODE=as.character(ISO_NODE))%>%
  left_join(dar_data,
            by=c("ISO_NODE"="ISO_NODEI"))


leaflet(data_to_plot)%>%
  addPolygons(weight=1,
              color = "#444444",
              smoothFactor = 1,
              fillOpacity = 1,
              fillColor = ~colorQuantile("Greens", pred)(pred),
              popup = ~pred)%>%
  addLegend("bottomright",
            colors = ~colorQuantile("Greens", pred)(pred), 
            labels = ~pred,
            opacity = 1)

# Gender disaggregated model of internal migration ####
names(table_gender_mig)
dim(table_gender_mig)
head(table_gender_mig)
table(table_gender_mig$NODEI[table_gender_mig$ISOI=="ARG"])
table(table_gender_mig$NODEJ[table_gender_mig$ISOI=="ARG"])

#  Select destination data Buenos Aires 
BA_data=table_gender_mig%>%
  filter(ISOI=="ARG" & NODEJ==2)%>%
  mutate(ISO_NODEJ=paste(ISOI,as.integer(NODEJ),sep="_"),
         ISO_NODEI=paste(ISOI,as.integer(NODEI),sep="_"))%>%
  arrange(ISO_NODEI)%>%
  select(MIGIJ_F_est,MIGIJ_M_est,ISO_NODEI,ISO_NODEJ)

head(BA_data)
# show on leaflet 
data_to_plot=all_admin_light

data_to_plot@data=data_to_plot@data%>%
  mutate(ISO_NODE=as.character(ISO_NODE))%>%
  left_join(BA_data,
            by=c("ISO_NODE"="ISO_NODEI"))

leaflet(data_to_plot)%>%
  addPolygons(weight=1,
              color = "#444444",
              smoothFactor = 1,
              fillOpacity = 1,
              fillColor = ~colorQuantile("Greens", MIGIJ_F_est)(MIGIJ_F_est),
              popup = ~MIGIJ_F_est)%>%
  addLegend("bottomright",
            colors = ~colorQuantile("Greens", MIGIJ_F_est)(MIGIJ_F_est), 
            labels = ~MIGIJ_F_est,
            opacity = 1)

# Total Emigration movement per admin unit from the SQLite db ####
mig_db = src_sqlite("table/mig_db.sqlite3")
EM_sq =tbl(mig_db, "EM")
admin_sq =tbl(mig_db, "admin")

data_to_plot=all_admin_light
data_to_plot@data=collect(admin_sq%>%
                                  left_join(EM_sq,
                                            by=c("ISO_NODE")))
leaflet(data_to_plot)%>%
  addPolygons(weight=1,
              color = "#444444",
              smoothFactor = 1,
              fillOpacity = 1,
              fillColor = ~colorQuantile("Greens", total)(total),
              popup = ~total)
