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
table_mig=read.csv("table/combined_df_20180122.csv")

all_admin_light=rgdal::readOGR("spatial/All_AdminUnits_final_simplified/ALL_AdminUnits_final_light.geojson")

# Select origin data Dar es Salaam ####
dar_data=table_mig%>%
  filter(ISOJ=="TZA" & NODEJ==7,
         is.na(movein)==F,
         pred>0)%>%
  mutate(ISO_NODEJ=paste(ISOJ,as.integer(NODEJ),sep="_"),
         ISO_NODEI=paste(ISOI,as.integer(NODEI),sep="_"))%>%
  arrange(ISO_NODEI)%>%
  select(pred,ISO_NODEI,ISO_NODEJ)

# show on leaflet ####
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

