rm(list=ls())
library(dplyr)
library(RPostgreSQL)
library(leaflet)
# set the directory ####
setwd("C:/Users/Xavier Vollenweider/Dropbox/FDFA_01/data/")

# load the data ####
all_admin=rgdal::readOGR("spatial/All_AdminUnits_final/ALL_AdminUnits_final.shp")

all_admin_GRASS_simple=rgdal::readOGR("spatial/All_AdminUnits_final_simplified/ALL_AdminUnits_final_GRASS_simple.shp") # data created with GRASS-> v.generalize.simplify

# simplify shape for faster map rendering ####
all_admin_s=rmapshaper::ms_simplify(all_admin_GRASS_simple,
                                    keep=0.1)

print(object.size(all_admin),units = "auto") # from 243 Mb to 
print(object.size(all_admin_GRASS_simple),units = "auto") # 30.5 Mb
print(object.size(all_admin_s),units = "auto") # 30.5 Mb

# check no ISO_NODE has been lost ####
length(unique(all_admin$ISO_NODE))
length(unique(all_admin_s$ISO_NODE))

# write as GeoJSON ####
rgdal::ogrDrivers()
rgdal::writeOGR(all_admin_s,
                "spatial/All_AdminUnits_final_simplified/ALL_AdminUnits_final_light.geojson",
                driver = "GeoJSON",
                layer=1)
