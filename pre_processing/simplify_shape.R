rm(list=ls())
library(rmapshaper)
library(sp)
library(geojsonio)
library(dplyr)
# set the directory ####
setwd("C:/Users/Xavier Vollenweider/Dropbox/FDFA_01/data/")

# load the data ####
all_admin=rgdal::readOGR("spatial/AdminUnits/AdminUnits.shp")

all_admin@data=all_admin@data%>%
  rename("ISO_NODE"="JOIN_ID")

dim(all_admin)
print(object.size(all_admin),units = "auto") # 272.5 Mb

# Convert to json ####
all_admin_json <- geojson_json(all_admin,
                               group = "ISO_NODE", 
                               geometry = "polygon")
print(object.size(all_admin_json),units = "auto") # 612 Mb

# Simplify the shape ####
check_sys_mapshaper()
# If you get an error, you will need to install mapshaper. First install node (https://nodejs.org/en/) and then install mapshaper with:
#   npm install -g mapshaper

# keep all the 3085 poly with 0.04
all_admin_06=ms_simplify(all_admin_json,
                         keep=0.06,
                         sys=T)
print(object.size(all_admin_06),units = "auto") # 46Mb


# keep only 3,054 poly out of 2,198 with 0.005
all_admin_005=ms_simplify(all_admin_json,
                          keep=0.005,
                          sys=T)
print(object.size(all_admin_005),units = "auto") # 6 Mb

# backtransform from json to sp ####
all_admin_005_sp=geojson_sp(all_admin_005)
print(object.size(all_admin_005_sp),units = "auto") # 12 Mb

all_admin_06_sp=geojson_sp(all_admin_06)
print(object.size(all_admin_06_sp),units = "auto") # 21.2 Mb

# identify missing poly ####
remaining_ISO=unique(all_admin_005_sp$ISO_NODE)
original_ISO=unique(all_admin_06_sp$ISO_NODE)
missing_ISO=which(!(original_ISO%in%remaining_ISO))


# extract missing poly all_admin_03_sp and add them in all_admin_005_sp #### 
all_admin_missing=all_admin_06_sp[missing_ISO,]
all_admin_simplified=rbind(all_admin_missing,
                           all_admin_005_sp)

print(object.size(all_admin_simplified),units = "auto") # 9.9Mb

# write to a geojson file #### 
rgdal::writeOGR(all_admin_simplified,
                "spatial/AdminUnits_simplified/AdminUnits_simplified.geojson",
                driver = "GeoJSON",
                layer=1)