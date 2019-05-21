rm(list=ls())
library(rmapshaper)
library(sp)
library(geojsonio)
# set the directory ####
setwd("C:/Users/Xavier Vollenweider/Dropbox/FDFA_01/data/")

# load the data ####
all_admin=rgdal::readOGR("spatial/All_AdminUnits_final/ALL_AdminUnits_final.shp")

# Convert to json ####
all_admin_json <- geojson_json(all_admin,
                               group = "ISO_NODE", 
                               geometry = "polygon")
# Simplify the shape ####
check_sys_mapshaper()
# If you get an error, you will need to install mapshaper. First install node (https://nodejs.org/en/) and then install mapshaper with:
#   npm install -g mapshaper

# keep all the 2,198 poly with 0.03
all_admin_03=ms_simplify(all_admin_json,
                         keep=0.03,
                         sys=T)
print(object.size(all_admin_03),units = "auto") # 5.2Mb

# keep only 2187 poly out of 2,198 with 0.005
all_admin_005=ms_simplify(all_admin_json,
                          keep=0.005,
                          sys=T)
print(object.size(all_admin_005),units = "auto") # 22.3Mb

# backtransform from json to sp ####
all_admin_005_sp=geojson_sp(all_admin_005)
print(object.size(all_admin_005_sp),units = "auto") # 9.7Mb

all_admin_03_sp=geojson_sp(all_admin_03)
print(object.size(all_admin_03_sp),units = "auto") # 21.2 Mb

# identify missing poly ####
remaining_ISO=unique(all_admin_005_sp$ISO_NODE)
original_ISO=unique(all_admin_03_sp$ISO_NODE)
missing_ISO=which(!(original_ISO%in%remaining_ISO))


# extract missing poly all_admin_03_sp and add them in all_admin_005_sp #### 
all_admin_missing=all_admin_03_sp[missing_ISO,]
all_admin_simplified=rbind(all_admin_missing,
                           all_admin_005_sp)

print(object.size(all_admin_simplified),units = "auto") # 9.9Mb

# write to a geojson file #### 
rgdal::writeOGR(all_admin_simplified,
                "spatial/All_AdminUnits_final_simplified//all_admin_simplified.geojson",
                driver = "GeoJSON",
                layer=1)