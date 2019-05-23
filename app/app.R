library(shiny)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(rlang)

rm(list = ls())

# set the directories ####
setwd("C:/Users/Xavier Vollenweider/Dropbox/FDFA_01/data/")
code_dir="C:/Users/Xavier Vollenweider/Documents/Flowminder/Migration_FDFA/code/FDFA_01/app/"

# load the shapefile ####
admin_poly=rgdal::readOGR("spatial/All_AdminUnits_final_simplified/all_admin_simplified.geojson")

# connections to database ####
mig_db = src_sqlite("table/mig_db.sqlite3")

admin =tbl(mig_db, "admin") # dataframe from all_admin_simplified=admin_poly
EM<-tbl(mig_db, "EM") # emigration
IM =tbl(mig_db, "IM") # immigration
Net_IM =tbl(mig_db, "Net_IM") # Net immigration
Net_EM =tbl(mig_db, "Net_EM") # Net emigration
EM_IM =tbl(mig_db, "EM_IM") # Net emigration

gender_mig =tbl(mig_db, "gender_mig") # "gender_mig" tables: Silvia's model with gender disaggregated mig data
nick_mig =tbl(mig_db, "nick_mig") # "nick_mig" tables: contextual data, Nick's internal mig model, international mig

Top_perc_mig_ISO=tbl(mig_db, "Top_perc_mig_ISO")
world_share_mig=tbl(mig_db, "world_share_mig")
# lauch the app ####
source(paste0(code_dir,
              "ui.R"))
source(paste0(code_dir,
              "server.R"))
shinyApp(ui, server)

