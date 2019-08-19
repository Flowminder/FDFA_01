library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(rlang)
library(chorddiag) # devtools::install_github("mattflor/chorddiag")
library(igraph)
library(tidygraph)
library(stringr)
library(rgdal)
library(countrycode)

rm(list = ls())

# set the directories ####
# setwd("C:/Users/Xavier Vollenweider/Dropbox/FDFA_01/data/")
# code_dir="C:/Users/Xavier Vollenweider/Documents/Flowminder/Migration_FDFA/code/FDFA_01/app/"

setwd("/Users/vishva/Dropbox/FDFA_01/data/")  # VS ***
code_dir= "/Users/vishva/Desktop/FDFA_Local/FDFA_01/app/"  #VS ****

# load the shapefile ####
admin_poly_modelled=rgdal::readOGR("spatial/All_AdminUnits_final_simplified/all_admin_simplified.geojson")

country_poly_modelled=rgdal::readOGR("bin/country_poly_modelled_admin_ISO.shp")

# Load the migration csv

int_mig_sub=read.csv("table/combined_df_20180122.csv")

# connections to database ####
mig_db = src_sqlite("table/mig_db.sqlite3")

admin =tbl(mig_db, "admin") # dataframe from all_admin_simplified=admin_poly
admin_modelled =tbl(mig_db, "admin_poly_modelled") # dataframe from all_admin_simplified=admin_poly

EM<-tbl(mig_db, "EM") # emigration
IM =tbl(mig_db, "IM") # immigration
Net_IM =tbl(mig_db, "Net_IM") # Net immigration
Net_EM =tbl(mig_db, "Net_EM") # Net emigration
EM_IM =tbl(mig_db, "EM_IM") # Net emigration

gender_mig =tbl(mig_db, "gender_mig") # "gender_mig" tables: Silvia's model with gender disaggregated mig data
nick_mig =tbl(mig_db, "nick_mig") # "nick_mig" tables: contextual data, Nick's internal mig model, international mig

Top_perc_mig_ISO=tbl(mig_db, "Top_perc_mig_ISO")
world_share_mig=tbl(mig_db, "world_share_mig")
POP_ISO_NODE=tbl(mig_db, "POP_ISO_NODE")

ISO=tbl(mig_db, "ISO")
admin1_names=tbl(mig_db, "admin_names_temp")

names(mig_db)

m <- list( # plot margins
  l = 50,
  r = 50,
  b = 50,
  t = 100,
  pad = 4
)

# lauch the app ####
source(paste0(code_dir,
              "ui.R"))
source(paste0(code_dir,
              "server.R"))
shinyApp(ui, server)
