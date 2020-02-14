rm(list=ls())
library(dplyr)
library(rgdal)
library(purrr)
library(RSQLite)
dir_code="C:/Users/Xavier Vollenweider/Documents/Flowminder/Migration_FDFA/code/FDFA_01_dev_v3/preprocess/"
dir_data_input="C:/Users/Xavier Vollenweider/Documents/Flowminder/Migration_FDFA/code/FDFA_01_dev_v2/"
dir_data_out="C:/Users/Xavier Vollenweider/Documents/Flowminder/Migration_FDFA/code/FDFA_01_dev_v3/data/"

admin_names=read.csv(paste0(dir_data_out,"admin_names_final.csv"))

mig_db = src_sqlite(paste0(dir_data_out,"mig_db.sqlite3"),
                    create = T)

copy_to(mig_db,
        admin_names,
        name="admin_names",
        temporary = FALSE,
        indexes = list("JOIN_ID","ISO"),
        overwrite = T)

source(paste0(dir_code,
              "2_arc_all_admin.R"))
source(paste0(dir_code,
              "3_arc_all_country.R"))
source(paste0(dir_code,
              "4_arc_male_female_admin.R"))
source(paste0(dir_code,
              "5_arc_male_female_country.R"))
source(paste0(dir_code,
              "6_col_admin.R"))

source(paste0(dir_code,
              "7_global_fig.R"))
source(paste0(dir_code,
              "8_nat_fig.R"))