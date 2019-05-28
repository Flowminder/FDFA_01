#########################
# database 
#########################

# load the data ####
table_gender_mig=read.csv("table/IntMigrEst.csv")

table_nick_mig=read.csv("table/combined_df_20180122.csv")

all_admin_light=rgdal::readOGR("spatial/All_AdminUnits_final_simplified/all_admin_simplified.geojson")

admin_poly_modelled=rgdal::readOGR("spatial/All_AdminUnits_final_simplified/admin_poly_modelled.geojson")
country_poly_modelled=rgdal::readOGR("spatial/All_AdminUnits_final_simplified/countries_poly_modelled.geojson")


ISO=read.delim("table/ISO.txt")

ISO_code=read.delim("table/ISO-3166-Countries-with-Regional-Codes.txt")

# Emigration movements per admin unit ####
EM_data=table_gender_mig%>%
  mutate(ISO_NODEI=paste(ISOI,as.integer(NODEI),sep="_"))%>% # ISO_NODEI: origin ISO and 
  group_by(ISO_NODEI)%>%
  summarise(total=sum(MIGIJ_F_est+MIGIJ_M_est,na.rm = T),
            females=sum(MIGIJ_F_est,na.rm = T),
            males=sum(MIGIJ_M_est,na.rm = T),
            females_perc=females/total,
            males_perc=males/total)%>%
  mutate(females_perc=ifelse(is.infinite(females_perc),0,females_perc),
         males_perc=ifelse(is.infinite(males_perc),0,males_perc))%>%
  select(ISO_NODEI,total,females,males,females_perc,males_perc)%>%
  mutate(ISO=substr(ISO_NODEI,1,3))%>%
  rename("ISO_NODE"="ISO_NODEI")

# Immigratoin movements per admin unit ####
IM_data=table_gender_mig%>%
  mutate(ISOJ=ISOI)%>% # to be removed once international data are available
  mutate(ISO_NODEJ=paste(ISOJ,as.integer(NODEJ),sep="_"))%>%
  group_by(ISO_NODEJ)%>%
  summarise(total=sum(MIGIJ_F_est+MIGIJ_M_est,na.rm = T),
            females=sum(MIGIJ_F_est,na.rm = T),
            males=sum(MIGIJ_M_est,na.rm = T),
            females_perc=females/total,
            males_perc=males/total)%>%
  mutate(females_perc=ifelse(is.infinite(females_perc),0,females_perc),
         males_perc=ifelse(is.infinite(males_perc),0,males_perc))%>%
  select(ISO_NODEJ,total,females,males,females_perc,males_perc)%>%
  mutate(ISO=substr(ISO_NODEJ,1,3))%>%
  rename("ISO_NODE"="ISO_NODEJ")

# Net immigration movements per admin unit ####
Net_IM=IM_data%>%
  select(ISO_NODE,total,females,males)%>%
  left_join(EM_data%>%
              rename("total_EM"="total",
                     "females_EM"="females",
                     "males_EM"="males"),
            by=c("ISO_NODE"))%>%
  mutate(net_IM=total-total_EM,
         net_IM_females=females-females_EM,
         net_IM_males=males-males_EM,
         females_perc=net_IM_females/net_IM,
         males_perc=net_IM_males/net_IM)%>%
  mutate(females_perc=ifelse(is.infinite(females_perc),0,females_perc),
         males_perc=ifelse(is.infinite(males_perc),0,males_perc))%>%
  select(ISO_NODE,ISO,net_IM,net_IM_females,net_IM_males,females_perc,males_perc)%>%
  rename("total"="net_IM",
         "females"="net_IM_females",
         "males"="net_IM_males")

# Net emigration movements per admin unit ####
Net_EM=IM_data%>%
  select(ISO_NODE,total,females,males)%>%
  left_join(EM_data%>%
              rename("total_EM"="total",
                     "females_EM"="females",
                     "males_EM"="males"),
            by=c("ISO_NODE"))%>%
  mutate(net_EM=total_EM-total,
         net_EM_females=females_EM-females,
         net_EM_males=males_EM-males,
         females_perc=net_EM_females/net_EM,
         males_perc=net_EM_males/net_EM)%>%
  mutate(females_perc=ifelse(is.infinite(females_perc),0,females_perc),
         males_perc=ifelse(is.infinite(males_perc),0,males_perc))%>%
  select(ISO_NODE,ISO,net_EM,net_EM_females,net_EM_males,females_perc,males_perc)%>%
  rename("total"="net_EM",
         "females"="net_EM_females",
         "males"="net_EM_males")

# Total emigration movements per admin unit ####
EM_IM=IM_data%>%
  select(ISO_NODE,total,females,males)%>%
  left_join(EM_data%>%
              rename("total_EM"="total",
                     "females_EM"="females",
                     "males_EM"="males"),
            by=c("ISO_NODE"))%>%
  mutate(EM_IM=total+total_EM,
         EM_IM_females=females+females_EM,
         EM_IM_males=males+males_EM,
         females_perc=EM_IM_females/EM_IM,
         males_perc=EM_IM_males/EM_IM)%>%
  select(ISO_NODE,ISO,EM_IM,EM_IM_females,EM_IM_males,females_perc,males_perc)%>%
  rename("total"="EM_IM",
         "females"="EM_IM_females",
         "males"="EM_IM_males")

# add the tables in a SQLITE database ####
mig_db <- src_sqlite("table/mig_db.sqlite3",
                     create = T)

copy_to(mig_db,
        EM_data,
        name="EM",
        temporary = FALSE,
        indexes = list("ISO_NODE","ISO"),
        overwrite = T)

copy_to(mig_db,
        IM_data,
        name="IM",
        temporary = FALSE,
        indexes = list("ISO_NODE","ISO"),
        overwrite = T)

copy_to(mig_db,
        Net_IM,
        name = "Net_IM",
        temporary = FALSE,
        indexes = list("ISO_NODE","ISO"),
        overwrite = T)

copy_to(mig_db,
        Net_EM,
        name = "Net_EM",
        temporary = FALSE,
        indexes = list("ISO_NODE","ISO"),
        overwrite = T)

copy_to(mig_db,
        EM_IM,
        name = "EM_IM",
        temporary = FALSE,
        indexes = list("ISO_NODE","ISO"),
        overwrite = T)

# store table_gender_mig in the database ####
table_gender_mig_s=table_gender_mig%>%
  mutate(ISOJ=ISOI,
         CONTJ=CONTI)%>% # to be removed once international data are available
  mutate(ISO_NODEJ=paste(ISOJ,as.integer(NODEJ),sep="_"),
         ISO_NODEI=paste(ISOI,as.integer(NODEI),sep="_"))%>%
  select(MIGIJ_F_est,MIGIJ_M_est,ISO_NODEI,ISO_NODEJ,CONTI,ISOI,CONTJ,ISOJ)%>%
  rename("females"="MIGIJ_F_est",
         "males"="MIGIJ_M_est")%>%
  mutate(total=females+males,
         females_perc=females/total,
         males_perc=males/total)


copy_to(mig_db,
        table_gender_mig_s,
        name = "gender_mig",
        temporary = FALSE,
        indexes = list("ISO_NODEI","ISO_NODEJ","CONTI","ISOI","CONTJ","ISOJ"),
        overwrite = T)

# store table_nick_mig in the database ####
table_nick_mig_s=table_nick_mig%>%
  mutate(ISO_NODEJ=paste(ISOJ,as.integer(NODEJ),sep="_"),
         ISO_NODEI=paste(ISOI,as.integer(NODEI),sep="_"))%>%
  select(CONTI, ISOI, ISO_NODEI,IPUMSPOPI, POPI, URBANPROPI, PPPI, ISOIPPP,CONTJ,ISOJ,ISO_NODEJ,IPUMSPOPJ,POPJ,URBANPROPJ,PPPJ,ISOJPPP,      
         IPUMSI, DOMMIGIJ,INTMIGIJ,pred,ASYMMIG_IN,ASYMMIG_OUT,movein,moveinprop,moveout,moveoutprop)

copy_to(mig_db,
        table_nick_mig_s,
        name = "nick_mig",
        temporary = FALSE,
        indexes = list("CONTI","ISOI","ISO_NODEI","CONTJ","ISOJ","ISO_NODEJ"),
        overwrite = T)

# store data from the SpatialPolygonDataFrame all_admin_light in the database ####
all_admin_light_db=all_admin_light
all_admin_light_db@data=all_admin_light_db@data%>%
  mutate(ISO_NODE=as.character(ISO_NODE))%>%
  select(CONT,ISO,ISO_NODE,TotPop,UrbanPop,UrbanProp)

copy_to(mig_db,
        all_admin_light_db@data,
        name="admin",
        temporary = FALSE,
        indexes = list("CONT","ISO","ISO_NODE"),
        overwrite = T)


# Share of migrant in total population ####
world_share_mig=IM%>%
  group_by(ISO)%>%
  summarise(sum_total=sum(total,na.rm = T))%>%
  left_join(nick_mig%>%
              select(ISOI,ISO_NODEI,POPI)%>%
              group_by(ISO_NODEI)%>%
              summarise(POP=mean(POPI,na.rm=T),
                        ISOI=ISOI)%>%
              group_by(ISOI)%>%
              summarise(POP=sum(POP,na.rm=T)),
            by=c("ISO"="ISOI"))%>%
  ungroup()%>%
  summarise(tot_pop=sum(POP,na.rm = T),
            tot_mig=sum(sum_total,na.rm = T))%>%
  mutate(mig_share=tot_mig/tot_pop)

copy_to(mig_db,
        world_share_mig,
        name="world_share_mig",
        temporary = FALSE,
        overwrite = T)

# top 10 countries by migrant share ####
Top_perc_mig_ISO=IM%>%
  group_by(ISO)%>%
  summarise(sum_total=sum(total,na.rm = T))%>%
  left_join(nick_mig%>%
              select(ISOI,ISO_NODEI,POPI)%>%
              group_by(ISO_NODEI)%>%
              summarise(POP=mean(POPI,na.rm=T),
                        ISOI=ISOI)%>%
              group_by(ISOI)%>%
              summarise(POP=sum(POP,na.rm=T)),
            by=c("ISO"="ISOI"))%>%
  collect()%>%
  mutate(migrant_per_pop=sum_total/POP)%>%
  arrange(desc(migrant_per_pop))%>%
  top_n(10,migrant_per_pop)

copy_to(mig_db,
        Top_perc_mig_ISO,
        name="Top_perc_mig_ISO",
        temporary = FALSE,
        indexes = list("ISO"),
        overwrite = T)


# top 10 countries by migrant share ####
POP_ISO_NODE=nick_mig%>%
              select(ISO_NODEI,POPI,ISOI)%>%
              group_by(ISO_NODEI)%>%
              summarise(POP=mean(POPI,na.rm=T),
                        ISO=ISOI)%>%
  rename("ISO_NODE"="ISO_NODEI")

copy_to(mig_db,
        POP_ISO_NODE,
        name="POP_ISO_NODE",
        temporary = FALSE,
        indexes = list("ISO","ISO_NODE"),
        overwrite = T)

#  add ISO-country_name #####
copy_to(mig_db,
        ISO,
        name="ISO",
        temporary = FALSE,
        indexes = list("ISO"),
        overwrite = T)

#  add admin_poly_modelled #####
admin_poly_modelled_data=admin_poly_modelled@data
admin_poly_modelled_data=admin_poly_modelled_data%>%
  mutate(ISO_NODE=paste0(ISO,"_",IPUMSID))

copy_to(mig_db,
        admin_poly_modelled_data,
        name = "admin_poly_modelled",
        temporary = FALSE,
        indexes = list("ISO","ISO_NODE"),
        overwrite = T)

# add admin names ####

## unzip admin 1 
## unzip files downloaded from IMPUM: https://international.ipums.org/international/gis_yrspecific_1st.shtml
for(i in 1:length(dir("bin/indiv_countries"))){
  unzip(paste0("bin/indiv_countries/",dir("bin/indiv_countries")[i]),
        exdir ="C:/Users/Xavier Vollenweider/Dropbox/FDFA_01/data/bin/indiv_countries_unzip")
}
# 
# # unzip admin 2
# # unzip files downloaded from IMPUM: https://international.ipums.org/international/gis_yrspecific_2nd.shtml
for(i in 1:length(dir("bin/indiv_countries_adm2"))){
  unzip(paste0("bin/indiv_countries_adm2/",dir("bin/indiv_countries_adm2")[i]),
        exdir ="C:/Users/Xavier Vollenweider/Dropbox/FDFA_01/data/bin/indiv_countries_adm2_unzip")
}

# admin 1 load the DBF from those extracted in indiv_countries_unzip 
dbf_files=dir("bin/indiv_countries_unzip/")[grep(".dbf",dir("bin/indiv_countries_unzip/"))]

country_set=c()
for(i in 1:length(dbf_files)){
  country_file=foreign::read.dbf(paste0("bin/indiv_countries_unzip/",dbf_files[i]),as.is=T)  
  
  country_file=country_file[,c("CNTRY_NAME","ADMIN_NAME","CNTRY_CODE",
                               names(country_file)[grep("IPUM",names(country_file))])]
  names(country_file)=c("CNTRY_NAME","ADMIN_NAME","CNTRY_CODE","NODE")
  
  country_set=country_set%>%
    bind_rows(country_file)
}
head(country_set)

# admin 2 load the DBF from those extracted in indiv_countries_adm2_unzip 
dbf_files=dir("bin/indiv_countries_adm2_unzip/")[grep(".dbf",dir("bin/indiv_countries_adm2_unzip/"))]

for(i in 1:length(dbf_files)){
  
  if(i %in% c(1,2)){ # Mali Sen
  country_file=foreign::read.dbf(paste0("bin/indiv_countries_adm2_unzip/",dbf_files[i]),as.is=T)  
  
  country_file=country_file[,c("CNTRY_NAME","ADMIN_NAME","CNTRY_CODE",
                               names(country_file)[grep("IPUM",names(country_file))])]
  names(country_file)=c("CNTRY_NAME","ADMIN_NAME","CNTRY_CODE","NODE_temp")
  
  country_file$NODE_lead=as.numeric(substr(country_file$NODE_temp,1,3))
  country_file$NODE_end=as.numeric(substr(country_file$NODE_temp,4,6))
  country_file$NODE=paste0(country_file$NODE_lead,country_file$NODE_end)
  
  country_file=country_file%>%
    select("CNTRY_NAME","ADMIN_NAME","CNTRY_CODE","NODE")
  
  country_set=country_set%>%
    bind_rows(country_file)
  }
  
  if(i %in% c(3)){ # ZMB
    country_file=foreign::read.dbf(paste0("bin/indiv_countries_adm2_unzip/",dbf_files[i]),as.is=T)  
    
    country_file=country_file[,c("CNTRY_NAME","ADMIN_NAME","CNTRY_CODE",
                                 names(country_file)[grep("IPUM",names(country_file))])]
    names(country_file)=c("CNTRY_NAME","ADMIN_NAME","CNTRY_CODE","NODE_temp")
    
    country_file$NODE_lead=as.numeric(substr(country_file$NODE_temp,1,3))
    country_file$NODE_end=as.numeric(substr(country_file$NODE_temp,4,6))
    country_file$NODE=paste0(country_file$NODE_lead,0,country_file$NODE_end)
    country_file$NODE[country_file$NODE%in%c("9010")]="910"
    country_file$NODE[country_file$NODE%in%c("9011")]="911"
    country_file=country_file%>%
      select("CNTRY_NAME","ADMIN_NAME","CNTRY_CODE","NODE")
    
    country_set=country_set%>%
      bind_rows(country_file)
  }
}

# adm1 load the DBF from those extracted in a folder 
sub_dir=list.dirs("bin/indiv_countries_unzip/")
sub_dir=sub_dir[2:length(sub_dir)]

for(i in 1:length(sub_dir)){
  
  dbf_file=dir(sub_dir[i])[grep(".dbf",dir(sub_dir[i]))]
  
  country_file=foreign::read.dbf(paste0(sub_dir[i],"/",dbf_file),as.is=T)  
  
  country_file=country_file[,c(1:3,
                               grep("IPUM",names(country_file)))]
  names(country_file)=c("CNTRY_NAME","ADMIN_NAME","CNTRY_CODE","NODE")
  
  country_set=country_set%>%
    bind_rows(country_file)
}

# adm2 load the DBF from those extracted in a folder 
sub_dir=list.dirs("bin/indiv_countries_adm2_unzip/")
sub_dir=sub_dir[2:length(sub_dir)]

# gender_mig%>%
#   filter(ISOI=="CMR")%>%
#   select(ISO_NODEI)%>%
#   distinct()%>%
#   arrange(ISO_NODEI)%>%
#   print(n=100)

for(i in 1:length(sub_dir)){
  
  dbf_file=dir(sub_dir[i])[grep(".dbf",dir(sub_dir[i]))]
  
  country_file=foreign::read.dbf(paste0(sub_dir[i],"/",dbf_file),as.is=T)  
  
  country_file=country_file[,c(1:3,
                               grep("IPUM",names(country_file)))]
  names(country_file)=c("CNTRY_NAME","ADMIN_NAME","CNTRY_CODE","NODE_temp")
  
  head(country_file)
  country_file$NODE_lead=as.numeric(substr(country_file$NODE_temp,1,3))
  country_file$NODE_end=as.numeric(substr(country_file$NODE_temp,4,6))
  country_file$NODE=paste0(country_file$NODE_lead,0,country_file$NODE_end)
  str_sub(country_file$NODE[which(nchar(country_file$NODE)>3)],2,2)=""
  
  country_file%>%
    arrange(NODE)%>%
    select(NODE)
  country_file=country_file%>%
    select("CNTRY_NAME","ADMIN_NAME","CNTRY_CODE","NODE")
  
  country_set=country_set%>%
    bind_rows(country_file)

}

admin_names=country_set%>%
  mutate(CNTRY_CODE=as.numeric(CNTRY_CODE))%>%
  select("CNTRY_NAME","ADMIN_NAME","CNTRY_CODE","NODE")%>%
  left_join(ISO_code%>%
              select("alpha.3","country.code"),
            by=c("CNTRY_CODE"="country.code"))%>%
  mutate(NODE=as.numeric(NODE))%>%
  rename("ISO"="alpha.3",
         "NODE_NAME"="ADMIN_NAME",
         "country_name"="CNTRY_NAME")%>%
  select(country_name,NODE_NAME,CNTRY_CODE,ISO,NODE)%>%
  filter(is.na(NODE)==F)%>%
  mutate(ISO_NODE=paste0(ISO,"_",NODE))%>%
  select(ISO,ISO_NODE,country_name,NODE_NAME)


gender_mig%>%
  select(ISO_NODEI)%>%
  rename("ISO_NODE"="ISO_NODEI")%>%
  distinct(ISO_NODE)%>%
  collect()%>%
  left_join(admin_names,
            by="ISO_NODE")%>%
  filter(is.na(NODE_NAME)==T)%>%
  mutate(country=substr(ISO_NODE,1,3))%>%
  distinct(country)%>%
  print(n=400)

copy_to(mig_db,
        admin_names,
        name = "admin_names",
        temporary = FALSE,
        indexes = list("ISO","ISO_NODE"),
        overwrite = T)

