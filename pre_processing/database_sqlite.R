#########################
# database 
#########################

# load the data ####
table_gender_mig=read.csv("table/IntMigrEst.csv")

table_nick_mig=read.csv("table/combined_df_20180122.csv")

all_admin_light=rgdal::readOGR("spatial/All_AdminUnits_final_simplified/all_admin_simplified.geojson")

# Emigration movements per admin unit ####
EM_data=table_gender_mig%>%
  mutate(ISO_NODEJ=paste(ISOI,as.integer(NODEJ),sep="_"),
         ISO_NODEI=paste(ISOI,as.integer(NODEI),sep="_"))%>%
  group_by(ISO_NODEI)%>%
  summarise(total=sum(MIGIJ_F_est+MIGIJ_M_est,na.rm = T),
            females=sum(MIGIJ_F_est,na.rm = T),
            males=sum(MIGIJ_M_est,na.rm = T),
            females_perc=females/total,
            males_perc=males/total)%>%
  mutate(females_perc=ifelse(is.infinite(females_perc),0,females_perc),
         males_perc=ifelse(is.infinite(males_perc),0,males_perc))%>%
  select(ISO_NODEI,total,females,males,females_perc,males_perc)%>%
  rename("ISO_NODE"="ISO_NODEI")

# Immigratoin movements per admin unit ####
IM_data=table_gender_mig%>%
  mutate(ISO_NODEJ=paste(ISOI,as.integer(NODEJ),sep="_"),
         ISO_NODEI=paste(ISOI,as.integer(NODEI),sep="_"))%>%
  group_by(ISO_NODEJ)%>%
  summarise(total=sum(MIGIJ_F_est+MIGIJ_M_est,na.rm = T),
            females=sum(MIGIJ_F_est,na.rm = T),
            males=sum(MIGIJ_M_est,na.rm = T),
            females_perc=females/total,
            males_perc=males/total)%>%
  mutate(females_perc=ifelse(is.infinite(females_perc),0,females_perc),
         males_perc=ifelse(is.infinite(males_perc),0,males_perc))%>%
  select(ISO_NODEJ,total,females,males,females_perc,males_perc)%>%
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
         net_IM_males=males-males_EM)%>%
  select(ISO_NODE,net_IM,net_IM_females,net_IM_males)

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
          net_EM_males=males_EM-males)%>%
  select(ISO_NODE,net_EM,net_EM_females,net_EM_males)

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
         EM_IM_males=males+males_EM)%>%
  select(ISO_NODE,EM_IM,EM_IM_females,EM_IM_males)

# add the tables in a SQLITE database ####
mig_db <- src_sqlite("table/mig_db.sqlite3",
                     create = T)

copy_to(mig_db,
        EM_data,
        name="EM",
        temporary = FALSE,
        indexes = list("ISO_NODE"),
        overwrite = T)

copy_to(mig_db,
        IM_data,
        name="IM",
        temporary = FALSE,
        indexes = list("ISO_NODE"),
        overwrite = T)

copy_to(mig_db,
        Net_IM,
        name = "Net_IM",
        temporary = FALSE,
        indexes = list("ISO_NODE"),
        overwrite = T)

copy_to(mig_db,
        Net_EM,
        name = "Net_EM",
        temporary = FALSE,
        indexes = list("ISO_NODE"),
        overwrite = T)

copy_to(mig_db,
        EM_IM,
        name = "EM_IM",
        temporary = FALSE,
        indexes = list("ISO_NODE"),
        overwrite = T)

# store table_gender_mig in the database ####
table_gender_mig_s=table_gender_mig%>%
  mutate(ISO_NODEJ=paste(ISOI,as.integer(NODEJ),sep="_"),
         ISO_NODEI=paste(ISOI,as.integer(NODEI),sep="_"))%>%
  select(MIGIJ_F_est,MIGIJ_M_est,ISO_NODEI,ISO_NODEJ,CONTI,ISOI)

copy_to(mig_db,
        table_gender_mig_s,
        name = "gender_mig",
        temporary = FALSE,
        indexes = list("ISO_NODEI","ISO_NODEJ","CONTI","ISOI"),
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
