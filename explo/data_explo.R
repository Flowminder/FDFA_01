########################
# data inspection ####
########################

rm(list=ls())
library(dplyr)
library(RPostgreSQL)
library(leaflet)
library(plotly)
library(tidyr)
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


# get total number of migrants per gender ####
IM =tbl(mig_db, "IM")
nick_mig =tbl(mig_db, "nick_mig")

# global
IM%>%
  summarise(sum_total=sum(total,na.rm = T),
            sum_females=sum(females,na.rm = T))%>%
  collect()%>%
  mutate(females_perc=sum_females/sum_total)%>%
  select(sum_total,females_perc)
  
IM%>%
  group_by(ISO)%>%
  summarise(sum_total=sum(total,na.rm = T))%>%
  arrange(desc(sum_total))%>%
  collect(n=10)%>%
  mutate(ISO_f=factor(1:10,
                      labels=ISO))%>%
  plot_ly(x=~ISO_f,
          y=~sum_total,
          type="bar")

Top_perc_mig_ISO=tbl(mig_db, "Top_perc_mig_ISO")

Top_perc_mig_ISO%>%
  collect()%>%
  mutate(ISO_f=factor(1:10, labels = ISO))%>%
  plot_ly(x=~ISO_f,
          y=~migrant_per_pop,
          type="bar")

# national
IM%>%
  filter(ISO=="ARG")%>%
  summarise(sum_females=sum(females,na.rm = T),
            sum_total=sum(total,na.rm = T))%>%
  collect()%>%
  mutate(females_perc=sum_females/sum_total)%>%
  select(sum_total,females_perc)%>%
  gather(key=key)
  
IM%>%
  filter(ISO=="ARG")%>%
  arrange(desc(total))%>%
  collect(n=10)%>%
  mutate(ISO_NODE_f=factor(1:10,
                      labels=ISO_NODE))%>%
  plot_ly(x=~ISO_NODE_f,
          y=~total,
          type="bar")

IM%>%
  filter(ISO=="ARG")%>%
  left_join(nick_mig%>%
              filter(ISOI=="ARG")%>%
              select(ISO_NODEI, POPI)%>%
              group_by(ISO_NODEI)%>%
              summarise(POP=mean(POPI,na.rm=T))%>%
              rename("ISO_NODE"="ISO_NODEI"),
            by="ISO_NODE")%>%
  collect()%>%
  mutate(prop_mig=total/POP)%>%
  arrange(desc(prop_mig))%>%
  top_n(10,prop_mig)%>%
  mutate(ISO_NODE_f=factor(1:10,
                           labels=ISO_NODE))%>%
  plot_ly(x=~ISO_NODE_f,
          y=~prop_mig,
          type="bar")



