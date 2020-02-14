summary(data_arc_collected)
data_arc
names(data_arc_collected)
data_arc%>%
  filter(COUNTRY_NAME_I=="Sao Tome and Principe")


STP


simple_countries_st=read_sf("data/simple_countries.shp") #"https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
simple_countries_st$id[simple_countries_st$id=="SSD"]="SUD"
simple_countries_st$id[simple_countries_st$id=="SDN"]="SUD"
simple_countries_st=subset(simple_countries_st,
                           id%in%c(as.character(unique(distinct_NODE$ISOI))))

simple_countries_ogr=readOGR("data/simple_countries.shp")
simple_countries_ogr$id=as.character(simple_countries_ogr$id)
simple_countries_ogr$id[simple_countries_ogr$id=="SSD"]="SUD"
simple_countries_ogr$id[simple_countries_ogr$id=="SDN"]="SUD"
simple_countries_ogr=subset(simple_countries_ogr,
                            id%in%c(as.character(unique(distinct_NODE$ISOI))))

simple_countries_df=as.data.frame(simple_countries_ogr)
simple_countries_df=simple_countries_df%>%
  select(id,name)%>%
  rename("ISO"="id")
simple_countries_df$lon=coordinates(simple_countries_ogr)[,1]
simple_countries_df$lat=coordinates(simple_countries_ogr)[,2]


topo_correct_ogr_df%>%
  filter(ISO=="STP")