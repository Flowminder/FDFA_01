#load the data ####
admin_poly=rgdal::readOGR("spatial/All_AdminUnits_final_simplified/all_admin_simplified.geojson")

# country poly: all ####

country_poly=rmapshaper::ms_dissolve(admin_poly,
                                       field="ISO")
rgdal::writeOGR(country_poly,
                "spatial/All_AdminUnits_final_simplified/country_poly.geojson",
                driver = "GeoJSON",
                layer=1)

# country poly: modeled ####
modelled_countries=c(unlist(t(gender_mig%>%
  distinct(ISOI)%>%
  collect())))

country_poly_modelled=country_poly
country_poly_modelled=country_poly_modelled[country_poly_modelled$ISO%in%modelled_countries,]

rgdal::writeOGR(country_poly_modelled,
                "spatial/All_AdminUnits_final_simplified/countries_poly_modelled.geojson",
                driver = "GeoJSON",
                layer=1)

# subset the admin which are modelled ####
admin_poly_modelled=admin_poly
admin_poly_modelled=admin_poly_modelled[admin_poly_modelled$ISO%in%modelled_countries,]

rgdal::writeOGR(admin_poly_modelled,
                "spatial/All_AdminUnits_final_simplified/admin_poly_modelled.geojson",
                driver = "GeoJSON",
                layer=1)
