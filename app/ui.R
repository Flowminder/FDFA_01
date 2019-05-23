###################
# ui #
###################

# UI components ####
leaflet_map=leafletOutput("map",width="100%",height="1200px")

dropdown_movement=selectInput("movement",
                              label = "Movements",
                              c("Total movements at admin unit"="movements",
                                "Origin-Destination from and to admin unit"="origin_destination"),
                              selected = "movements")

dropdown_direction=selectInput("direction", 
                               label = "Direction",
                               choices = c("Emigration"="emigration",
                                           "Immigration"="immigration",
                                           "Net immigration"="net_immigration",
                                           "Net Emigration"="net_emigration",
                                           "Total migration (in and out)"="total_migration"),
                               selected = "emigration")

dropdown_gender=selectInput("gender", 
                            label = "Gender",
                            choices = c("Total (females and males)"="total",
                                        "Females"="females",
                                        "Males"="males",
                                        "Females as percent of total"="females_perc",
                                        "Males as percent of total"="males_perc"),
                            selected = "total")

dropdown_basemap=selectInput("base_map", 
                             label = "Base map",
                             choices = c("Open street map"="OSM",
                                         "Population density"="pop_dens"),
                             selected = "OSM")


widget_panel=absolutePanel(top = 50, left = 15,
                           draggable = TRUE,
                           class = "panel panel-default",
                           id="PANEL",
                           dropdown_movement,
                           dropdown_direction,
                           dropdown_gender,
                           dropdown_basemap)

panel_style=tags$head(tags$style(HTML('#PANEL {background-color: rgba(190,190,190,0.5);}')))

# UI structure ####
ui <- shinyUI(navbarPage("Gender-disaggregated Migration Movements",
                         
                         tabPanel("Map",
                                  fluidPage(
                                    panel_style,
                                    column(12,
                                           leaflet_map,
                                           widget_panel
                                           
                                    )
                                  )
                         )
)
)

