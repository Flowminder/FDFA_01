###################
# ui #
###################

# UI components ####

panel_style=tags$head(tags$style(HTML('#PANEL {background-color: rgba(190,190,190,0.5);}')))

# Tab 1
leaflet_map1=leafletOutput("map1",width="100%",height="1200px")

dropdown_direction1=selectInput("direction1", 
                               label = "Direction",
                               choices = c("Emigration"="emigration",
                                           "Immigration"="immigration",
                                           "Net immigration"="net_immigration",
                                           "Net Emigration"="net_emigration",
                                           "Total migration (in and out)"="total_migration"),
                               selected = "emigration")

dropdown_gender1=selectInput("gender1", 
                            label = "Gender",
                            choices = c("Total (females and males)"="total",
                                        "Females"="females",
                                        "Males"="males",
                                        "Females as percent of total"="females_perc",
                                        "Males as percent of total"="males_perc"),
                            selected = "total")

dropdown_basemap1=selectInput("base_map1", 
                             label = "Base map",
                             choices = c("Open street map"="OSM",
                                         "Population density"="pop_dens"),
                             selected = "OSM")


widget_panel1=absolutePanel(top = 50, left = 15,
                           draggable = TRUE,
                           class = "panel panel-default",
                           id="PANEL",
                           dropdown_direction1,
                           dropdown_gender1,
                           dropdown_basemap1)

# Tab 2
leaflet_map2=leafletOutput("map2",width="100%",height="1200px")

dropdown_direction2=selectInput("direction2", 
                                label = "Direction",
                                choices = c("Emigration"="emigration",
                                            "Immigration"="immigration",
                                            "Net immigration"="net_immigration",
                                            "Net Emigration"="net_emigration",
                                            "Total migration (in and out)"="total_migration"),
                                selected = "emigration")

dropdown_gender2=selectInput("gender2", 
                             label = "Gender",
                             choices = c("Total (females and males)"="total",
                                         "Females"="females",
                                         "Males"="males",
                                         "Females as percent of total"="females_perc",
                                         "Males as percent of total"="males_perc"),
                             selected = "total")

dropdown_basemap2=selectInput("base_map2", 
                              label = "Base map",
                              choices = c("Open street map"="OSM",
                                          "Population density"="pop_dens"),
                              selected = "OSM")


widget_panel2=absolutePanel(top = 50, left = 15,
                             draggable = TRUE,
                             class = "panel panel-default",
                             id="PANEL",
                             dropdown_direction2,
                             dropdown_gender2,
                             dropdown_basemap2)

# UI structure ####
ui <- shinyUI(navbarPage("Gender-disaggregated Migration Movements",
                         
                         tabPanel("Map global",
                                  fluidPage(
                                    panel_style,
                                    column(12,
                                           leaflet_map1,
                                           widget_panel1
                                           
                                    )
                                  )
                         ),
                         tabPanel("Origin destination",
                                  fluidPage(
                                    panel_style,
                                    column(12,
                                           leaflet_map2,
                                           widget_panel2

                                    )
                                  )
                         )
)
)

