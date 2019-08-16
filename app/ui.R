###################
# ui #
###################

# UI components ####

panel_style=tags$head(tags$style(HTML('#PANEL {background-color: rgba(190,190,190,0.5);}')))

# Tab 1
leaflet_map1=leafletOutput("map1",width="100%",height="800px")

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
title_map2_html=htmlOutput("title_map2", align="centre")
leaflet_map2=leafletOutput("map2",width="100%",height="800px")

dropdown_direction2=selectInput("direction2", 
                                label = "Direction",
                                choices = c("Emigration"="emigration",
                                            "Immigration"="immigration"),
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

global_figures=htmlOutput("global_figures", align="left")
global_female_pie=plotlyOutput("global_female_pie")

top_10_total_bar=plotlyOutput("top_10_total_bar")
checkbox_top_10_total_perc=checkboxInput("top_10_total_perc",
                                         "migrants as % of population",
                                         value = FALSE)

top_10_females_bar=plotlyOutput("top_10_females_bar")
top_10_females_perc=checkboxInput("top_10_females_perc",
                                  "females as % of migrant",
                                  value = FALSE)

country_summary=htmlOutput("country_summary", align="centre")
country_female_pie=plotlyOutput("country_female_pie")

top_10_country_bar=plotlyOutput("top_10_country_bar")
top_10_country_dropdown=selectInput("top_10_country_dropdown", 
                                    label = "",
                                    choices = c("Number"="number",
                                                "Proportion of destination population"="perc"),
                                    selected = "number")

top_10_country_EM_bar=plotlyOutput("top_10_country_EM_bar")
top_10_country_EM_dropdown=selectInput("top_10_country_EM_dropdown", 
                                       label = "",
                                       choices = c("Number"="number",
                                                   "Proportion of origin population"="perc"),
                                       selected = "number")

dropdown_gender3=selectInput("gender3", 
                             label = "",
                             choices = c("Total (females and males)"="total",
                                         "Females"="females",
                                         "Males"="males"),
                             selected = "total")
dropdown_gender4=selectInput("gender4", 
                             label = "",
                             choices = c("Total (females and males)"="total",
                                         "Females"="females",
                                         "Males"="males"),
                             selected = "total")

dropdown_gender5=selectInput("gender5", 
                             label = "",
                             choices = c("Total (females and males)"="total",
                                         "Females"="females",
                                         "Males"="males"),
                             selected = "total")


chorddiagOutput_1=chorddiagOutput("chorddiagOutput_1",height = "800px")


title_map3_html=htmlOutput("title_map3", align="centre")

leaflet_map3=leafletOutput("map3",width="100%",height="400px")

render_treemap1 =  plotOutput("Treemap", width = "80%", height = "700px")


# UI structure ####

ui <- shinyUI(navbarPage("Gender-disaggregated Migration Movements",
                         
                         fluidPage(
                           panel_style,
                           
                           fluidRow(
                             column(4,
                                    global_figures,
                                    global_female_pie),
                             column(4,
                                    top_10_total_bar,
                                    checkbox_top_10_total_perc),
                             column(4,
                                    top_10_females_bar,
                                    top_10_females_perc)
                           ),
                           fluidRow(
                             column(6,
                                    fluidRow(leaflet_map1,
                                             widget_panel1)
                                    ),
                             column(6,
                                    fluidRow(country_summary),
                                    fluidRow(country_female_pie),
                                    tabBox(
                                      title = "Top regions ranked by:", width = NULL,
                                      
                                      tabPanel("Immigration",
                                               fluidRow(top_10_country_bar),
                                               fluidRow(column(6,top_10_country_dropdown),
                                                        column(6,dropdown_gender3))
                                      ),
                                      tabPanel("Emigration",
                                               fluidRow(top_10_country_EM_bar,
                                                        top_10_country_EM_dropdown,
                                                        dropdown_gender4))
                                    )
                             )
                           ),
                           fluidRow(
                             column(6,
                                    fluidRow(
                                      title_map2_html,
                                             leaflet_map2,
                                             widget_panel2)
                                    ),
                             column(6,
                                    fluidRow(chorddiagOutput_1),
                                    fluidRow(dropdown_gender5))
                             ),
                           fluidRow(
                             column(6,
                                    fluidRow(
                                      title_map3_html,
                                             leaflet_map3)
                                    )),
                           fluidRow(
                             column(12,
                                    fluidRow(
                                      render_treemap1
                                    )
                           )
                              
                           
                           )
)
)
)

