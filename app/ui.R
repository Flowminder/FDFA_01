###################
# ui #
###################
ui <- shinyUI(navbarPage("Gender-disaggregated Migration Movements",
                         
                         tabPanel("Map",
                                  fluidPage(
                                    tags$head(tags$style(
                                      HTML('
                                           #PANEL {background-color: rgba(190,190,190,0.5);}')
                                    )),
                                    column(12,
                                           leafletOutput("map",width="100%",height="1200px"),
                                           
                                           absolutePanel(top = 50, right = 15,
                                                         draggable = TRUE,
                                                         class = "panel panel-default",
                                                         id="PANEL",
                                                         
                                                         selectInput("movements", 
                                                                     label = "Movements",
                                                                     choices = c("Total migration"="total_migration",
                                                                                 "Internal Emigration"="emigration",
                                                                                 "Internal Immigration"="immigration",
                                                                                 "Net immigration"="net_immigration"),
                                                                     selected = "PERF"),
                                                         
                                                         selectInput("gender", 
                                                                     label = "Gender",
                                                                     choices = c("Total (females and males)"="total",
                                                                                 "Females"="females",
                                                                                 "Males"="males",
                                                                                 "Females as percent of total"="females_perc",
                                                                                 "Males as percent of total"="males_perc"),
                                                                     selected = "total"),
                                                         
                                                         selectInput("metric", 
                                                                     label="Metric",
                                                                     choices = c("Number of people"="number",
                                                                                 "Proportion of population"="prop_pop"),
                                                                     selected = "number"),
                                                         
                                                         
                                                         checkboxInput("destination",
                                                                       "Destination",
                                                                       value = F),
                                                         
                                                         selectInput("direction", 
                                                                     label = "Direction",
                                                                     choices = c("Origin"="origin",
                                                                                 "Destination"="destination",
                                                                                 "Net departure"="net_departure",
                                                                                 "Net arrival"="net_arrival"),
                                                                     selected = "origin")
                                           )
                                           
                                    )
                                  )
                         )
)
)

