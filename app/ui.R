library(jsonlite)
library(leaflet)
library(plotly)
library(shiny)
library(shinydashboard)


dropdown_mov=selectInput("movement",
                         label = "Movements",
                         c("Total movements at admin unit"="movements",
                           "Origin-Destination from and to admin unit"="origin_destination"),
                         selected = "movements")

dropdown_dir=selectInput("direction", 
                         label = "Direction",
                         choices = c("Emigration"="emigration",
                                     "Immigration"="immigration",
                                     "Net immigration"="net_immigration",
                                     "Net Emigration"="net_emigration",
                                     "Total migration (in and out)"="total_migration"),
                         selected = "emigration")

dropdown_gen=selectInput("gender", 
                         label = "Gender",
                         choices = c("Total (females and males)"="total",
                                     "Females"="females",
                                     "Males"="males",
                                     "Females as percent of total"="females_perc",
                                     "Males as percent of total"="males_perc"),
                         selected = "total")

dropdown_bas=selectInput("base_map", 
                         label = "Base map",
                         choices = c("Open street map"="OSM",
                                     "Population density"="pop_dens"),
                         selected = "OSM")

slider_transparency = sliderInput(
  "transparency",
  "Transparency",
  min=0,
  max=1,
  value = 1)

dashboard_map = box(
  title = "Map",
  width = NULL,
  solidHeader = TRUE,
  status = "primary",
  htmlOutput("movement_type", align="center"),
  leafletOutput("map")
)

dashboard_pie_gender = box(
  title = "Females",
  width = NULL,
  solidHeader = TRUE,
  status = "primary",
  plotlyOutput("gender_pie"))

dashboard_circle = box(
  title = "Origin-Destination Movements",
  width = NULL,
  solidHeader = TRUE,
  status = "primary",
  plotOutput("circle_od"))


dashboard_bar = box(
  title = "",
  width = NULL,
  solidHeader = TRUE,
  status = "primary",
  htmlOutput("od_bar_title", align="center"),
  plotlyOutput("od_bar"))

dashboard_header = dashboardHeader(title = "Gender-disaggregated Migration Movements v.0")
dashboard_sidebar = dashboardSidebar(dropdown_mov,
                                     dropdown_dir,
                                     dropdown_gen,
                                     dropdown_bas,
                                     slider_transparency)
dashboard_body = dashboardBody(
  fluidRow(column(width=8, 
                  dashboard_map),
           column(width=4, 
                  dashboard_pie_gender)),
  fluidRow(column(width=7, 
                  dashboard_circle),
           column(width=5, 
                  dashboard_bar))
)

ui = dashboardPage(
  dashboard_header,
  dashboard_sidebar,
  dashboard_body)


