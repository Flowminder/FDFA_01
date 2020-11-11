library(shiny)
library(dplyr)
library(rgdal)
library(purrr)
library(tidyverse)
library(tictoc)
library(mapdeck)
library(RSQLite)
library(shinycssloaders)
library(igraph)
library(tidygraph)
library(chorddiag)
library(plotly)
library(shinydashboard)
library(sf)
library(shinyhelper)
library(magrittr)
# rm(list=ls())

# set the directories ####
# setwd("C:/Users/Xavier Vollenweider/Documents/Flowminder/Migration_FDFA/code/FDFA_01_dev_v3/")
api_token="insert_your_token"

# load the data ####
sqlite = dbDriver("SQLite")
# dbDisconnect(mig_db) 
mig_db  = dbConnect(sqlite,"data/mig_db.sqlite3") # 
# db_list_tables(mig_db)

topo_correct_st = st_read("data/AdminUnits_TopoCorrect_light.shp")
topo_correct_ogr = readOGR("data/AdminUnits_TopoCorrect_light.shp")
topo_correct_ogr_df=topo_correct_ogr@data
topo_correct_ogr_df$lon=coordinates(topo_correct_ogr)[,1]
topo_correct_ogr_df$lat=coordinates(topo_correct_ogr)[,2]

distinct_NODE=read.csv("data/distinct_NODE.csv")


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

simple_countries_df$ISO[grep("Papua",simple_countries_df$name)]


# function ####
source("function/arc/arc_slicer.R")
source("function/arc/mapdeck_arc.R")

source("function/col/col_slicer_1.R")
source("function/col/col_slicer_2_admin.R")
source("function/col/col_slicer_2_country.R")
source("function/col/mapdeck_col.R")

source("function/poly/mapdeck_poly.R")

source("function/text/deck_caption_function.R")

source("function/chord/chord_slicer_global.R")
source("function/chord/chord_slicer_country.R")
source("function/chord/function_chord.R")
source("function/text/chord_caption_function.R")

source("function/bar/function_bar.R")

source("function/text/global_fig_function_topline.R")
source("function/text/nat_fig_function_topline.R")
source("function/text/admin_fig_function_topline.R")

source("function/text/format_n.R")
source("function/text/info_box/country_text.R")
source("function/text/info_box/admin_text.R")

source("function/text/info_box/global_int_X_txt.R")
source("function/text/info_box/nat_int_X_txt.R")
source("function/text/info_box/admin_int_X_txt.R")
source("function/text/info_box/global_prop_label_int_X.R")
source("function/text/info_box/nat_prop_label_int_X.R")
source("function/text/info_box/admin_prop_label_int_X.R")

source("function/text/info_box/nat_int_M_txt.R")
source("function/text/info_box/admin_int_M_txt.R")
source("function/text/info_box/nat_prop_label_int_M.R")
source("function/text/info_box/admin_prop_label_int_M.R")

source("function/text/info_box/global_nat_pop_label.R")
source("function/text/info_box/global_nat_X_txt.R")
source("function/text/info_box/nat_nat_prop_label_X.R")
source("function/text/info_box/nat_nat_X_text.R")


source("function/text/info_box/admin_nat_prop_label_X.R")
source("function/text/info_box/admin_nat_X_text.R")

source("function/text/info_box/admin_nat_prop_label_M.R")
source("function/text/info_box/admin_nat_M_text.R")



# ui component ####
mapdeck_output=mapdeckOutput("deck",width="100%",height=400) #"600px"

chord=chorddiagOutput("chord1",height = "800px") #

chord_caption=htmlOutput("chord_caption",
                         height="80px",
                         align="center")
top_line_figures=htmlOutput("top_line_figures",
                            height="100%")
bar_plot_int_M_text=htmlOutput("bar_plot_int_M_text",
                               height="400px")
bar_plot_int_X_text=htmlOutput("bar_plot_int_X_text",
                               height="400px")

bar_plot_int_X=plotlyOutput("bar_plot_int_X",
                            height="400px")
bar_plot_int_M=plotlyOutput("bar_plot_int_M",
                            height="400px")


# UI structure ####
ui =dashboardPage(
  dashboardHeader(title = "Five Year Sex-disaggregated Migration Movements",titleWidth=500),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Map", tabName = "map"),
                menuItem("Chord diagram", tabName = "chord"),
                menuItem("Background & Methods", tabName = "method"),
                
                uiOutput("focus_dynamic"),
                uiOutput("sex_dynamic"),
                uiOutput("type_dynamic"),
                uiOutput("direction_dynamic"),
                uiOutput("aggregation_dynamic"),
                uiOutput("top_n_per_country_dynamic")
                
    )),
  
  dashboardBody(
    # tags$style(type = "text/css", "#deck {height: calc(100vh - 120px) !important;}"),
    tabItems(
      tabItem("map"
              ,includeScript("function/js/check.js")
              ,uiOutput("browser_check")
              
              ,fluidRow(infoBoxOutput("SelectionBox",width=3)
                        ,valueBoxOutput("ValueBox_Int_X",width=2)
                        ,uiOutput("ValueBox_Int_M")
                        ,valueBoxOutput("ValueBox_Nat_X",width=2)
                        ,uiOutput("ValueBox_Nat_M")
              )
              ,fluidRow(
                column(width=7
                       ,box(height = 500
                            ,width = 12
                            ,mapdeck_output%>% withSpinner(color="#0dc5c1")
                            ,fluidRow(
                              column(5,uiOutput("graph_type_dynamic"))
                              ,column(3,uiOutput("scale_dynamic"))
                              ,column(4,uiOutput("slice_n_dynamic")))
                       )
                )
                ,column(width=5
                        ,tabBox(height = 500
                                ,width = 12
                                ,id="tabBox_bar_int"
                                ,tabPanel("Emigration",
                                          bar_plot_int_X
                                )
                                ,tabPanel("Immigration",
                                          bar_plot_int_M
                                )
                        )
                )
              )
              
              
      )
      ,tabItem("chord"
               ,fluidRow(
                 column(2
                        ,uiOutput("slice_chord_dynamic"))
                 ,column(10
                         ,chord_caption%>% withSpinner(color="#0dc5c1",
                                                       proxy.height="50px")%>%
                           helper(icon = "info",
                                  colour = "blue",
                                  type = "markdown",
                                  content = "Chord",
                                  style="icon-align: left;")
                         ,chord%>% withSpinner(color="#0dc5c1",
                                               proxy.height="50px")
                 )
               )
      )
      ,tabItem("method",
               tabsetPanel(type = "tabs",
                           tabPanel("Overview", includeMarkdown("background_methods_tab/background_methods_tab.md")), #
                           tabPanel("Internal migration",  includeMarkdown("background_methods_tab/internal.md")),
                           tabPanel("International migration", includeMarkdown("background_methods_tab/international.md")), #
                           tabPanel("GIS covariates", includeMarkdown("background_methods_tab/GIS_covariates.md")) #
                           
               )
      )
    )
  )
)  

# Server ####
server <-  function(input, output) {
  
  # # Background tab ####
  # output$overview_tab=renderUI({
  #   includeHTML("background_methods_tab/background_methods_tab.html")
  # })
  # 
  # output$internal_tab=renderUI({
  #   includeHTML("background_methods_tab/internal.html")
  # })
  # output$international_tab=renderUI({
  #   includeHTML("background_methods_tab/international.html")
  # })
  # 
  # output$GIS_covariates=renderUI({
  #   includeHTML("background_methods_tab/GIS_covariates.html")
  # })
  
  # country_admin_clicked_f ####
  country_admin_clicked_f=reactive({
    
    # coordinates
    poly_click=input$deck_polygon_click
    if(is.null(poly_click)){poly_click="{\"lon\":65.98695,\"lat\":33.54722}"}
    poly_click_js=jsonlite::fromJSON(poly_click)
    
    
    xy_clicked=data.frame(lon=poly_click_js$lon,
                          lat=poly_click_js$lat)

    coordinates(xy_clicked)=~lon+lat
    proj4string(xy_clicked)=proj4string(simple_countries_ogr)
    
    # country
    country_clicked_df=sp::over(xy_clicked,
                                topo_correct_ogr) 
    country_clicked=as.character(country_clicked_df$ISO)
    
    if(is.null(country_clicked)){
      country_clicked="AFG"  # at application launch
    }
    
    # admin
    admin_clicked_df=sp::over(xy_clicked,
                              topo_correct_ogr)
    admin_clicked=as.character(admin_clicked_df$NODE)
    
    if(is.null(admin_clicked)){
      admin_clicked="1"  # at application launch
    }
    
    # return
    country_admin_clicked=data.frame("country_clicked"=country_clicked,
                                     "admin_clicked"=admin_clicked,
                                     stringsAsFactors =FALSE)
    return(country_admin_clicked)
  })
  
  # deck#####
  output$deck=renderMapdeck({
    mapdeck(token = api_token,
            zoom=1,
            location = c(14.39697, 29.22912),
            pitch = 35,
            style = "mapbox://styles/mapbox/dark-v9",
            show_view_state=F)%>%
      mapdeck::add_polygon(data=topo_correct_st # poly base
                           # getPolygon = ~geometry,
                           , fill_opacity = 0
                           , auto_highlight=T
                           , highlight_colour= "#aaffff80"
                           , layer_id="poly_base"
                           , focus_layer=F
                           , update_view = F)
  })
  
  # observer for deck ####
  observeEvent(c(input$graph_type,input$focus,input$sex,input$direction,input$type,
                 input$top_n_per_country,
                 input$aggregation,input$scale,input$deck_polygon_click,input$n_slice,
                 input$tabs), { #
                   
                   country_admin_clicked=country_admin_clicked_f()
                   country_clicked=country_admin_clicked$country_clicked[1]
                   admin_clicked=country_admin_clicked$admin_clicked[1]
                   
                   graph_type=input$graph_type
                   if(is.null(graph_type)){graph_type="NULL"}
                   
                   focus=input$focus
                   if(is.null(focus)){focus="global"}
                   
                   direction=input$direction
                   if(is.null(direction)){direction="X"}
                   
                   type=input$type
                   if(is.null(type)){type="nat"}
                   
                   top_n_per_country=input$top_n_per_country
                   if(is.null(top_n_per_country)){top_n_per_country=F}
                   
                   aggregation=input$aggregation
                   if(is.null(aggregation)==T){aggregation="admin"}
                   
                   scale=input$scale
                   if(is.null(scale)){scale=2}
                   
                   if(any(graph_type%in%c("col","arc"))){
                     mapdeck_poly(mig_db,
                                  topo_correct_st,
                                  simple_countries_st,
                                  simple_countries_df,
                                  country_clicked,
                                  admin_clicked,
                                  focus,
                                  type,
                                  direction,
                                  input$sex,
                                  aggregation,
                                  input$n_slice,
                                  top_n_per_country)
                   }
                   
                   if(any(graph_type=="col")){
                     mapdeck_col(mig_db,
                                 simple_countries_df,
                                 country_clicked,
                                 admin_clicked,
                                 focus,
                                 type,
                                 direction,
                                 input$sex,
                                 aggregation,
                                 input$n_slice,
                                 scale)
                   }else{
                     mapdeck_update(map_id = "deck") %>%
                       clear_column(layer_id = "level_1")
                     mapdeck_update(map_id = "deck") %>%
                       clear_column(layer_id = "level_2")
                   }
                   
                   if(any(graph_type=="arc")){
                     mapdeck_arc(mig_db,
                                 country_clicked,
                                 admin_clicked,
                                 focus,
                                 type,
                                 direction,
                                 input$sex,
                                 aggregation,
                                 input$n_slice,
                                 top_n_per_country)
                   }else{
                     mapdeck_update(map_id = "deck") %>%
                       clear_arc(layer_id = "arc")
                   }
                   
                   if(focus=="global"){
                     location_view=c(14.39697, 29.22912)
                     zoom_view=1
                   }
                   if(focus=="country"){
                     lon_lat=simple_countries_df%>%
                       filter(ISO==country_clicked)%>%
                       select(lon,lat)%>%
                       distinct(lon,lat)
                     location_view=c(lon_lat$lon,
                                     lon_lat$lat)
                     zoom_view=3
                   }
                   
                   if(focus=="admin"){
                     lon_lat=topo_correct_ogr_df%>%
                       filter(ISO==country_clicked,
                              NODE==admin_clicked)%>%
                       select(lon,lat)%>%
                       distinct(lon,lat)
                     location_view=c(lon_lat$lon,
                                     lon_lat$lat)
                     zoom_view=4
                   }
                   
                   
                   mapdeck_update(map_id = "deck") %>%
                     add_title(title=list(title = deck_caption_function(mig_db,country_clicked,admin_clicked,
                                                                        graph_type,focus,type,direction,
                                                                        input$sex,aggregation,input$n_slice),
                                          css ="background-color: transparent;color: white; font-size: x-large;")
                     )%>%
                     mapdeck_view(location=location_view
                                  ,duration=500
                                  ,zoom = zoom_view
                                  ,transition="fly")
                   
                   
                 })
  
  # # chord ####
  inserted <- c()
  output$chord1=renderChorddiag({
    
    country_admin_clicked=country_admin_clicked_f()
    country_clicked=country_admin_clicked$country_clicked
    
    focus=input$focus
    
    type=input$type
    
    direction=input$direction
    
    sex=input$sex
    
    slice_chord=input$slice_chord
    
    
    function_chord(mig_db,country_clicked,focus,type,direction,sex,"50",slice_chord)
    
  })
  
  # chord_caption ####
  output$chord_caption=renderUI({
    
    country_admin_clicked=country_admin_clicked_f()
    country_clicked=country_admin_clicked$country_clicked
    
    focus=input$focus
    
    type=input$type
    
    direction=input$direction
    
    chord_caption_function(mig_db,
                           country_clicked,
                           focus,
                           type,direction,
                           input$sex,
                           input$n_slice)
    
  })
  
  
  # bar_plot_int_X ####
  output$bar_plot_int_X=renderPlotly({
    
    country_admin_clicked=country_admin_clicked_f()
    country_clicked=country_admin_clicked$country_clicked
    admin_clicked=country_admin_clicked$admin_clicked
    
    focus=input$focus
    if(is.null(focus)){focus="global"}
    
    direction="X"
    
    type=input$type
    
    n_slice=10
    
    top_n_per_country=input$top_n_per_country
    if(is.null(top_n_per_country)){top_n_per_country=F}
    
    aggregation=input$aggregation
    if(is.null(aggregation)==T){aggregation="country"}
    
    function_bar(mig_db,simple_countries_df,country_clicked,admin_clicked,focus,type,direction,input$sex,
                 aggregation,n_slice)
    
  })
  
  # bar_plot_int_M ####
  output$bar_plot_int_M=renderPlotly({
    
    country_admin_clicked=country_admin_clicked_f()
    country_clicked=country_admin_clicked$country_clicked
    admin_clicked=country_admin_clicked$admin_clicked
    
    focus=input$focus
    if(is.null(focus)){focus="global"}
    
    direction="M"
    
    type=input$type
    
    n_slice=10
    
    top_n_per_country=input$top_n_per_country
    if(is.null(top_n_per_country)){top_n_per_country=F}
    
    aggregation=input$aggregation
    if(is.null(aggregation)==T){aggregation="country"}
    
    function_bar(mig_db,simple_countries_df,country_clicked,admin_clicked,focus,type,direction,input$sex,
                 aggregation,n_slice)
    
  })
  
  #####
  
  # SelectionBox ####
  output$SelectionBox = renderInfoBox({
    
    country_admin_clicked=country_admin_clicked_f()
    country_clicked=country_admin_clicked$country_clicked
    admin_clicked=country_admin_clicked$admin_clicked
    
    focus=input$focus
    if(is.null(focus)){focus="global"}
    
    sex=input$sex
    if(is.null(sex))(sex="F")
    sex_label=switch(sex,
                     "F"="Females",
                     "M"="Males",
                     "all"="Females and Males")
    
    switch(focus,
           "global"=infoBox(title = tags$b("Global", style = "font-size: 200%;"),
                            color = "blue", 
                            subtitle = sex_label,
                            icon = icon("globe-americas", lib = "font-awesome")
           )
           ,"country"=infoBox(title = tags$b(country_text(mig_db,country_clicked), style = "font-size: 200%;"),
                              subtitle = sex_label,
                              color = "blue", 
                              icon = icon("map-marked-alt", lib = "font-awesome")
           )
           ,"admin"=infoBox(title = tags$b(admin_text(mig_db,country_clicked,admin_clicked), style = "font-size: 200%;"),
                            subtitle = tags$b(paste0("(",country_text(mig_db,country_clicked),")"), style = "font-size: 150%;"),
                            color = "blue", 
                            icon = icon("map-marker", lib = "font-awesome")
           )
    )
    
    
  })
  
  # ValueBox_Int_X####
  output$ValueBox_Int_X=renderValueBox({
    
    country_admin_clicked=country_admin_clicked_f()
    country_clicked=country_admin_clicked$country_clicked
    admin_clicked=country_admin_clicked$admin_clicked
    
    focus=input$focus
    if(is.null(focus)){focus="global"}
    
    sex=input$sex
    if(is.null(sex)){sex="F"}
    
    sex_label=switch(sex,
                     "F"="Female",
                     "M"="Male",
                     "all"="Female and male")
    
    switch(focus,
           "global"=valueBox(paste(sex_label,"international emigrants",
                                   global_prop_label_int_X(mig_db,sex)),
                             value=global_int_X_txt(mig_db,sex),
                             color = "orange", 
                             icon = icon("angle-right", lib = "font-awesome")
           )
           ,"country"=valueBox(paste(sex_label,"international emigrants",
                                     nat_prop_label_int_X(mig_db,country_clicked,sex)),
                               value=nat_int_X_txt(mig_db,country_clicked,sex),
                               color = "orange", 
                               icon = icon("angle-right", lib = "font-awesome")
           )
           ,"admin"=valueBox(paste(sex_label,"international emigrants",
                                   admin_prop_label_int_X(mig_db,simple_countries_df,country_clicked,admin_clicked,sex)),
                             value=admin_int_X_txt(mig_db,simple_countries_df,country_clicked,admin_clicked,sex),
                             color = "orange", 
                             icon = icon("angle-right", lib = "font-awesome")
           )
    )
    
    
  })
  
  # ValueBox_Int_M####
  output$ValueBox_Int_M=renderUI({
    
    country_admin_clicked=country_admin_clicked_f()
    country_clicked=country_admin_clicked$country_clicked
    admin_clicked=country_admin_clicked$admin_clicked
    
    focus=input$focus
    if(is.null(focus)){focus="global"}
    
    sex=input$sex
    if(is.null(sex)){sex="F"}
    
    sex_label=switch(sex,
                     "F"="Female",
                     "M"="Male",
                     "all"="Female and male")
    if(focus%in%c("country","admin")){
      switch(focus
             ,"country"=valueBox(paste(sex_label,"international immigrants",
                                       nat_prop_label_int_M(mig_db,country_clicked,sex)),
                                 value=nat_int_M_txt(mig_db,country_clicked,sex),
                                 color = "orange", 
                                 width=2,
                                 icon = icon("angle-left", lib = "font-awesome")
             )
             ,"admin"=valueBox(paste(sex_label,"international immigrants",
                                     admin_prop_label_int_M(mig_db,simple_countries_df,country_clicked,admin_clicked,sex)),
                               value=admin_int_M_txt(mig_db,simple_countries_df,country_clicked,admin_clicked,sex),
                               color = "orange", 
                               width=2,
                               icon = icon("angle-left", lib = "font-awesome")
             )
      )
    }
    
  })
  
  
  # ValueBox_Nat_X####
  output$ValueBox_Nat_X=renderValueBox({
    
    country_admin_clicked=country_admin_clicked_f()
    country_clicked=country_admin_clicked$country_clicked
    admin_clicked=country_admin_clicked$admin_clicked
    
    focus=input$focus
    if(is.null(focus)){focus="global"}
    
    sex=input$sex
    if(is.null(sex)){sex="F"}
    
    sex_label=switch(sex,
                     "F"="Female",
                     "M"="Male",
                     "all"="Female and male")
    
    switch(focus,
           "global"=valueBox(paste(sex_label,"internal emigrants",
                                   global_nat_pop_label(mig_db,sex)),
                             value=global_nat_X_txt(mig_db,sex),
                             color = "green", 
                             icon = icon("angle-right", lib = "font-awesome")
           )
           ,"country"=valueBox(paste(sex_label,"internal emigrants",
                                     nat_nat_prop_label_X(mig_db,country_clicked,sex)),
                               value=nat_nat_X_text(mig_db,country_clicked,sex),
                               color = "green", 
                               icon = icon("angle-right", lib = "font-awesome")
           )
           ,"admin"=valueBox(paste(sex_label,"internal emigrants",
                                   admin_nat_prop_label_X(mig_db,simple_countries_df,country_clicked,admin_clicked,sex)),
                             value=admin_nat_X_text(mig_db,simple_countries_df,country_clicked,admin_clicked,sex),
                             color = "green", 
                             icon = icon("angle-right", lib = "font-awesome")
           )
    )
    
    
  })
  
  # ValueBox_Nat_M####
  output$ValueBox_Nat_M=renderUI({
    
    country_admin_clicked=country_admin_clicked_f()
    country_clicked=country_admin_clicked$country_clicked
    admin_clicked=country_admin_clicked$admin_clicked
    
    focus=input$focus
    if(is.null(focus)){focus="global"}
    
    sex=input$sex
    if(is.null(sex)){sex="F"}
    
    sex_label=switch(sex,
                     "F"="Female",
                     "M"="Male",
                     "all"="Female and male")
    
    if(focus=="admin"){
      valueBox(paste(sex_label,"internal immigrants",
                     admin_nat_prop_label_M(mig_db,simple_countries_df,country_clicked,admin_clicked,sex)),
               value=admin_nat_M_text(mig_db,simple_countries_df,country_clicked,admin_clicked,sex),
               color = "green", 
               width=2,
               icon = icon("angle-left", lib = "font-awesome")
      )      
    }
  })
  
  
  #####
  # observe_helpers ####
  observe_helpers(withMathJax = TRUE)
  
  # top_n_per_country_dynamic ####
  output$top_n_per_country_dynamic = renderUI({
    
    focus=input$focus
    if(is.null(focus)){focus="global"}
    
    aggregation=input$aggregation
    if(is.null(aggregation)){aggregation="admin"}
    
    top_n_per_country=input$top_n_per_country
    if(is.null(top_n_per_country)){top_n_per_country=FALSE}
    
    graph_type=input$graph_type
    if(is.null(graph_type)){graph_type="col"}
    
    
    if(focus%in%c("global","country")&aggregation=="admin"&graph_type=="arc"&input$tabs=="map"){
      checkboxInput("top_n_per_country",
                    "Show only top connections per country",
                    value = top_n_per_country)
    }
    
  })
  
  # aggregation_dynamic ####
  output$aggregation_dynamic = renderUI({
    focus=input$focus
    if(is.null(focus)){focus="global"}
    
    aggregation=input$aggregation
    if(is.null(aggregation)){aggregation="admin"}
    
    type=input$type
    if(is.null(type)){type="int"}
    
    
    if(focus=="global" &input$tabs=="map"|focus=="country"&type=="int" &input$tabs=="map"){
      selectInput("aggregation", 
                  label = NULL,
                  choices = c("Sum for country"="country",
                              "Sum for admin"="admin"),
                  selected = aggregation)%>% 
        helper(icon = "info",
               colour = "white",
               type = "markdown",
               content = "Aggregation")
    }
  })
  
  # scale_dynamic ####
  output$scale_dynamic = renderUI({
    graph_type=input$graph_type
    if(is.null(graph_type)){graph_type="NULL"}
    
    if(any(graph_type=="col")){
      sliderInput("scale", 
                  label = "Column scale",
                  min=1,
                  max=50,
                  ticks=FALSE,
                  value = 10)
    }
  })
  
  # # focus_dynamic ####
  output$focus_dynamic = renderUI({
    focus=input$focus
    if(is.null(focus)){focus="global"}
    if(input$tabs=="map"){
      focus_dynamic=selectInput("focus",
                                label = NULL,
                                choices = c("All countries"="global",
                                            "One country"="country",
                                            "One administrative area"="admin"),
                                selected = focus)%>% 
        helper(icon = "info",
               colour = "white",
               type = "markdown",
               content = "Focus")
      return(focus_dynamic)
    }
    if(input$tabs=="chord"){
      focus_dynamic=selectInput("focus",
                                label = NULL,
                                choices = c("All countries"="global",
                                            "One country"="country"),
                                selected = focus)%>% 
        helper(icon = "info",
               colour = "white",
               type = "markdown",
               content = "Focus")
      return(focus_dynamic)
    }
    
    
  })
  
  # graph_type_dynamic ####
  output$graph_type_dynamic = renderUI({
    graph_type=input$graph_type
    
    if(input$tabs=="map"){
      checkboxGroupInput(inputId="graph_type",
                         label = "Visualisation type",
                         choices = c("Origin-destination arcs"="arc",
                                     "Column totals"="col"),
                         selected=graph_type)
    }
    
  })
  
  # direction_dynamic ####
  output$direction_dynamic = renderUI({
    
    direction=input$direction
    if(is.null(direction)){direction="X"}
    
    if(input$tabs=="map"){
      
      direction_dynamic=selectInput(inputId="direction",
                                    label = NULL,
                                    choices = c("Emigration"="X",
                                                "Immigration"="M"),
                                    selected=direction)%>% 
        helper(icon = "info",
               colour = "white",
               type = "markdown",
               content = "Direction")
      return(direction_dynamic)
    }
    if(input$tabs=="chord"){
      
      direction_dynamic=selectInput(inputId="direction",
                                    label = NULL,
                                    choices = c("Emigration"="X",
                                                "Immigration"="M"),
                                    selected=direction)%>% 
        helper(icon = "info",
               colour = "white",
               type = "markdown",
               content = "Direction")
      
      return(direction_dynamic)
    }
    
  })
  
  # type_dynamic ####
  output$type_dynamic = renderUI({
    type=input$type
    if(is.null(type)){type="nat"}
    focus=input$focus
    
    if(is.null(focus)){focus="country"}
    
    if(input$tabs=="map"){
      type_dynamic=selectInput(inputId="type",
                               label = NULL,
                               choices = c("Internal movements"="nat",
                                           "International movements"="int"),
                               selected=type)%>% 
        helper(icon = "info",
               colour = "white",
               type = "markdown",
               content = "Type")
      return(type_dynamic)
    }
    
    if(input$tabs=="chord"){
      
      if(focus=="admin"){
        type_dynamic=selectInput(inputId="type",
                                 label = NULL,
                                 choices = c("Internal movements"="nat",
                                             "International movements"="int"),
                                 selected=type)%>% 
          helper(icon = "info",
                 colour = "white",
                 type = "markdown",
                 content = "Type")
      }
      
      if(focus=="country"){
        type_dynamic=selectInput(inputId="type",
                                 label = NULL,
                                 choices = c("Internal movements"="nat",
                                             "International movements"="int"),
                                 selected=type)%>% 
          helper(icon = "info",
                 colour = "white",
                 type = "markdown",
                 content = "Type")
      }
      if(focus=="global"){
        type_dynamic=selectInput(inputId="type",
                                 label = NULL,
                                 choices = c("International movements"="int"),
                                 selected="int")%>% 
          helper(icon = "info",
                 colour = "white",
                 type = "markdown",
                 content = "Type")
      }
      return(type_dynamic)
    }
    
    
  })
  
  
  # slice_n_dynamic ####
  output$slice_n_dynamic = renderUI({
    n_slice=input$n_slice
    if(is.null(n_slice)){n_slice="3"}
    graph_type=input$graph_type
    if(is.null(graph_type)){graph_type="NULL"}
    focus=input$focus
    if(is.null(focus)){focus="global"}
    type=input$type
    if(is.null(type)){type="int"}
    
    if(graph_type=="arc"|graph_type=="col"&focus=="country"&type=="int"|focus=="admin"){
      SLICE=selectInput("n_slice", 
                        label = "Number of connexions",
                        choices = c("Top 1 connection"="1",
                                    "Top 3 connections"="3",
                                    "Top 10 connections"="10",
                                    "Top 20 connections"="20",
                                    "Top 50 connections"="50"),
                        selected = n_slice)%>% 
        helper(icon = "info",
               colour = "blue",
               type = "markdown",
               content = "Slice_n")
      return(SLICE)
    }
  })
  
  # slice_chord_dynamic ####
  output$slice_chord_dynamic = renderUI({
    slice_chord=input$slice_chord
    if(is.null(slice_chord)){slice_chord=10}
    sliderInput("slice_chord",
                "Maximum number of relations",
                min=1,
                max=50,
                value = slice_chord)
    
  })
  
  # sex_dynamic ####
  output$sex_dynamic = renderUI({
    
    sex=input$sex
    if(is.null(sex)){sex="F"}
    if(input$tabs!="method"){
      selectInput("sex", 
                  label = NULL,
                  choices = c("Female and Male"="all",
                              "Female"="F",
                              "Male"="M"),
                  selected = sex,
                  width='100%')%>% 
        helper(icon = "info",
               colour = "white",
               type = "markdown",
               content = "Sex")
    }
    
  })
  
  # browser_check ####
  # output$browser_check <- 
  output$browser_check = renderUI({
    
    check=input$check$data[[1]]
    if(is.null(check)){check=T}
    
    if(check==F){
      showModal(modalDialog(
        title = "This dashboard works best with the Google Chrome web browser",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
  })
  
  
}

shinyApp(ui, server)
