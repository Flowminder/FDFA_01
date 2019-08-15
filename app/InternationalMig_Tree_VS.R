library(dplyr)
library(rgdal)
library(leaflet)
library(sp)
library(shiny)
library(chorddiag)  
library(treemap)

#int_mig_sub=read.csv("C:/Users/Xavier Vollenweider/Dropbox/FDFA_01/data/table/combined_df_20180122.csv")  #*****
int_mig_sub=read.csv("/Users/vishva/Dropbox/FDFA_01/data/table/combined_df_20180122.csv")

#admin_units=readOGR("C:/Users/Xavier Vollenweider/Dropbox/FDFA_01/data/spatial/AdminUnits_simplified/AdminUnits_simplified.geojson")  #******
admin_units=readOGR("/Users/vishva/Dropbox/FDFA_01/data/spatial/All_AdminUnits_final_simplified/all_admin_simplified.geojson")

# ui #####

ui <- shinyUI(navbarPage("Inward Migration Movements",
                          fluidPage(
                            leafletOutput("map3",width="100%",height="800px"),
                            plotOutput("Treemap", width = "80%", height = "700px")
                          )
)
)

# server #####
server <- shinyServer(function(input, output, session){
  
  # Identifying the clicked rows
  layerID_clicked3=reactive({
    event=input$map3_shape_click
    
    if(is.null(event)){
      event<-data.frame("id"=101)
    }
    
    layerID_clicked3=event$id
    
    return(layerID_clicked3)
    
  })
  
  # origin countries for clicked admin (on a district level). Identifying from migration df and merging with shapefile
  admin_data_selected=reactive({
    
    ISO_NODE_s=admin_units@data$ISO_NODE[layerID_clicked3()] # clicked ISO_NODE
    
    int_mig_sub_NODE=int_mig_sub%>% # migration data for the clicked ISO_NODE
      mutate(ISO_NODEJ=paste0(ISOJ,"_",NODEJ))%>%
      filter(ISO_NODEJ==ISO_NODE_s) %>%
      mutate(ISO_NODE = paste0(ISOI,"_",NODEI)) %>%
      select(ISO_NODEJ,ISO_NODE, INTMIGIJ) %>%
      arrange(desc(INTMIGIJ)) %>%
      collect()
    
    admin_data_selected=admin_units
    
    
    admin_data_selected@data=admin_data_selected@data%>%    #merging the specific region to the aggregate shape file 
      mutate(ISO_NODE=as.character(ISO_NODE))%>%
      left_join(int_mig_sub_NODE,
                by=c("ISO_NODE"))
    
    return(admin_data_selected)
    
  })
  
# selected polygon #### subsetting the clicke node 
  Selected_Node=reactive({
    ISO_NODE_s=admin_units@data$ISO_NODE[layerID_clicked3()]
    Selected_Node=subset(admin_units,
                         admin_units@data$ISO_NODE==ISO_NODE_s)
    return(Selected_Node)
  })
  
# Categorizing top (non-zero) origin Countries to clicked admin for tree map  ####
  
int_mig_sub_Tree = reactive({
  
  layerID_clicked3=layerID_clicked3()
  
  ISO_NODE_s = admin_units@data$ISO_NODE[layerID_clicked3]  # clicked admin
  
  int_mig_sub_Tree=int_mig_sub %>%   
    mutate(ISO_NODEJ=paste0(ISOJ,"_",NODEJ))%>%
    filter(ISO_NODEJ==ISO_NODE_s) %>%
    mutate(ISO_NODE = paste0(ISOI,"_",NODEI)) %>%
    group_by(ISOI) %>%
    summarise(INTMIGIJ=mean(INTMIGIJ)) %>%
    filter( INTMIGIJ > 0) %>%
    mutate(ISO_NodeJ = ISO_NODE_s) %>%
    select(ISO_NodeJ,ISOI, INTMIGIJ ) %>%
    collect()
  
  int_mig_sub_Tree$Origin_Name = countrycode(int_mig_sub_Tree$ISOI,"iso3c","country.name")
  
  int_mig_sub_Tree=as.data.frame(int_mig_sub_Tree)
  
    return(int_mig_sub_Tree)
})

  # map 3 base ####
  
  output$map3<-renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng=0,lat=0,zoom = 2)
  })
  
  # map 3 #####
  
  observe({
    
    print(admin_units@data$ISO_NODE[layerID_clicked3()])
    
    Selected_Node_dta=Selected_Node()
    admin_data_selected_dta=admin_data_selected()
    
    
    mypalette = colorNumeric(palette="Greens", domain=admin_units@data$INTMIGIJ, na.color="transparent")
    Hovertext=paste("ISO_Node: ", admin_units@data$ISO_NODE,"<br/>", 
                    "Outward Migration to Dest: ",admin_units@data$INTMIGIJ, sep="") %>%
      lapply(htmltools::HTML)  
    
    
    
    leafletProxy('map3')%>%
      clearShapes()  %>%
      addPolygons(data=admin_data_selected_dta,
                  stroke = F,
                  fillOpacity = 0.5,
                  fillColor = ~mypalette(admin_data_selected_dta@data$INTMIGIJ), 
                  label = Hovertext,
                  layerId = ~1:dim(admin_data_selected_dta)[1]) %>%
      addPolygons(data=Selected_Node_dta,
                  stroke = T,
                  fillOpacity = 1,
                  fillColor = "red")
    
  })
  
output$Treemap = renderPlot({
  
  int_mig_sub_Tree_collected=int_mig_sub_Tree()
  plot_t=treemap(int_mig_sub_Tree_collected,
          index="Origin_Name",
          vSize = "INTMIGIJ",
          position.legend = "right",
          fontsize.labels = 16)
  
  return(plot_t)
})


})
shinyApp(ui, server)

