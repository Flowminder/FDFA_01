focus="global"
direction="X"
type="int"
sex_f=sex="F"
aggregation="country"
n_slice="1"
top_n_per_country=F
country_clicked=selected_country= "AFG" #"AFG"#
# selected_admin="SUD"
admin_clicked="1"
show_source_dest=T
slice_chord=10

deck_object=    mapdeck(token = api_token,
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



deck_object=deck_object%>%
  add_arc(data =data_arc_collected 
          , origin=c("x_from","y_from")
          , destination=c("x_to","y_to")
          , tooltip="label_arc"
          , stroke_from=color_arc_source
          , stroke_to=color_arc_target
          , stroke_width="width"
          , tilt="tilt"
          , layer_id="arc"
          , auto_highlight=TRUE
          , focus_layer=FALSE
          , update_view = FALSE
          , legend = legend_arc_js
          , transitions= list( origin = 700, destination = 700, stroke_from = 700, stroke_to = 700, stroke_width = 700)
  )

# deck object
deck_object=deck
deck_object=deck_object%>%
  add_column_layer( # column level 1 : IN
    data = data_col_1_collected,
    autoHighlight=TRUE,
    diskResolution = 10,
    radius = 20000,
    coverage=coverage,
    opacity=opacity,
    offset=offset,
    elevationScale=scale,
    getPosition = get_position("lat", "lon"),
    getElevation = get_property("move"),
    getFillColor = col_bar,
    extruded = TRUE,
    getTooltip=JS(label_1), 
    id="level_1",
    pickable=TRUE)

if(focus%in%c("admin","country")&type=="int" |
   focus=="admin"&type=="nat"){
  deck_object=deck_object%>%
    add_column_layer( # column level 2: out
      data = data_col_2_collected,
      autoHighlight=TRUE,
      diskResolution = 4,
      radius = 20000,
      coverage=coverage,
      opacity=opacity,
      offset=offset,
      elevationScale=scale,
      getPosition = get_position("lat", "lon"),
      getElevation = get_property("move"),
      getFillColor = col_bar,
      extruded = TRUE,
      legend = TRUE,
      getTooltip=JS(label_2), 
      id="level_2",
      pickable=TRUE)
}