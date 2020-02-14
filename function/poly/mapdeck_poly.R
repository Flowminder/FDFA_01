mapdeck_poly=function(mig_db,topo_correct_st,simple_countries_st,simple_countries_df,country_clicked,admin_clicked,focus,type,direction,sex,aggregation,n_slice,top_n_per_country){
  
  # null parameters
  if(is.null(country_clicked)){country_clicked="AFG"}
  if(is.null(admin_clicked)){admin_clicked="13"}
  if(is.null(focus)){focus="global"}
  if(is.null(type)){type="int"}
  if(is.null(direction)){direction="X"}
  if(is.null(sex)){sex="F"}
  if(is.null(aggregation)){aggregation="admin"}
  if(is.null(n_slice)){n_slice="3"}
  if(is.null(top_n_per_country)){top_n_per_country=FALSE}
  
  aggregation_f=aggregation
  if(focus=="admin"){aggregation_f="admin"}
  if(is.null(focus)){focus="global"}
  if(is.null(aggregation_f)){aggregation_f="admin"}
  if(is.null(type)){type="nat"}
  if(is.null(sex)){sex="F"}
  
  # filter and collect data 1 ####
  data_col_1_collected=col_slicer_1(mig_db,simple_countries_df,country_clicked,admin_clicked,focus,type,direction,sex,aggregation_f)
  
  # filter and collect data 2 ####
  if(focus%in%c("country","admin")){
    if(aggregation_f=="admin"){
      data_col_2_collected=col_slicer_2_admin(mig_db,country_clicked,admin_clicked,focus,type,direction,sex,aggregation_f,n_slice)
    }
    if(aggregation_f=="country"&type=="int"){
      data_col_2_collected=col_slicer_2_country(mig_db,country_clicked,admin_clicked,focus,type,direction,sex,aggregation_f,n_slice)
    }
  }
  
  # polys
  if(focus=="admin"){
    topo_in_selected=unique(c(data_col_1_collected$JOIN_ID))     # selected admin in
    topo_in_selected_sub=subset(topo_correct_st,
                                as.character(topo_correct_st$JOIN_ID)%in%topo_in_selected)
    
    topo_in_all=unique(c(data_col_1_collected$ISO))               # non selected admin from in country
    topo_in_all_sub=subset(topo_correct_st,
                           as.character(topo_correct_st$ISO)%in%topo_in_all)
    
    topo_out=unique(c(data_col_2_collected$ID_group_by))        # selected admin from out country
    topo_out_sub=subset(topo_correct_st,
                        as.character(topo_correct_st$JOIN_ID)%in%topo_out)
  }
  
  if(focus=="country"){
    topo_in_selected=unique(c(data_col_1_collected$ISO))
    topo_in_selected_sub=subset(topo_correct_st,
                                as.character(topo_correct_st$ISO)%in%topo_in_selected)
    
    topo_out=unique(c(data_col_2_collected$ID_group_by))
    topo_out_sub=subset(topo_correct_st,
                        as.character(topo_correct_st$JOIN_ID)%in%topo_out)
  }
  
  if(aggregation_f=="country"&focus%in%c("admin","country")){
    topo_in=unique(c(data_col_1_collected$ISO))
    topo_in_sub=subset(simple_countries_st,
                       as.character(simple_countries_st$id)%in%topo_in)
    
    topo_out=unique(c(data_col_2_collected$ISO_group_by))
    topo_out_sub=subset(simple_countries_st,
                        as.character(simple_countries_st$id)%in%topo_out)
  }  
  # color
  color_in=switch(type,
                  "nat"=c("#00c3ff70"),
                  "int"=c("#f2e3c970"))
  color_out=switch(type,
                   "nat"=c("#6e71ca70"),
                   "int"=c("#badebd70"))
  # deck
  deck_object=mapdeck_update(map_id = "deck")
  
  if(nrow(data_col_1_collected)!=0){
    if(focus!="global"){
      
      deck_object=deck_object%>%
        add_polygon(data=topo_in_selected_sub # admin areas of selected IN
                    , stroke_opacity = 1
                    , stroke_colour = "#fcfcfc"
                    , stroke_width=3000
                    , fill_opacity = 0.5
                    , fill_colour = color_in
                    , auto_highlight=T
                    , layer_id="selected_admin_in"
                    , focus_layer=F
                    , update_view = F
                    , transitions = list(polygon = 700, fill_colour = 700, stroke_colour = 700, stroke_width = 700) )
      
      if(focus%in%c("admin","country")&type=="int" |
         focus=="admin"&type=="nat"){
        deck_object=deck_object%>%
          add_polygon(data=topo_out_sub # admin areas of selected OUT
                      , stroke_opacity = 1
                      , stroke_colour = "#fcfcfc"
                      , stroke_width=1000
                      , fill_opacity = 0.5
                      , fill_colour = color_out
                      , auto_highlight=T
                      , layer_id="selected_admin_out"
                      , focus_layer=F
                      , update_view = F
                      , transitions = list(polygon = 700, fill_colour = 700, stroke_colour = 700, stroke_width = 700))
      }else{
        deck_object=deck_object%>%
          clear_polygon(layer_id = "selected_admin_out")
      }
      
      if(focus=="admin"){
        deck_object=deck_object%>%
          add_polygon(data=topo_in_all_sub # admin areas of selected country not selected
                      , stroke_opacity = 0
                      , stroke_colour = color_out
                      , stroke_width=1000
                      , fill_opacity = 0
                      , auto_highlight=T
                      , layer_id="non_selected_admin_in"
                      , focus_layer=F
                      , update_view = F
                      , transitions = list(polygon = 700, fill_colour = 700, stroke_colour = 700, stroke_width = 700))
      }
      
    }
  }
  return(deck_object)
}
# focus="country"
# mapdeck_poly(mig_db,topo_correct_st,simple_countries_st,simple_countries_df,deck_proxy,country_clicked,admin_clicked,focus,type,direction,sex,aggregation,n_slice,top_n_per_country)