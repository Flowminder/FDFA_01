# function_chord
function_chord=function(mig_db,country_clicked,focus,type,direction,sex,n_slice,slice_chord){
  
  # null parameters
  if(is.null(country_clicked)){country_clicked="AFG"}
  # if(is.null(admin_clicked)){admin_clicked="13"}
  if(is.null(focus)){focus="global"}
  if(is.null(type)){type="int"}
  if(is.null(direction)){direction="X"}
  if(is.null(sex)){sex="F"}
  # if(is.null(aggregation)){aggregation="admin"}
  if(is.null(n_slice)){n_slice="3"}
  # if(is.null(top_n_per_country)){top_n_per_country=FALSE}
  if(is.null(slice_chord)){slice_chord=10}
  
  # filter and collect the data
  if(focus=="global"){
    chord_data_plot=chord_slicer_global(mig_db,direction,sex,n_slice,slice_chord)  
  }
  if(focus %in%c("country","admin")){
    chord_data_plot=chord_slicer_country(mig_db,country_clicked,type,direction,sex,n_slice,slice_chord)  
  }
  
  # plot
  chord=chorddiag(data = chord_data_plot,
                   groupnamePadding = 30,
                   groupPadding = 3,
                   groupnameFontsize = 13 ,
                   showTicks = FALSE,
                   margin=150,
                   tooltipGroupConnector = "    &#x25B6;    ",
                   chordedgeColor = "#B3B6B7"
  )
  return(chord)
  
}