url <- 'https://raw.githubusercontent.com/plotly/datasets/master/2011_february_aa_flight_paths.csv'
flights <- read.csv(url)
flights$id <- seq_len(nrow(flights))
flights$stroke <- sample(1:3, size = nrow(flights), replace = T)
flights$info <- paste0("<b>",flights$airport1, " - ", flights$airport2, "</b>")

mapdeck( style = mapdeck_style("dark"), pitch = 45 ) %>%
  add_arc(
    data = flights
    , layer_id = "arc_layer"
    , origin = c("start_lon", "start_lat")
    , destination = c("end_lon", "end_lat")
    , stroke_from = "airport1"
    , stroke_to = "airport2"
    , stroke_width = "stroke"
    , tooltip = "info"
    , auto_highlight = TRUE
    , legend = T
    , legend_options = list(
      stroke_from = list( title = "Origin airport" ),
      css = "max-height: 100px;")
  )
#> Registered S3 method overwritten by 'jsonlite':
#>   method     from   
#>   print.json jsonify

mapdeck( token = api_token, style = mapdeck_style("dark")) %>%
  add_arc(
    data = flights
    , layer_id = "arc_layer"
    , origin = c("start_lon", "start_lat")
    , destination = c("end_lon", "end_lat")
    , stroke_from = "airport1"
    , stroke_to = "airport2"
    , stroke_width = "stroke"
  )

## Using a 2-sfc-column sf object
library(sf)
#> Linking to GEOS 3.7.2, GDAL 2.4.2, PROJ 5.2.0

sf_flights <- cbind(
  sf::st_as_sf(flights, coords = c("start_lon", "start_lat"))
  , sf::st_as_sf(flights[, c("end_lon","end_lat")], coords = c("end_lon", "end_lat"))
)

mapdeck(
) %>%
  add_arc(
    data = sf_flights
    , origin = 'geometry'
    , destination = 'geometry.1'
    , layer_id = 'arcs'
    , stroke_from = "airport1"
    , stroke_to = "airport2"
  )

## using a brush

mapdeck(
  , style = mapdeck_style("light")
) %>%
  add_arc(
    data = sf_flights
    , origin = 'geometry'
    , destination = 'geometry.1'
    , layer_id = 'arcs'
    , stroke_from = "airport1"
    , stroke_to = "airport2"
    , stroke_width = 4
    , brush_radius = 500000
  )

