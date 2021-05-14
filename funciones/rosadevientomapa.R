rosadevientomapa <- function(coord, tb, ruta){
  
  pal_col <- tb %>% dplyr::pull(PAL_COL)
  int_col <- tb %>% dplyr::pull(INT_COL)
  
  lng1 = coord %>% dplyr::pull(LON1)
  lat1 = coord %>% dplyr::pull(LAT1)
  lng2 = coord %>% dplyr::pull(LON2)
  lat2 = coord %>% dplyr::pull(LAT2)

  leaflet(
    options = leafletOptions(
      zoomControl = F,
      minZoom = 16, 
      maxZoom = 17) # O solo especificar maxZoom
  ) %>%
    fitBounds(
      lng1 = lng1,
      lat1 = lat1,
      lng2 = lng2,
      lat2 = lat2
    ) %>% 
    addProviderTiles(providers$Esri.WorldImagery) %>%
    htmlwidgets::onRender(
      gsub("\n", "", 
           paste0(
             "function(el, x) {
             console.log(this);
             var myMap = this;
             var imageUrl = '",ruta,"';
             var imageBounds = [[", lat1, ", ", lng1, "], [", lat2, ",", lng2, "]];
             L.imageOverlay(imageUrl, imageBounds, {opacity:0.6, interactive: true}).addTo(myMap);}"
           )
      )
    ) %>%
    addLegend(
      position = "topleft",
      colors = rgb(t(col2rgb(pal_col))/255),
      labels = int_col,
      opacity = 0.6,
      title = "<center>Wind rose</br>(m/s)</center>"
    )

}