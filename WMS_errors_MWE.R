
# Loading EMODnet WMS Tiles works -----------------------------------------

library(leaflet)
library(dplyr)


leaflet::leaflet() %>% 
  addTiles() %>%
  addWMSTiles(
    baseUrl = "https://geo.vliz.be/geoserver/MarineRegions/wms",
    layers = "eez",   # replace with your layer name
    options = WMSTileOptions(
      format = "image/png",
      styles = "Polygons_greyoutline",
      transparent = T
    )
  )


# Loading VLIZ geoserver WMS Tiles does not work --------------------------


library(leaflet)
library(dplyr)


leaflet::leaflet() %>% 
  setView(3, 51.5, zoom = 8) %>%
  addTiles() %>%
  addWMSTiles(
    baseUrl = "https://ows.emodnet-humanactivities.eu/wms",
    layers = "windfarmspoly",
    options = WMSTileOptions(
      format = "image/png",
      transparent = T
    )
  )

# Loading RBINS geoserver WMS Tiles works --------------------------


library(leaflet)
library(dplyr)


leaflet::leaflet() %>% 
  setView(3, 51.5, zoom = 8) %>%
  addTiles() %>%
  addWMSTiles(
    baseUrl = "https://spatial.naturalsciences.be/geoserver/od_nature/wms",
    layers = "	od_nature:Habitat_BPNS_Seafloor",
    options = WMSTileOptions(
      format = "image/png",
      transparent = T
    )
  )
