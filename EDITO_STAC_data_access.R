##################################################################################
##################################################################################

# Author: Lotte Pohl
# Email: lotte.pohl@vliz.be
# Date: 2026-01-26
# Script Name: ~/DUC2_viewer_acoustic_telemetry/leaflet_env_data.R
# Script Description: load (meta)data from the EDITO STAC catalogue to be displayed on leaflet maps.
#                     Metada for all environmental data layers, data for acoustic telemetry data from the European Tracking Network (ETN)

##################################################################################
##################################################################################


# install and load libraries ----------------------------------------------

#install.packages("rstac")
#install.packages("mregions2")
library(rstac)
library(purrr)
library(arrow)
library(dplyr)
library(leaflet)


# overview --------------------------------------------------------------------

# etn data -> done
# marine spatial plan -> DONE
# seabed habitats -> not working yet
# shipwrecks -> not STAC but done
# OWF -> DONE
# EEZ -> not STAC but done
# MSFD and Natura2000 boundaries -> DONE
# sst -> TODO


# 0. make return df -------------------------------------------------------
# this dataframe will be loaded, it will contain all WMS links and STAC collection names of the 
# (meta)data accessed in this script.

wms_registry <- tibble(env_data_name = character(),
                             collection_name = character(),
                             wms_link = character(),
                             wms_base = character(),
                             wms_layer_name = character(),
                             legend_link = character(),
                             .added_at = as.POSIXct(character(), tz = "UTC"))

# 1. access the STAC catalogue and get data collections ----------------------------

stac_endpoint_url <- 'https://catalog.dive.edito.eu/'
api_endpoint_url <- "https://api.dive.edito.eu/data/collections/"
stac_obj <- rstac::stac(stac_endpoint_url)

stac_overview <- rstac::stac('https://catalog.dive.edito.eu/catalogs')%>% rstac::get_request()

# collections
c_obj <- rstac::collections(stac_obj) %>%
  rstac::get_request()

c_all <- c_obj$collections |> vapply(`[[`, character(1), "id") %>% as_tibble()



# 2. get data layer data and metadata -------------------------------------

## ETN dataset "PhD_Gossens" -----------------------------------------------
# this is the data that DUC4.2 is partially based upon. Here, the data will be 
# 1) queried in .parquet format, 2) summarised (to #detections and #individuals per day),
# and 3) saved in "./data/"

dataset_name <- "PhD_Goossens"
etn_collection <- "animal_tracking_datasets"

## etn data collection in EDITO STAC catalogue
etn_items <- stac_obj %>%
  rstac::stac_search(collections = etn_collection) %>%
  rstac::get_request()%>%
  rstac::items_fetch()

# Loop through the features and check if dataset_name is included in the title
for (feature in etn_items$features) {
  # Extract the title and href for the feature's asset
  title <- feature$assets$data$title
  href <- feature$assets$data$href
  
  # If the dataset_name is found in the title, extract the href
  if (grepl(dataset_name, title, fixed = TRUE)) {
    etn_dataset_href <- href
    break
  }
}

# Print the matching href
#print(etn_dataset_href)

etn_dataset <- arrow::read_parquet(etn_dataset_href, format = "parquet")
# start <- "2021-01-01" %>% as.POSIXct()
# end <- "2022-12-31" %>% as.POSIXct()

etn_daily_sum <- 
  etn_dataset %>% 
  #dplyr::filter(datetime %>% between(start, end)) %>%
  dplyr::mutate(date = datetime %>% as.Date()) %>%
  dplyr::group_by(date, station_name) %>%
  dplyr::summarise(latitude = mean(latitude, na.rm = T),
                   longitude = mean(longitude, na.rm = T),
                   n_detections = n(),
                   n_individuals = tag_serial_number %>% unique() %>% length())

# save as .RDS
etn_daily_sum_path <- "./data/etn_sum_seabass.rds"
saveRDS(etn_daily_sum, etn_daily_sum_path)

rm(etn_items, etn_dataset)

# appending row to wms_registry
wms_registry <- add_row(
  wms_registry,
  env_data_name = "seabass acoustic detections",
  collection_name = etn_collection,
  wms_link    = etn_dataset_href,
  wms_base    = etn_daily_sum_path,
  wms_layer_name  = "",
  legend_link = "",
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 


## Offshore Wind Farm -------------------------------------------------------

c_owf_list <- c_all %>% dplyr::filter(grepl("wind", c_all$value))
c_owf_selection <- c_owf_list$value[10]

owf_items <- stac_obj %>%
  rstac::stac_search(collections = c_owf_selection, limit = 500) %>%
  rstac::get_request() %>%
  rstac::items_fetch()

# wms
owf_wms_link <- owf_items$features[[1]]$assets$wms$href
owf_wms_base <- sub("\\?.*$", "", owf_wms_link)
owf_layer_name <- sub(".*LAYERS=([A-Za-z0-9_:]+).*", "\\1", owf_wms_link)


## legend
owf_legend_url <- paste0(
  owf_wms_base,
  "?SERVICE=WMS&REQUEST=GetLegendGraphic",
  "&FORMAT=image/png",
  "&LAYER=", owf_layer_name,
  "&VERSION=1.1.1"
)

# remove obj we no longer need
rm(c_owf_list, owf_items)

# # test map
# leaflet() %>%
#   setView(3, 51.5, zoom = 8) %>%
#   addTiles() %>%
#   addWMSTiles(
#     baseUrl = owf_wms_base,
#     layers  = owf_layer_name,
#     options = WMSTileOptions(
#       format = "image/png",
#       transparent = T,
#       opacity = 1
#     )) %>%
#   addControl(
#     html = paste0(
#       '<div style="background:white;padding:6px;border-radius:4px;">',
#       '<div style="font-weight:600;margin-bottom:4px;">Wind farms</div>',
#       '<img src="', owf_legend_url, '" />',
#       '</div>'
#     ),position = "bottomright"
#   )

# appending row to wms_registry
wms_registry <- add_row(
  wms_registry,
  env_data_name = "owf",
  collection_name = c_owf_selection,
  wms_link    = owf_wms_link,
  wms_base    = owf_wms_base,
  wms_layer_name  = owf_layer_name,
  legend_link = owf_legend_url,
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 


## marine spatial plan -----------------------------------------------------

c_msp_list <- c_all %>% dplyr::filter(grepl("spatial_planning", c_all$value))
c_msp_selection <- c_msp_list$value[1]

msp_items <- stac_obj %>%
  rstac::stac_search(collections = c_msp_selection, limit = 500) %>%
  rstac::get_request() %>%
  rstac::items_fetch()

# wms
msp_wms_link <- msp_items$features[[3]]$assets$wms$href
msp_wms_base <- sub("\\?.*$", "", msp_wms_link)
msp_layer_name <- sub(".*LAYERS=([A-Za-z0-9_:]+).*", "\\1", msp_wms_link)


## legend
msp_legend_url <- paste0(
  msp_wms_base,
  "?SERVICE=WMS&REQUEST=GetLegendGraphic",
  "&FORMAT=image/png",
  "&LAYER=", msp_layer_name,
  "&VERSION=1.1.1"
)

# browseURL(msp_legend_url)

rm(c_msp_list, msp_items)

# # test map
# leaflet() %>%
#   setView(3, 51.5, zoom = 8) %>%
#   addTiles() %>%
#   addWMSTiles(
#     baseUrl = msp_wms_base,
#     layers  = msp_layer_name,
#     options = WMSTileOptions(
#       format = "image/png",
#       transparent = T,
#       opacity = 1
#     )) %>%
#   addControl(
#     html = paste0(
#       '<div style="background:white;padding:6px;border-radius:4px;">',
#       '<div style="font-weight:600;margin-bottom:4px;">Spatial Planning Areas</div>',
#       '<img src="', msp_legend_url, '" />',
#       '</div>'
#     ),position = "bottomright"
#   )

# appending row to wms_registry

wms_registry <- add_row(
  wms_registry,
  env_data_name = "msp",
  collection_name = c_msp_selection,
  wms_link    = msp_wms_link,
  wms_base    = msp_wms_base,
  wms_layer_name  = msp_layer_name,
  legend_link = msp_legend_url,
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 

## Natura2000 areas -----------------------------------------------------

c_natura2000_list <- c_all %>% dplyr::filter(grepl("protected_areas", c_all$value))
c_natura2000_selection <- c_natura2000_list$value[1]

natura2000_items <- stac_obj %>%
  rstac::stac_search(collections = c_natura2000_selection, limit = 500) %>%
  rstac::get_request() %>%
  rstac::items_fetch()

# wms
natura2000_wms_link <- natura2000_items$features[[4]]$assets$wms$href
natura2000_wms_base <- sub("\\?.*$", "", natura2000_wms_link)
natura2000_layer_name <- sub(".*LAYERS=([A-Za-z0-9_:]+).*", "\\1", natura2000_wms_link)


## legend
natura2000_legend_url <- paste0(
  natura2000_wms_base,
  "?SERVICE=WMS&REQUEST=GetLegendGraphic",
  "&FORMAT=image/png",
  "&LAYER=", natura2000_layer_name,
  "&VERSION=1.1.1"
)

# browseURL(natura2000_legend_url)

rm(c_natura2000_list, natura2000_items)

# # test map
# leaflet() %>%
#   setView(3, 51.5, zoom = 8) %>%
#   addTiles() %>%
#   addWMSTiles(
#     baseUrl = natura2000_wms_base,
#     layers  = natura2000_layer_name,
#     options = WMSTileOptions(
#       format = "image/png",
#       transparent = T,
#       opacity = 1
#     )) %>%
#   addControl(
#     html = paste0(
#       '<div style="background:white;padding:6px;border-radius:4px;">',
#       '<div style="font-weight:600;margin-bottom:4px;">Natura2000 Areas</div>',
#       '<img src="', natura2000_legend_url, '" />',
#       '</div>'
#     ),position = "bottomright"
#   )

# appending row to wms_registry

wms_registry <- add_row(
  wms_registry,
  env_data_name = "natura2000",
  collection_name = c_natura2000_selection,
  wms_link    = natura2000_wms_link,
  wms_base    = natura2000_wms_base,
  wms_layer_name  = natura2000_layer_name,
  legend_link = natura2000_legend_url,
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 


## international sea conventions -------------------------------------------

sea_conventions_wms_link <- natura2000_items$features[[3]]$assets$wms$href
sea_conventions_wms_base <- sub("\\?.*$", "", sea_conventions_wms_link)
sea_conventions_layer_name <- sub(".*LAYERS=([A-Za-z0-9_:]+).*", "\\1", sea_conventions_wms_link)


## legend
sea_conventions_legend_url <- paste0(
  sea_conventions_wms_base,
  "?SERVICE=WMS&REQUEST=GetLegendGraphic",
  "&FORMAT=image/png",
  "&LAYER=", sea_conventions_layer_name,
  "&VERSION=1.1.1"
)

# browseURL(sea_conventions_legend_url)

rm(c_sea_conventions_list, sea_conventions_items)

# # test map
# leaflet() %>%
#   setView(3, 51.5, zoom = 8) %>%
#   addTiles() %>%
#   addWMSTiles(
#     baseUrl = sea_conventions_wms_base,
#     layers  = sea_conventions_layer_name,
#     options = WMSTileOptions(
#       format = "image/png",
#       transparent = T,
#       opacity = 1
#     )) %>%
#   addControl(
#     html = paste0(
#       '<div style="background:white;padding:6px;border-radius:4px;">',
#       '<div style="font-weight:600;margin-bottom:4px;">sea_conventions Areas</div>',
#       '<img src="', sea_conventions_legend_url, '" />',
#       '</div>'
#     ),position = "bottomright"
#   )

# appending row to wms_registry

wms_registry <- add_row(
  wms_registry,
  env_data_name = "sea_conventions",
  collection_name = c_natura2000_selection,
  wms_link    = sea_conventions_wms_link,
  wms_base    = sea_conventions_wms_base,
  wms_layer_name  = sea_conventions_layer_name,
  legend_link = sea_conventions_legend_url,
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 


## bathymetry --------------------------------------------------------------

c_bathy_list <- c_all %>% dplyr::filter(grepl("elevation", c_all$value))
c_bathy_selection <- c_bathy_list$value[1]

bathy_objects <- stac_obj %>%
  rstac::collections(c_bathy_selection)%>%
  rstac::get_request()

bathy_items <- stac_obj %>%
  rstac::stac_search(collections = c_bathy_selection) %>%
  rstac::get_request()%>%
  rstac::items_fetch()

bathy_wms_link <- bathy_items$features[[8]]$assets$wms$href
bathy_wms_base <- sub("\\?.*$", "", bathy_wms_link)
bathy_layer_name <- sub(".*LAYERS=([A-Za-z0-9_:]+).*", "\\1", bathy_wms_link)
bathy_layer_name_1 <- "mean_atlas_land"
bathy_layer_name_2 <- "emodnet:mean_multicolour"

## legend
bathy_legend_url_1 <- paste0(
  bathy_wms_base,
  "?SERVICE=WMS&REQUEST=GetLegendGraphic",
  "&FORMAT=image/png",
  "&LAYER=", bathy_layer_name_1,
  "&VERSION=1.1.1"
)

bathy_legend_url_2 <- paste0(
  bathy_wms_base,
  "?SERVICE=WMS&REQUEST=GetLegendGraphic",
  "&FORMAT=image/png",
  "&LAYER=", bathy_layer_name_2,
  "&VERSION=1.1.1"
)

browseURL(bathy_legend_url_1)

# test map
leaflet() %>%
  setView(3, 51.5, zoom = 8) %>%
  addTiles() %>%
  addWMSTiles(
    baseUrl = bathy_wms_base,
    layers  = bathy_layer_name_1,
    options = WMSTileOptions(
      format = "image/png",
      transparent = T,
      opacity = 1
    )) %>%
  addControl(
    html = paste0(
      '<div style="background:white;padding:6px;border-radius:4px;">',
      '<div style="font-weight:600;margin-bottom:4px;">Bathymetry</div>',
      '<img src="', bathy_legend_url_1, '" />',
      '</div>'
    ),position = "bottomright"
  )

# add both bathymetry layers
wms_registry <- add_row(
  wms_registry,
  env_data_name = "bathy_atlas", # more 'elegant' looking
  collection_name = c_bathy_selection,
  wms_link    = bathy_wms_link,
  wms_base    = bathy_wms_base,
  wms_layer_name  = bathy_layer_name_1,
  legend_link = bathy_legend_url_1,
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 

wms_registry <- add_row(
  wms_registry,
  env_data_name = "bathy_multicolor", # more intense color scale
  collection_name = c_bathy_selection,
  wms_link    = bathy_wms_link,
  wms_base    = bathy_wms_base,
  wms_layer_name  = bathy_layer_name_2,
  legend_link = bathy_legend_url_2,
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 

## seabedhabitats -doesnt work -----------------------------------------------------

c_seabedhabitats_list <- c_all %>% dplyr::filter(grepl("habitat", c_all$value))
c_seabedhabitats_selection <- c_seabedhabitats_list$value[42] 
c_seabedhabitats_selection
#12 # should be the layer
# 4 shows sth
# 5 # gives complete legend: "https://ows.emodnet-seabedhabitats.eu/geoserver/emodnet_view/wms?SERVICE=WMS&REQUEST=GetLegendGraphic&FORMAT=image/png&LAYER=eunismaps_all&VERSION=1.1.1"
# 13 shows something but not BPNS
# 15, 16, 23 only baltic
# 22 IRL
# 24 # emodnet-modelled_projections_of_habitat_for_commercial_fish_around_north_western_europe_under_climate_change_2020_to_2060
# 27 seagrass cover (EOV)

seabedhabitats_items <- stac_obj %>%
  rstac::stac_search(collections = c_seabedhabitats_selection, limit = 500) %>%
  rstac::get_request() %>%
  rstac::items_fetch()


# wms
seabedhabitats_wms_link <- seabedhabitats_items$features[[1]]$assets$wms$href
seabedhabitats_wms_link
browseURL(seabedhabitats_wms_link)
seabedhabitats_wms_base <- sub("\\?.*$", "", seabedhabitats_wms_link)
seabedhabitats_layer_name <- sub(".*LAYERS=([A-Za-z0-9_:]+).*", "\\1", seabedhabitats_wms_link)

# parquet
seabedhabitats_parquet_link <- seabedhabitats_items$features[[1]]$assets$parquet$href
seabedhabitats_parquet <- arrow::read_parquet(seabedhabitats_parquet_link, format = "parquet")
# 
# library(sf)
# #library(dplyr)
# geom_test <- st_as_sfc(as.list(seabedhabitats_parquet$geometry[1:5]), crs = 4326)
# st_bbox(geom_test)
# 
# library(DBI)
# library(duckdb)
# 
# con <- dbConnect(duckdb())
# 
# dbExecute(con, "INSTALL spatial;")
# dbExecute(con, "LOAD spatial;")
# 
# sql <- "
# SELECT *
# FROM read_parquet($1)
# WHERE ST_Intersects(
#   geometry,
#   ST_MakeEnvelope(2, 50, 6, 52)
# )
# "
# 
# subset_df <- dbGetQuery(con, sql, params = list(seabedhabitats_parquet_link))
# subset_df <- dbGetQuery(con, sql, params = list(seabedhabitats_parquet_link))
# 
# dbDisconnect(con, shutdown = TRUE)
# 


#library(sf)
#seabedhabitats_p_sf <- sf::st_as_sf(seabedhabitats_parquet)


## legend
seabedhabitats_legend_url <- paste0(
  seabedhabitats_wms_base,
  "?SERVICE=WMS&REQUEST=GetLegendGraphic",
  "&FORMAT=image/png",
  "&LAYER=", seabedhabitats_layer_name,
  "&VERSION=1.1.1"
)

browseURL(seabedhabitats_legend_url)

# test map
leaflet() %>%
  setView(3, 51.5, zoom = 8) %>%
  addTiles() %>%
  addWMSTiles(
    baseUrl = seabedhabitats_wms_base,
    layers  = seabedhabitats_layer_name,
    options = WMSTileOptions(
      format = "image/png",
      transparent = T,
      opacity = 1
    )) %>%
  addControl(
    html = paste0(
      '<div style="background:white;padding:6px;border-radius:4px;">',
      '<div style="font-weight:600;margin-bottom:4px;">seabedhabitats Areas</div>',
      '<img src="', seabedhabitats_legend_url, '" />',
      '</div>'
    ),position = "bottomright"
  )





# non-STAC (for the moment) layers ----------------------------------------
## a few layers are not in the EDITO STAC catalogue so we call them externally

## EEZ ---------------------------------------------------------------------

wms_registry <- add_row(
  wms_registry,
  env_data_name = "eez", 
  collection_name = "",
  wms_link    = "https://geo.vliz.be/geoserver/MarineRegions/wms?service=WMS&version=1.1.0&request=GetMap&layers=MarineRegions%3Aeez&bbox=-180.0%2C-62.78834217149148%2C180.0%2C86.99400535016684&width=768&height=330&srs=EPSG%3A4326&styles=&format=application/openlayers",
  wms_base    = "https://geo.vliz.be/geoserver/MarineRegions/wms",
  wms_layer_name  = "eez",
  legend_link = "",
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 


## BPNS shipwrecks --------------------------------------------------------------

wms_registry <- add_row(
  wms_registry,
  env_data_name = "shipwrecks",
  collection_name = "",
  wms_link    = "https://geo.vliz.be/geoserver/Kustportaal/wms?service=WMS&version=1.1.0&request=GetMap&layers=Kustportaal%3Ascheepswrakken_20180604&bbox=2.305333333%2C51.11496667%2C4.39055%2C51.84788333&width=768&height=330&srs=EPSG%3A4326&styles=&format=application/openlayers",
  wms_base    = "https://geo.vliz.be/geoserver/Kustportaal/wms",
  wms_layer_name  = "Kustportaal:scheepswrakken_20180604",
  legend_link = "",
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 


# SST - TODO --------------------------------------------------------------

"climate_forecast-sea_surface_temperature" # right collection?

# save wms registry -------------------------------------------------------
wms_registry_path <- "./data/EDITO_STAC_layers_metadata.csv"
write.csv(wms_registry, wms_registry_path)

rm(list = ls())

# # OLD below here ----------------------------------------------------------
# 
# # get all relevant collections--------------------------------------------
# ## collection names
# c_ETN_data <- "animal_tracking_datasets"
# 
# c_bathy_list <- c_all %>% dplyr::filter(grepl("bathymetry", c_all$value))
# c_bathy_selection <- "emodnet-bathymetry"
# 
# c_owf_list <- c_all %>% dplyr::filter(grepl("wind", c_all$value))
# c_owf_selection <- c("emodnet-wind_farm_power_mw")
# 
# #col_shipwrecks <- col_all %>% dplyr::filter(grepl("ship_wreck", col_all$value))
# 
# 
# # etn data ----------------------------------------------------------------
# 
# animal_tracking_col <- stac_obj %>%
#   collections(c_ETN_data)%>%
#   get_request()
# 
# #etn_link <- paste0(api_endpoint_url, c_ETN_data)
# # # items - probably not needed!
# # item_obj <- rstac::items(col_obj) %>%
# #   rstac::get_request()
# 
# etn_items <- stac_obj %>%
#   rstac::stac_search(collections = "animal_tracking_datasets") %>%
#   rstac::get_request()%>%
#   rstac::items_fetch()
# 
# etn_link_href <- etn_items$features[[13]]$assets$data$href
# 
# etn_seabass <- arrow::read_parquet(etn_link_href, format = "parquet")
# 
# 
# # OWF ---------------------------------------------------------------------
# 
# OWF_col <- stac_obj %>%
#   collections(c_owf_selection)%>%
#   get_request()
# 
# owf_items <- stac_obj %>%
#   stac_search(collections = c_owf_selection, limit = 500) %>%
#   get_request() %>%
#   items_fetch()
# 
# # wms
# owf_link_wms_href <- owf_items$features[[1]]$assets$wms$href
# 
# wms_base <- "https://ows.emodnet-humanactivities.eu/wms" #TODO: make programmatically
# wms_base_owf <- sub("\\?.*$", "", owf_link_wms_href)
# layer_name_owf <- "windfarmspoly" #TODO: make programmatically
# 
# 
# # legend
# 
# legend_url <- paste0(
#   wms_base,
#   "?SERVICE=WMS&REQUEST=GetLegendGraphic",
#   "&FORMAT=image/png",
#   "&LAYER=", layer_name_owf,
#   "&VERSION=1.1.1"
# )
# 
# legend_url
# browseURL(legend_url)   # opens the legend image in your browser
# 
# 
# # EEZs --------------------------------------------------------------------
# 
# mregions2::mrp_list %>% View()
# 
# leaflet() %>%
#   addWMSTiles(
#     baseUrl = "https://geo.vliz.be/geoserver/MarineRegions/wms",
#     layers = "eez",   # replace with your layer name
#     options = WMSTileOptions(
#       format = "image/png",
#       styles = "Polygons_greyoutline",
#       transparent = TRU
#     )
#     
#   )
# 
# library(mregions2)
# library(leaflet)
# library(dplyr)
# 
# m <- 
#   leaflet::leaflet() %>% 
#   addWMSTiles(
#     baseUrl = "https://geo.vliz.be/geoserver/MarineRegions/wms",
#     layers = "iho",   # replace with your layer name
#     options = WMSTileOptions(
#       format = "image/png",
#       styles = "Polygons_greyoutline",
#       transparent = T
#     )
#   )
# 
# str(m)
# 
# # Leaflet CRS definition for EPSG:4326 (degrees)
# 
# 
# crs4326 <- leafletCRS(
#   crsClass = "L.CRS.EPSG4326",
#   code = "EPSG:4326",
#   proj4def = "+proj=longlat +datum=WGS84 +no_defs",
#   resolutions = 1.40625 / (2^(0:18))
# )
# 
# leaflet(options = leafletOptions(crs = crs4326)) %>%
#   fitBounds(-180, -63, 180, 87) %>%
#   addWMSTiles(
#     baseUrl = "https://geo.vliz.be/geoserver/MarineRegions/wms",
#     layers  = "MarineRegions:eez",
#     options = WMSTileOptions(
#       version     = "1.1.1",
#       format      = "image/png",
#       transparent = TRUE,
#       styles      = ""
#     )
#   )
# 
# 
# leaflet() %>%
#  # setView(0, 20, 2) %>%
#   
#   addWMSTiles(
#     baseUrl = "https://geo.vliz.be/geoserver/MarineRegions/wms",
#     layers  = "MarineRegions:eez",
#     options = WMSTileOptions(
#       version     = "1.1.1",
#       srs         = "EPSG:3857",   # 🔴 EXPLICITLY FORCE SRS
#       format      = "image/png",
#       transparent = TRUE,
#       styles      = ""
#     )
#   )
# # map ---------------------------------------------------------------------
# 
# 
# 
# 
# # map
# 
# library(leaflet)
# library(magrittr)
# 
# leaflet() %>%
#   addTiles() %>%
#   addWMSTiles(
#     baseUrl = wms_base_owf,
#     layers  = layer_name_owf,
#     options = WMSTileOptions(
#       format = "image/png",
#       transparent = TRUE
#     ))
# 
# m <- leaflet() %>%
#   # --- Base map (background) ---
#   addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>%
#   
#   # --- Optional: Bathymetry as a BASE layer (select one base at a time) ---
#   addWMSTiles(
#     baseUrl = wms_base_bathy,
#     layers  = layer_name_bathy,
#     options = WMSTileOptions(
#       format = "image/png",
#       transparent = TRUE
#     ),
#     group = "EMODnet Bathymetry"
#   ) %>%
#   
#   # --- Overlays (toggle on/off) ---
#   addWMSTiles(
#     baseUrl = wms_base_owf,
#     layers  = layer_name_owf,
#     options = WMSTileOptions(
#       format = "image/png",
#       transparent = TRUE
#     ),
#     group = "OWF"  # IMPORTANT: group belongs here (not inside WMSTileOptions)
#   ) %>%
#   
#   addWMSTiles(
#     baseUrl = "https://geo.vliz.be/geoserver/MarineRegions/wms",
#     layers  = "eez",
#     options = WMSTileOptions(
#       format = "image/png",
#       styles = "Polygons_greyoutline",
#       transparent = F),
#     group = "EEZ"
#   ) %>%
#   
#   # --- Legend / control ---
#   addControl(
#     html = paste0(
#       '<div style="background:white;padding:6px;border-radius:4px;">',
#       '<div style="font-weight:600;margin-bottom:4px;">Wind farms</div>',
#       '<img src="', legend_url, '" />',
#       '</div>'
#     ),
#     position = "bottomright"
#   ) %>%
#   
#   # --- Layer switcher ---
#   addLayersControl(
#     baseGroups    = c("EMODnet Bathymetry", "CartoDB.Positron", "OWF", "EEZ"),
#     overlayGroups = c("OWF", "EEZ"),
#     options       = layersControlOptions(collapsed = FALSE)
#   )
# 
# m
# 
# # tests
# leaflet() %>%
#   addWMSTiles(
#     baseUrl = "https://geo.vliz.be/geoserver/MarineRegions/wms",
#     layers  = "eez",   # FULL qualified layer name
#     options = WMSTileOptions(
#       format      = "image/png",
#       transparent = TRUE,
#       styles      = "",              # empty style ensures default drawing
#       version     = "1.1.1"          # use WMS 1.1.1 for Leaflet compatibility
#     ),
#     group = "EEZ"
#   )
# 
# leaflet() %>%
#   #addTiles() %>%
#   addWMSTiles(
#     baseUrl = "https://geo.vliz.be/geoserver/Belgium/wms",
#     layers  = "Belgium"
#   )
# 
# # leaflet() %>% 
# #   addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>%
# #   addWMSTiles(
# #     baseUrl = wms_base,
# #     layers  = layer_name,
# #     options = WMSTileOptions(
# #       format = "image/png",
# #       transparent = TRUE,
# #       group = "OWF"
# #     )) %>%
# #       addWMSTiles(
# #         baseUrl = "https://geo.vliz.be/geoserver/MarineRegions/wms",
# #         layers = "eez",   # replace with your layer name
# #         options = WMSTileOptions(
# #           format = "image/png",
# #           styles = "Polygons_greyoutline",
# #           transparent = TRUE,
# #           group = "EEZ"
# #         )) %>%
# #     addWMSTiles(
# #       baseUrl = wms_base_bathy,
# #       layers  = layer_name_bathy,
# #       options = WMSTileOptions(format = "image/png", transparent = TRUE),
# #       group = "EMODnet Bathymetry"
# #     ) %>%
# #   addControl(
# #     html = paste0('<div style="background:white;padding:0px;border-radius:1px;">',
# #                   '<div style="font-weight:600;margin-bottom:1px;">Wind farms</div>',
# #                   '<img src="', legend_url, '" />',
# #                   '</div>'),
# #     position = "bottomright"
# #   ) %>%
# #   addLayersControl(
# #     baseGroups = c(
# #       "CartoDB.Positron", "EMODnet Bathymetry"),
# #     overlayGroups = c("OWF", "EEZ"),
# #     options = layersControlOptions(collapsed = FALSE)
# #   )
# 
# # bathymetry --------------------------------------------------------------
# 
# bathy_objects <- stac_obj %>%
#   rstac::collections(c_bathy_selection)%>%
#   rstac::get_request()
# 
# bathy_items <- stac_obj %>%
#   rstac::stac_search(collections = c_bathy_selection) %>%
#   rstac::get_request()%>%
#   rstac::items_fetch()
# 
# test <- bathy_items$features[[20]]
# bathy_link_href <- bathy_items$features[[20]]$assets$data$href
# 
# arrow::open_dataset(test[["url"]])
# 
# rstac::assets_url(test)                 # all asset hrefs
# 
# 
# # chatgpt suggestions -----------------------------------------------------
# 
# library(rstac)
# library(purrr)
# library(dplyr)
# library(leaflet)
# 
# # bathy_items is your rstac items object
# 
# # 1) Find an item that actually has a WMS asset
# has_asset <- function(item, asset_name) {
#   !is.null(item$assets[[asset_name]]) && !is.null(item$assets[[asset_name]]$href)
# }
# 
# idx_wms <- which(map_lgl(bathy_items$features, has_asset, asset_name = "wms"))[1]
# 
# stopifnot(!is.na(idx_wms))  # will error if no WMS found
# 
# wms_item <- bathy_items$features[[idx_wms]]
# wms_href <- wms_item$assets$wms$href
# wms_href
# # print it so you can inspect the URL
# 
# # 2) Add it in leaflet
# # Leaflet needs:
# #   - a base WMS URL (usually the part before the '?')
# #   - one or more layer names (often you can discover these from GetCapabilities)
# wms_base_bathy <- sub("\\?.*$", "", wms_href)
# 
# # GetCapabilities URL (helps you discover the 'layers' name)
# caps_url <- paste0(wms_base, "?service=WMS&request=GetCapabilities")
# browseURL(caps_url)
# 
# # 3) Replace this with a real layer name you see in GetCapabilities
# layer_name <- "emodnet:mean"
# layer_name <- "emodnet:mean_multicolour"
# layer_name <- "mean_atlas_land"
# 
# leaflet() %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addWMSTiles(
#     baseUrl = wms_base,
#     layers  = layer_name,
#     options = WMSTileOptions(format = "image/png", transparent = TRUE)
#   )
# 
# 
# # geotiff -----------------------------------------------------------------
# 
# library(terra)
# library(leaflet)
# 
# idx_tif <- which(map_lgl(bathy_items$features, has_asset, asset_name = "geotiff"))[1]
# tif_href <- bathy_items$features[[idx_tif]]$assets$geotiff$href
# 
# # download (recommended for https) then read
# tf <- tempfile(fileext = ".tif")
# download.file(tif_href, tf, mode = "wb")
# 
# r <- terra::rast(tf)
# 
# leaflet() %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addRasterImage(r, opacity = 0.7)
# 
# 
# # other -------------------------------------------------------------------
# 
# 
# 
# # bathy_href <- bathy_objects[["url"]]
# # arrow::open_dataset(bathy_href)
# 
# 
# # shipwrecks --------------------------------------------------------------
# 
# shipwreck_projectid <- "emodnet_bathymetry_ship_wreck"
# 
# shipwreck <- bathy_items %>% dplyr::filter(grepl(shipwreck_projectid, bathy_items[["features"]][[1]][["properties"]][["productIdentifier"]]))
# 
# # connect_eurobis <- function(){data_lake <- arrow::S3FileSystem$create(
# #   anonymous = T,
# #   scheme = "https",
# #   endpoint_override = "s3.waw3-1.cloudferro.com"
# # )
# # 
# # s3_path = "emodnet/emodnet_biology/12639/eurobis_gslayer_obisenv_19022025.parquet"
# # eurobis <- arrow::open_dataset(
# #   s3_path,
# #   filesystem = data_lake,
# #   format = "parquet"
# # )
# # return(eurobis)
# #}
# # Do this when: This is a direct connection to the S3 bucket, using Arrow’s S3FileSystem.
# # 
# # Advantages:
# #   
# #   Works with very large datasets that don’t fit in memory.
# # 
# # Allows querying, filtering, and joining without downloading the full file.
# # 
# # Can read Parquet natively, no need to convert.
# # 
# # But otherwise just
# # 
# # test <- arrow::read_parquet(link_href)
# # 
# # stac_obj
# # 
# # 3️⃣ How to “load” a dataset
# # 
# # Usually, you want something like:
# #   
# #   first_item <- items$features[[1]]
# # first_item$assets$tracking_data$href  # URL to the actual CSV / NetCDF / file
# # 
# # 
# # Then you can read it in R:
# #   
# #   read.csv(first_item$assets$tracking_data$href)
# # 
# # 
# # Or, if it’s a raster/NetCDF:
# #   
# #   library(terra)
# # rast(first_item$assets$tracking_data$href)