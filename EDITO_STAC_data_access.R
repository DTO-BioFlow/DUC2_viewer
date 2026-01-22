install.packages("rstac")
library(rstac)
library(purrr)
library(arrow)
library(dplyr)
library(leaflet)

# acces the STAC catalogue and get collections ----------------------------

stac_endpoint_url <- 'https://catalog.dive.edito.eu/'
api_endpoint_url <- "https://api.dive.edito.eu/data/collections/"
stac_obj <- stac(stac_endpoint_url)

stac_overview <- stac('https://catalog.dive.edito.eu/catalogs')%>%get_request()

# collections
col_obj <- collections(stac_obj) %>%
  get_request()

c_all <- col_obj$collections |> vapply(`[[`, character(1), "id") %>% as_tibble()



# get all relevant collections--------------------------------------------
## collection names
c_ETN_data <- "animal_tracking_datasets"

c_bathy_list <- c_all %>% dplyr::filter(grepl("bathymetry", c_all$value))
c_bathy_selection <- "emodnet-bathymetry"

c_owf_list <- c_all %>% dplyr::filter(grepl("wind", c_all$value))
c_owf_selection <- c("emodnet-wind_farm_year")

#col_shipwrecks <- col_all %>% dplyr::filter(grepl("ship_wreck", col_all$value))


# etn data ----------------------------------------------------------------

animal_tracking_col <- stac_obj %>%
  collections(c_ETN_data)%>%
  get_request()

#etn_link <- paste0(api_endpoint_url, c_ETN_data)
# # items - probably not needed!
# item_obj <- rstac::items(col_obj) %>%
#   rstac::get_request()

etn_items <- stac_obj %>%
  rstac::stac_search(collections = "animal_tracking_datasets") %>%
  rstac::get_request()%>%
  rstac::items_fetch()

etn_link_href <- etn_items$features[[13]]$assets$data$href

etn_seabass <- arrow::read_parquet(etn_link_href, format = "parquet")


# OWF ---------------------------------------------------------------------

OWF_col <- stac_obj %>%
  collections(c_owf_selection)%>%
  get_request()

owf_items <- stac_obj %>%
  stac_search(collections = c_owf_selection, limit = 500) %>%
  get_request() %>%
  items_fetch()

# reading parquet does not work!
owf_link_parquet_href <- owf_items$features[[1]]$assets$parquet$href
owf <- arrow::read_parquet(owf_link_wmf_href, format = "parquet")

# wms
owf_link_wmf_href <- owf_items$features[[1]]$assets$wms$href

wms_base <- "https://ows.emodnet-humanactivities.eu/wms" #TODO: make programmatically
layer_name <- "windfarmspoly" #TODO: make programmatically

leaflet() %>% #TODO: understand what colors mean
  addProviderTiles("CartoDB.Positron") %>%
  addWMSTiles(
    baseUrl = wms_base,
    layers  = layer_name,
    options = WMSTileOptions(
      format = "image/png",
      transparent = TRUE
      # You can add: opacity = 0.7
    )
  )

# bathymetry --------------------------------------------------------------

bathy_objects <- stac_obj %>%
  rstac::collections(c_bathy_selection)%>%
  rstac::get_request()

bathy_items <- stac_obj %>%
  rstac::stac_search(collections = c_bathy_selection) %>%
  rstac::get_request()%>%
  rstac::items_fetch()

test <- bathy_items$features[[20]]
bathy_link_href <- bathy_items$features[[20]]$assets$data$href

arrow::open_dataset(test[["url"]])

rstac::assets_url(test)                 # all asset hrefs


# chatgpt suggestions -----------------------------------------------------

library(rstac)
library(purrr)
library(dplyr)
library(leaflet)

# bathy_items is your rstac items object

# 1) Find an item that actually has a WMS asset
has_asset <- function(item, asset_name) {
  !is.null(item$assets[[asset_name]]) && !is.null(item$assets[[asset_name]]$href)
}

idx_wms <- which(map_lgl(bathy_items$features, has_asset, asset_name = "wms"))[1]

stopifnot(!is.na(idx_wms))  # will error if no WMS found

wms_item <- bathy_items$features[[idx_wms]]
wms_href <- wms_item$assets$wms$href
wms_href
# print it so you can inspect the URL

# 2) Add it in leaflet
# Leaflet needs:
#   - a base WMS URL (usually the part before the '?')
#   - one or more layer names (often you can discover these from GetCapabilities)
wms_base <- sub("\\?.*$", "", wms_href)

# GetCapabilities URL (helps you discover the 'layers' name)
caps_url <- paste0(wms_base, "?service=WMS&request=GetCapabilities")
browseURL(caps_url)

# 3) Replace this with a real layer name you see in GetCapabilities
layer_name <- "emodnet:mean"
layer_name <- "emodnet:mean_multicolour"
layer_name <- "mean_atlas_land"

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addWMSTiles(
    baseUrl = wms_base,
    layers  = layer_name,
    options = WMSTileOptions(format = "image/png", transparent = TRUE)
  )


# geotiff -----------------------------------------------------------------

library(terra)
library(leaflet)

idx_tif <- which(map_lgl(bathy_items$features, has_asset, asset_name = "geotiff"))[1]
tif_href <- bathy_items$features[[idx_tif]]$assets$geotiff$href

# download (recommended for https) then read
tf <- tempfile(fileext = ".tif")
download.file(tif_href, tf, mode = "wb")

r <- terra::rast(tf)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addRasterImage(r, opacity = 0.7)


# other -------------------------------------------------------------------



# bathy_href <- bathy_objects[["url"]]
# arrow::open_dataset(bathy_href)


# shipwrecks --------------------------------------------------------------

shipwreck_projectid <- "emodnet_bathymetry_ship_wreck"

shipwreck <- bathy_items %>% dplyr::filter(grepl(shipwreck_projectid, bathy_items[["features"]][[1]][["properties"]][["productIdentifier"]]))

# connect_eurobis <- function(){data_lake <- arrow::S3FileSystem$create(
#   anonymous = T,
#   scheme = "https",
#   endpoint_override = "s3.waw3-1.cloudferro.com"
# )
# 
# s3_path = "emodnet/emodnet_biology/12639/eurobis_gslayer_obisenv_19022025.parquet"
# eurobis <- arrow::open_dataset(
#   s3_path,
#   filesystem = data_lake,
#   format = "parquet"
# )
# return(eurobis)
#}
# Do this when: This is a direct connection to the S3 bucket, using Arrow’s S3FileSystem.
# 
# Advantages:
#   
#   Works with very large datasets that don’t fit in memory.
# 
# Allows querying, filtering, and joining without downloading the full file.
# 
# Can read Parquet natively, no need to convert.
# 
# But otherwise just
# 
# test <- arrow::read_parquet(link_href)
# 
# stac_obj
# 
# 3️⃣ How to “load” a dataset
# 
# Usually, you want something like:
#   
#   first_item <- items$features[[1]]
# first_item$assets$tracking_data$href  # URL to the actual CSV / NetCDF / file
# 
# 
# Then you can read it in R:
#   
#   read.csv(first_item$assets$tracking_data$href)
# 
# 
# Or, if it’s a raster/NetCDF:
#   
#   library(terra)
# rast(first_item$assets$tracking_data$href)