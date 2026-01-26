##################################################################################
##################################################################################

# Author: Lotte Pohl
# Email: lotte.pohl@vliz.be
# Date: 2026-01-26
# Script Name: ~/DUC2_viewer_acoustic_telemetry/leaflet_env_data.R
# Script Description: make a leaflet map with several environmental layers as overlaygroups 

##################################################################################
##################################################################################


# install and load libraries ----------------------------------------------
# install.packages("leaflet.extras")
# install.packages("leafem")
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(htmlwidgets)

# read in EDITO STAC WMS metadata -----------------------------------------
if(!file.exists("./data/EDITO_STAC_layers_metadata.csv")){
  source("EDITO_STAC_data_access.R")
  }
# read in WMS layer metadata
wms_layers <- 
  read.csv("./data/EDITO_STAC_layers_metadata.csv") %>%
  split(.$env_data_name)


# make base leaflet map --------------------------------------------------------

north.arrow.icon <-
  "<img src='https://www.clipartbest.com/cliparts/yTo/Lgr/yToLgryGc.png' style='width:35px;height:45px;'>"


map_base <-
  leaflet() %>%
  setView(3, 51.5, zoom = 8) %>%
  addTiles(group = "OpenStreetMap") %>%
  leafem::addMouseCoordinates() %>%
  leaflet.extras::addFullscreenControl() %>%
  leaflet::addScaleBar(position = "bottomleft",
                       options = scaleBarOptions(
                         maxWidth = 150,
                         imperial = FALSE)) %>%  
  # north arrow
  leaflet::addControl( html = north.arrow.icon,
                       position = "topleft",
                       className = "fieldset {border: 0;}") 

map_base



# add WMS layers to leaflet map --------------------------------------------------------

bathy_legend <- paste0(
  '<details id="bathy-legend" style="background:white;padding:6px;border-radius:4px;">',
  '<summary style="cursor:pointer;font-weight:600;">', wms_layers$bathy_atlas$env_data_name, '</summary>',
  '<img src="', wms_layers$bathy_atlas$legend_link, '" />',
  '</details>'
)

map_WMS_EDITO <-
  map_base %>%

## bathymetry WMS --------
  addWMSTiles(
    baseUrl = wms_layers$bathy_atlas$wms_base, layers = wms_layers$bathy_atlas$wms_layer_name,
    options = WMSTileOptions(format = "image/png", transparent = T),
    group = "bathymetry") %>%
  addControl(html = 
               paste0(
                 '<details id="bathy-legend" style="background:white;padding:6px;border-radius:4px;">',
                 '<summary style="cursor:pointer;font-weight:600;">', "Elevation", '</summary>',
                 '<img src="', wms_layers$bathy_atlas$legend_link, '" />',
                 '</details>'
               ), position = "topright") %>%
  # make Legend collapsible
  onRender("
    function(el, x){
      var map = this;
      var lg = document.getElementById('bathy-legend');
      function set(v){ lg.style.display = v ? 'block' : 'none'; }
      map.on('overlayadd', function(e){
        if(e.name === 'bathymetry') set(true);
      });
      map.on('overlayremove', function(e){
        if(e.name === 'bathymetry') set(false);
      });
    }
  ") %>%
  
  ## OWF WMS --------
addWMSTiles(
  baseUrl = wms_layers$owf$wms_base, layers = wms_layers$owf$wms_layer_name,
  options = WMSTileOptions(format = "image/png", transparent = T),
  group = "Offshore Wind Farms (OWF)") %>%
  addControl(html = 
               paste0(
                 '<details id="bathy-legend" style="background:white;padding:6px;border-radius:4px;">',
                 '<summary style="cursor:pointer;font-weight:600;">', "OWF status", '</summary>',
                 '<img src="', wms_layers$owf$legend_link, '" />',
                 '</details>'
               ), position = "topright") %>%
  # make Legend collapsible
  onRender("
    function(el, x){
      var map = this;
      var lg = document.getElementById('bathy-legend');
      function set(v){ lg.style.display = v ? 'block' : 'none'; }
      map.on('overlayadd', function(e){
        if(e.name === 'Offshore Wind Farms (OWF)') set(true);
      });
      map.on('overlayremove', function(e){
        if(e.name === 'Offshore Wind Farms (OWF)') set(false);
      });
    }
  ") %>%

  ## EEZ WMS --------
addWMSTiles(
  baseUrl = wms_layers$eez$wms_base, layers = wms_layers$eez$wms_layer_name,
  options = WMSTileOptions(format = "image/png", transparent = T),
  group = "Exclusive Economic Zones (EEZ)") %>%


## Layer control --------
addLayersControl(position = "bottomleft" ,
                 baseGroups = c("bathymetry"),
                 overlayGroups = c("Offshore Wind Farms (OWF)"), #, "shipwrecks"
                 options = layersControlOptions(collapsed = FALSE)) #%>%
   

map_WMS_EDITO


# tests -------------------------------------------------------------------

leaflet() %>%
  addWMSTiles(
    baseUrl = wms_layers$eez$wms_base, layers = wms_layers$eez$wms_layer_name,
    options = WMSTileOptions(format = "image/png", transparent = T),
    group = "Exclusive Economic Zones (EEZ)") 
