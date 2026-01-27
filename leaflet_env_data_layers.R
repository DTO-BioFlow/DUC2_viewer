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

# colors
blue_light <- "#cde3f6" 
blue_medium <- "#477292"
blue_dark <- "#11395a"

# read in EDITO STAC WMS metadata -----------------------------------------
if(!file.exists("./data/EDITO_STAC_layers_metadata.csv")){
  source("EDITO_STAC_data_access.R")
  }
# read in WMS layer metadata
wms_layers <- 
  wms_registry %>%
  # read.csv("./data/EDITO_STAC_layers_metadata.csv") %>%
  split(.$env_data_name)


# make base leaflet map --------------------------------------------------------

north.arrow.icon <-
  "<img src='https://www.clipartbest.com/cliparts/yTo/Lgr/yToLgryGc.png' style='width:35px;height:45px;'>"


map_base <-
  leaflet() %>%
  setView(3, 51.5, zoom = 8) %>%
  addTiles(group = "Open Street Map") %>%
  addProviderTiles("CartoDB.Positron",
                   group = "CartoDB.Positron") %>%
  addTiles(urlTemplate = "https://tiles.emodnet-bathymetry.eu/2020/baselayer/web_mercator/{z}/{x}/{y}.png",
           group = "EMODnet Bathymetry") %>%
  leafem::addMouseCoordinates() %>%
  leaflet.extras::addFullscreenControl() %>%
  leaflet::addScaleBar(position = "bottomleft",
                       options = scaleBarOptions(
                         maxWidth = 150,
                         imperial = FALSE)) %>%  
  # north arrow
  leaflet::addControl( html = north.arrow.icon,
                       position = "topleft",
                       className = "fieldset {border: 0;}") %>%
  
  # minimap
  addMiniMap(position = "topright",
             width = 125,
             height = 100,
             zoomLevelOffset = -4,
             zoomLevelFixed = T,
             centerFixed = F,
             aimingRectOptions = list(color = blue_medium, weight = 1, clickable = FALSE),
             shadowRectOptions = list(color = blue_dark, weight = 1, clickable = FALSE, opacity = 0, fillOpacity = 0)
             # ,tiles = providers$Esri.WorldStreetMap
             , tiles = "https://tiles.emodnet-bathymetry.eu/2020/baselayer/web_mercator/{z}/{x}/{y}.png"
             
  )

# map_base


# function to render the legends correctly --------------------------------
# we want to have the legend disappear when the respective group is toggled off in the overlaygroups

# so we first make a function with the html layout for the legend to be displayed on the map
legend_control <- function(id, title, img_url) {
  paste0(
    '<details id="', id, '" style="background:white;padding:0px;border-radius:0px;display:none;">',
    '<summary style="cursor:pointer;font-weight:600;">', title, '</summary>',
    '<img src="', img_url, '" />',
    '</details>'
  )
}

# add WMS layers to leaflet map --------------------------------------------------------

map_WMS_EDITO <-
  map_base %>%
  
  ## Note: map layers will be plotted in the order of appearance of the layers below

  ## seabed habitats WMS --------
  addWMSTiles(
    baseUrl = wms_layers$seabedhabitats$wms_base, layers = wms_layers$seabedhabitats$wms_layer_name,
    options = WMSTileOptions(format = "image/png", transparent = T),
    group = "Seabed substrates") %>%
    addControl(
      html = legend_control("legend-seabedhabitats", "Substrate type", wms_layers$seabedhabitats$legend_link),
      position = "topright") %>%
  
  ## bathymetry WMS --------
  addWMSTiles(
    baseUrl = wms_layers$bathy_multicolor$wms_base, layers = wms_layers$bathy_multicolor$wms_layer_name,
    options = WMSTileOptions(format = "image/png", transparent = T, opacity = 0.5),
    group = "Bathymetry (multicolor)") %>%
    addControl(
      html = legend_control("legend-bathy", "Depth", wms_layers$bathy_multicolor$legend_link),
      position = "topright") %>%
    
  ## EEZ WMS --------
  # from a human activities layer in the STAC catalogue
  addWMSTiles(
    baseUrl = wms_layers$stac_eez$wms_base, layers = wms_layers$stac_eez$wms_layer_name,
    options = WMSTileOptions(format = "image/png", transparent = T), #, styles = "Polygons_greyoutline"
    group = "Exclusive Economic Zones (EEZ)") %>%
  
  # from the VLIZ geoserver (does not work all the time in the EDITO datalab)
  addWMSTiles(
    baseUrl = wms_layers$eez$wms_base, layers = wms_layers$eez$wms_layer_name,
    options = WMSTileOptions(format = "image/png", transparent = T, styles = "Polygons_greyoutline"),
    group = "Exclusive Economic Zones (EEZ)") %>%
  
  ## MSP WMS --------
  addWMSTiles(
    baseUrl = wms_layers$msp$wms_base, layers = wms_layers$msp$wms_layer_name,
    options = WMSTileOptions(format = "image/png", transparent = T),
    group = "Marine Spatial Plans") %>%
    addControl(
      html = legend_control("legend-msp", "Activity", wms_layers$msp$legend_link),
      position = "topright") %>%
  
  ## sea_conventions WMS --------
  addWMSTiles(
    baseUrl = wms_layers$sea_conventions$wms_base, layers = wms_layers$sea_conventions$wms_layer_name,
    options = WMSTileOptions(format = "image/png", transparent = T, opacity = 0.75),
    group = "Sea convention polygons") %>%
    addControl(
      html = legend_control("legend-sea_conventions", "Convention framework", wms_layers$sea_conventions$legend_link),
      position = "topright") %>%
  
  ## spc WMS --------
  addWMSTiles(
    baseUrl = wms_layers$spc$wms_base, layers = wms_layers$spc$wms_layer_name,
    options = WMSTileOptions(format = "image/png", transparent = T),
    group = "Submarine Power Cables (SPC)") %>%
    addControl(
      html = legend_control("legend-spc", "Cable owner", wms_layers$spc$legend_link),
      position = "topright") %>%  
  
  ## OWF WMS --------
  addWMSTiles(
    baseUrl = wms_layers$owf$wms_base, layers = wms_layers$owf$wms_layer_name,
    options = WMSTileOptions(format = "image/png", transparent = T, opacity = 1),
    group = "Offshore Wind Farms (OWF)") %>%
    addControl(
      html = legend_control("legend-owf", "OWF status", wms_layers$owf$legend_link),
      position = "topright") %>%
  
  ## shipwrecks WMS --------
  addWMSTiles(
    baseUrl = wms_layers$shipwrecks$wms_base, layers = wms_layers$shipwrecks$wms_layer_name,
    options = WMSTileOptions(format = "image/png", transparent = T),
    group = "Shipwrecks inside the Belgian North Sea") %>%

# hide some layers at the start -------------------------------------------
  hideGroup(c("Bathymetry (multicolor)", "Seabed substrates", "Sea convention polygons", "Marine Spatial Plans", "Submarine Power Cables (SPC)"))
  
#map_WMS_EDITO

# add Legend(s) -----------------------------------------------------------

## Note: Layers will be listed in the toggle on/off (overlaygroups) in the order of appearance in `legend_map`

legend_map <- list(
  "Offshore Wind Farms (OWF)" = "legend-owf",
  "Submarine Power Cables (SPC)" = "legend-spc",
  "Marine Spatial Plans" = "legend-msp",
  "Sea convention polygons" = "legend-sea_conventions",
  "Seabed substrates" = "legend-seabedhabitats",
  "Bathymetry (multicolor)" = "legend-bathy"
  # add more: "EEZ" = "legend-eez", ...
)

overlay_sections <- list(
  "Management layers" = c("MSP", "Offshore Wind Farms (OWF)", "Submarine Power Cables (SPC)", "Sea convention polygons"),
  "Natural layers"    = c("Bathymetry (multicolor)", "Seabed substrates")
)

map_WMS_EDITO_legend <- 
  map_WMS_EDITO %>%

# Layer control -----------------------------------------------------------
    addLayersControl(
      baseGroups = c("Open Street Map", "EMODnet Bathymetry", "CartoDB.Positron"),
      overlayGroups = names(legend_map),
      options = layersControlOptions(collapsed = FALSE),
      position = "bottomleft"
    ) %>%

  htmlwidgets::onRender(
    "
function(el, x, legends){
    var map = this;

    function showByLayer(layerName, visible){
      var id = legends[layerName];
      if(!id) return;
      var node = document.getElementById(id);
      if(!node) return;
      node.style.display = visible ? 'block' : 'none';
    }

    function syncFromLayerControl(){
      // layer control DOM is inside the widget element `el`
      var inputs = el.querySelectorAll('.leaflet-control-layers-overlays input[type=checkbox]');
      inputs.forEach(function(inp){
        var label = inp.parentElement; // <label> ... text node
        var name = (label && label.textContent) ? label.textContent.trim() : null;
        if(name && legends[name] !== undefined){
          showByLayer(name, inp.checked);
        }
      });
    }

    function addBaseHeadingOnce(){
      // avoid duplicating the heading if re-rendered
      if(el.querySelector('.base-heading')) return;

      var base = el.querySelector('.leaflet-control-layers-base');
      if(!base) return;

      var hd = document.createElement('div');
      hd.className = 'base-heading';
      hd.style.textAlign = 'center';
      hd.style.fontWeight = '600';
      hd.style.marginBottom = '4px';
      hd.textContent = 'Background map';

      base.prepend(hd);
    }

    // Hide all legends first
    Object.keys(legends).forEach(function(layerName){
      showByLayer(layerName, false);
    });

    // Initial sync AFTER Leaflet applies hideGroup + builds control
    requestAnimationFrame(function(){
      requestAnimationFrame(function(){
        addBaseHeadingOnce();
        syncFromLayerControl();
      });
    });

    // Keep in sync when toggling
    map.on('overlayadd', function(e){ showByLayer(e.name, true); });
    map.on('overlayremove', function(e){ showByLayer(e.name, false); });

    // Also re-sync when layer control changes (covers edge cases)
    map.on('layeradd layerremove', function(){
      addBaseHeadingOnce();
      syncFromLayerControl();
    });
  }
    ",
    data = legend_map
  )

# map_WMS_EDITO_legend


