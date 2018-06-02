library(RColorBrewer)
library(leaflet)
library(sf)


sf_fin <- sf::read_sf("https://github.com/avoindata/mml/raw/master/data/mml/kuntajako_2017_maa_alueet_sote.gpkg") %>% 
  dplyr::filter(!is.na(sote_alue))
  
sf_fin_wgs84 <- sf::st_transform(sf_fin, "+init=epsg:4326")

epsg3067 <- leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "EPSG:3067",
  proj4def = "+proj=utm +zone=35 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
  resolutions = 2^(16:7))


factpal <- colorFactor(brewer.pal(n = length(unique(sf_fin$sote_alue)), name = "Paired"), 
                       sf_fin$sote_alue)

leaflet(sf_fin_wgs84, options = leafletOptions(crs = epsg3067)) %>%
  addPolygons(weight = 0.5, color = "white", opacity = 1,
              fillOpacity = 0.9, smoothFactor = 0.5,
              fillColor = ~factpal(sote_alue), label = ~paste(NAMEFIN, sote_alue),
              labelOptions = labelOptions(direction = "auto"))
