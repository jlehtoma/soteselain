library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(leaflet)
library(raster)
library(RColorBrewer)
library(sf)

sf_fin <- sf::read_sf("data/kuntajako_2017_maa_alueet_sote.gpkg") %>% 
  dplyr::filter(!is.na(sote_alue))

# Group by SOTE regions
sf_fin_sote <- sf_fin %>% 
  dplyr::group_by(sote_alue) %>% 
  dplyr::summarize()

dbHeader <- dashboardHeader(title = "SOTE-selain")

ui <- dashboardPage(
  skin = "black",
  dbHeader,
  dashboardSidebar(
    disable = TRUE
  ),
  dashboardBody(
    includeCSS("www/style.css"),
    box(width = 4, title = "SOTE-alueet",
        checkboxInput("show_munis", "Näytä kunnat", value = TRUE),
        withSpinner(leafletOutput("sote_alueet", height = 800),
                    type = 4)
    ),
    box(width = 8, title = "Tilastotiedot")
  )
)

server <- function(input, output, session) {
  
  update_map <- function(session, input, output) {
    if (input$show_munis) {
      sf_data <- sf_fin
      labeller <- paste0(sf_data$NAMEFIN, ", ", sf_data$sote_alue)
    } else {
      sf_data <- sf_fin_sote
      labeller <- sf_data$sote_alue
    }
    
    sf_fin_wgs84 <- sf::st_transform(sf_data, "+init=epsg:4326")
    
    epsg3067 <- leafletCRS(
      crsClass = "L.Proj.CRS",
      code = "EPSG:3067",
      proj4def = "+proj=utm +zone=35 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
      resolutions = 2^(16:7))
    
    
    factpal <- colorFactor(brewer.pal(n = length(unique(sf_fin$sote_alue)), name = "Paired"), 
                           sf_fin$sote_alue)
    
    map <- leaflet(sf_fin_wgs84, options = leafletOptions(crs = epsg3067)) %>%
      addPolygons(weight = 0.5, color = "white", opacity = 1,
                  fillOpacity = 0.9, smoothFactor = 0.5,
                  fillColor = ~factpal(sote_alue), label = labeller,
                  labelOptions = labelOptions(direction = "auto")) %>% 
      addLegend(pal = factpal, values = ~sote_alue, opacity = 1,
                position = "topleft", title = "")
    return(map)
  }
  
  # observer_open_review_pdf ---------------------------------------------------
  observeEvent(input$show_munis, {
    update_map(session, input, output)
  })
  
  output$sote_alueet <- renderLeaflet({
    update_map(session, input, output)
  })
}

shinyApp(ui, server)
