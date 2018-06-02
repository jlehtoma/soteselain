library(leaflet)
library(raster)
library(pxweb)
library(RColorBrewer)
library(R.cache)
library(sf)
library(shiny)
library(shinydashboard)
library(shinycssloaders)

sf_fin <- sf::read_sf("data/kuntajako_2017_maa_alueet_sote.gpkg") %>% 
  dplyr::filter(!is.na(sote_alue))

df_fin <- sf_fin 
sf::st_geometry(df_fin) <- NULL

# Group by SOTE regions
sf_fin_sote <- sf_fin %>% 
  dplyr::group_by(sote_alue) %>% 
  dplyr::summarize()

# Get municipal statistics using cached version if available
query_params <- list(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/Kuntien_avainluvut/2017/kuntien_avainluvut_2017_aikasarja.px",
                     dims = list("Alue 2017" = c('*'),
                                 Tiedot = c('*'),
                                 Vuosi = c('*')),
                     clean = TRUE)
stat_data <- R.cache::loadCache(query_params)
if (!is.null(stat_data)) {
  message("Loaded cached data")
} else {
  stat_data <- get_pxweb_data(url = query_params[["url"]],
                              dims = query_params[["dims"]],
                              clean = query_params[["clean"]])
  R.cache::saveCache(stat_data, key = query_params, comment = as.character(Sys.Date()))
}

stat_variables <- c("Maakunnat", sort(unique(as.character(stat_data$Tiedot))))
stat_years <- sort(unique(as.character(stat_data$Vuosi)), decreasing = TRUE)

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
        fluidRow(
          column(width = 12,
                 selectInput("select_variable", "Muuttuja", 
                             choices = stat_variables)
          ),
          column(width = 12,
                 selectInput("select_year", "Vuosi", 
                             choices = stat_years)
          ),
          column(width = 12,
                 checkboxInput("show_munis", "Näytä kunnat", value = FALSE)
          )
        ),
        withSpinner(leafletOutput("sote_alueet", height = 800),
                    type = 4)  
    ),
    box(width = 8, title = "Tilastotiedot"
    )
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
    
    if (input$select_variable == "Maakunnat") {
      fill_variable <- "sote_alue"
      pal <- colorFactor(brewer.pal(n = length(unique(sf_data$sote_alue)), name = "Paired"), 
                         sf_data$sote_alue)
    } else {
      dat <- stat_data %>% 
        dplyr::filter(Tiedot == input$select_variable, 
                      Vuosi == as.numeric(input$select_year)) %>%
        dplyr::select(region = `Alue 2017`, variable = Tiedot, values) %>% 
        dplyr::right_join(df_fin, by = c("region" = "NAMEFIN")) %>% 
        dplyr::group_by(sote_alue) %>% 
        dplyr::summarise(
          values = sum(values)
        )
      
      pal <- colorNumeric("viridis", dat$values)
      
      fill_variable <- "values"
    }
    
    sf_data <- dplyr::left_join(sf_data, dat, by = c("sote_alue" = "sote_alue"))
    
    sf_fin_wgs84 <- sf::st_transform(sf_data, "+init=epsg:4326")
    
    epsg3067 <- leafletCRS(
      crsClass = "L.Proj.CRS",
      code = "EPSG:3067",
      proj4def = "+proj=utm +zone=35 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
      resolutions = 2^(16:7))
    
    map <- leaflet(sf_fin_wgs84, options = leafletOptions(crs = epsg3067,
                                                          zoomControl = FALSE)) %>%
      addPolygons(weight = 0.5, color = "white", opacity = 1,
                  fillOpacity = 0.9, smoothFactor = 0.5,
                  fillColor = pal(sf_fin_wgs84[[fill_variable]]), label = labeller,
                  labelOptions = labelOptions(direction = "auto")) 
      
      
    return(map)
  }
  
  # observer_show_munis -------------------------------------------------------
  observeEvent(input$show_munis, {
    update_map(session, input, output)
  })
  
  # observer_select_variable ---------------------------------------------------
  observeEvent(input$select_variable, {
    update_map(session, input, output)
  })
  
  output$sote_alueet <- renderLeaflet({
    update_map(session, input, output)
  })
}

shinyApp(ui, server)
