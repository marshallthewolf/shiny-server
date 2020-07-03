library(leaflet)
library(shiny)
library(raster)
library(spdplyr)
library(rgeos)
library(rgdal)
library(viridis)
library(mapview)
library(rmapshaper)

gap <- readOGR("./files/gap_simpl.shp")
spp.heat <- raster("./files/spp_rast2.5k.tif")
spp.needpa <- raster("./files/spp_rast2.5k_needPAs.tif")

##### Basic Leaflet Build ##########
#################################
spp.heat[spp.heat == 0] <- NA
spp.needpa[spp.needpa == 0] <- NA

bins <- c(1,2,3,5,10,16)
pal <- c('#ffdb00', '#ffa904','#ee7b06','#a12424', '#400b0b')
pal.bin  <- colorBin(pal, values(spp.heat),
                     na.color = "transparent", bins = bins)
pal.need <- c("#fcc5c0", "#f768a1", "#dd3497", "#ae017e", "#34004c" )
pal.need.bin <- colorBin(pal.need, values(spp.needpa),
                         na.color = "transparent", bins = bins)

labs <- c("1","2-3","4-5","6-9","10+")


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"))


server <- function(input, output){
  output$map <- renderLeaflet({
    leaflet(gap) %>% 
      # Base Groups
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Imagery") %>%
      # Add data
      addRasterImage(spp.heat, colors = pal, opacity = 0.8,
                     layerId= "TE Species", group = "TE Species") %>%
      addRasterImage(spp.needpa, colors = pal.need, opacity = 0.8,
                     layerId= "Species Requiring Additional Protected Areas Coverage",
                     group = "TE Need Coverage") %>%
      addPolygons(color = "#3633FF", stroke = F, group = "Protected Areas", 
                  fillOpacity = 0.8, weight = 1) %>%
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner", "ESRI Imagery"),
        overlayGroups = c("TE Species", "TE Need Coverage", "Protected Areas"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("TE Species") %>%
      # Add Legend
      addLegend(colors = pal, labels = labs, position = "bottomright",
                title = "# of Species", group = "TE Species") %>%
      addLegend(colors = pal.need, position = "bottomright",
                title = "# of Species", group = "TE Need Coverage",
                labels = labs) %>%
      addLegend(position = "bottomright", colors = "#3633FF", 
                label = "Protected Areas",group = "Protected Areas" )%>%
      
      #mouse over
      
      leafem::addImageQuery(spp.needpa, type="mousemove", layerId = "Species Requiring Additional Protected Areas Coverage", 
                            position = "bottomleft")
  })
  
  #pop up on click lat/long
  observeEvent(input$map_click, {
    click <- input$map_click
    text <- paste(sep = "<br/>", "<b>Latitude, Longtitude </b>",
                  paste(sep = ",", round(click$lat, 6),  round(click$lng, 6)))
    
    
    
    proxy <- leafletProxy("map")
    proxy %>% clearPopups() %>%
      addPopups(click$lng, click$lat, text)
  })
}


shinyApp(ui, server)




