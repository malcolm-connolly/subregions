#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(sf)
library(RColorBrewer)
library(viridis)

subregions_shp <- readRDS("./www/subregions_shp.RData") 
subregions_shp0 <- readRDS("./www/subregions_shp0.RData") 

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Child poverty and climate risk in the Indian subcontinent"),

    # Sidebar 
    sidebarLayout(
      
      sidebarPanel(
        
        selectInput(inputId = "subregion.variable",
                    label=h3("Subregion variable"), 
                    choices = list("Risk of at least one extreme climate event (% of children)" = 42,
                                   "Risk of crop failures (% of children)" = 43,
                                   "Risk of tropical cyclones (% of children)" = 44,
                                   "Risk of droughts (% of children)"=45,
                                   "Risk of wildfires (% of children)" = 46,
                                   "Risk of river floods (% of children)" = 47,
                                   "Risk of heatwaves (% of children)" = 48,
                                   "Multidimensional child poverty: Prevalence (%)" = 3,
                                   "Multidimensional child poverty: Depth (# of deprivations)" = 4,
                                   "Climate exposure, Multiple Hazard Index 0-10 (weighted average)" = 9,
                                   "Human Development Index (0-1) (weighted average)" = 10
                                   ),
                    selected = 42),
        h3("Select country"),
        leafletOutput("Countrymap")
      ),

      # Show a plot of the generated distribution
      mainPanel(
          leafletOutput("Regionsmap")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$Countrymap_shape_click,{
    p <- input$Countrymap_shape_click
    print(p)
  })
  


  output$Countrymap <- renderLeaflet(
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      setView(lng = 80, lat = 25, zoom = 4) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(data = subregions_shp0,
                  fillOpacity = 0.5,
                  weight  = 1,
                  popup = ~country,
                  fillColor = subregions_shp0$country_col,
                  color = subregions_shp0$country_col,
                  layerId = ~country) %>%
      addControl(
        html = "", position = "bottomleft" # placeholder control to ensure position works
      ) %>%
      htmlwidgets::onRender("
    function(el, x) {
      // Move zoom control to bottom left
      this.zoomControl.setPosition('bottomright');
    }
  ")
  )
  
  output$Regionsmap <- renderLeaflet({
    if( is.null(input$Countrymap_shape_click[1]) ){
      temp.code <- c("India","*", 25,80 )
    } else {
      temp.code <- input$Countrymap_shape_click
    }
    
    if( is.null(input$subregion.variable) ){
      temp.num <- 42
    } else {
      temp.num <- as.numeric(input$subregion.variable)
    }
    
    pal <- colorNumeric(
      palette = "viridis",
      domain = range(subregions_shp[,temp.num] %>% as.data.frame() %>% drop_na() %>% select(1)))
    
    col <- subregions_shp[,temp.num] %>% names() %>% `[`(c(1))
    
    leaflet() %>%
      setView(lng = as.numeric(temp.code[4]), lat = as.numeric(temp.code[3]), zoom = 6) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(data = subregions_shp,
                  fillOpacity = 0.25,
                  popup = ~paste0(subregion_name, "<br>", round(subregions_shp[[col]],3)),
                  color = ~pal(subregions_shp[[col]])) %>%
      addPolylines(data = subregions_shp0[subregions_shp0$country == temp.code[1],],
                   fillOpacity = 0,
                   weight  = 2,
                   color = "red")%>%
      addLegend(position = "bottomright",
                pal = pal,
                values = range(subregions_shp[,temp.num] %>% as.data.frame() %>% drop_na() %>% select(1)))
    
  })
  
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
