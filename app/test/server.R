#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

# Read Tree Data
trees <- read.csv("C:/Users/jessm/OneDrive/Documents/Columbia University/Fall 2019/STAT 5243 - Applied Data Science/Projects/Project 2/Data/trees.csv", header = TRUE, stringsAsFactors = FALSE)

treeCountsGroupedByZipCode <- trees %>% group_by(zipcode) %>% tally()
treeCountsGroupedByZipCode <- as.data.frame(treeCountsGroupedByZipCode)
colnames(treeCountsGroupedByZipCode) <- c("ZIPCODE", "value")

labels_sales <- sprintf(
  "Zip Code: <strong>%s</strong><br/>Average Annual Salary (AAS): <strong>$%g/yr<sup></sup></strong>",
  as.character(treeCountsGroupedByZipCode$ZIPCODE), treeCountsGroupedByZipCode$value
) %>% lapply(htmltools::HTML)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$numberOfTreesHeatMap <- renderLeaflet({
    m <- leaflet(data = treeCountsGroupedByZipCode) %>%
      addTiles() %>%
      setView(lng=-73.98928, lat = 40.75042 , zoom =12)%>%
      addProviderTiles("Stamen.Toner")})
  pal <- colorNumeric(
    palette = "Reds",
    domain = treeCountsGroupedByZipCode$value
  )
  leafletProxy("numberOfTreesHeatMap",data=treeCountsGroupedByZipCode)%>%
    addPolygons(layerId = ~ZIPCODE,
                stroke = T, weight=1,
                fillOpacity = 0.95,
                color = ~pal(value),
                highlightOptions = highlightOptions(color='#ff0000', opacity = 0.5, weight = 4, fillOpacity = 0.9,bringToFront = TRUE, sendToBack = TRUE),label = labels_sales,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"))%>%
    addLegend(pal = pal, values = ~value, opacity = 1)
  # median=jobs$medium
  
})
