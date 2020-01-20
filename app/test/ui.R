#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Number of Trees in NYC"),
  
  # Implement Tab Style
  id = "inTabset",
  
  # Number of Trees Tab Label
  tabPanel("Number of Trees Heat Map", leafletOutput("numberOfTreesHeatMap", height = 1000)
  
  )
))
