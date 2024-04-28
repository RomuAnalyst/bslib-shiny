library(ggplot2)
library(bslib)
library(shiny)
library(crosstalk)
library(plotly)
library(leaflet)
library(apexcharter)
library(lorem)
library(shinyWidgets)
library(dplyr)
library(shinythemes)
library(sf)
library(reactable)
library(palmerpenguins)
library(bsicons)


ui <- page_fixed(
  value_box(
    title = "Heure actuelle",
    value = textOutput("time"),
    showcase = bs_icon("clock")
  )
)

server <- function(input, output) {
  output$time <- renderText({
    invalidateLater(1000)
    format(Sys.time())
  })
}

shinyApp(ui, server)