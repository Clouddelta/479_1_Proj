library(shiny)
library(bslib)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(sf)
library(gstat)
library(magrittr)
library(kriging)
library(raster)
library(scales)
library(usmap)
library(leaflet)
library(ggrepel)
library(scales)
library(patchwork)

# Data Preparation --------------------------------------------------------
source("preprocess.R")
source("kriging.R")


# Plot Function -----------------------------------------------------------
source("plot.R")



# UI ----------------------------------------------------------------------
ui = dashboardPage(
  header = dashboardHeader(title = "Parking Dashboard"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Map", icon = icon("map"), tabName = "map"),
      menuItem("Summary Plots", icon = icon("chart-bar"), tabName = "sum"),
      menuItem("Modeling", icon = icon("cubes"), tabName = "model"),
      menuItem("About", icon = icon("info"), tabName = "about")
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "map",
        box(width = 12, status = "primary",
          selectInput("citySelect", "Select a city", choices = c("All Across US", sort(unique(parking$City))))
        ),
        leafletOutput("leaflet", height = "60rem")
      ),
      tabItem(tabName = "sum",
        fluidRow(
          box(plotOutput("bar_top")),
          box(plotOutput("scatter"))
        )
      ),
      tabItem(tabName = "model",
        box(title = "Kriging Interpolation",
          p("We use Kriging method to interpolate the parking lot finding time for areas without data"),
          plotOutput("krig", width = "100%"),
          width = 12
        )
      ),
      tabItem(tabName = "about",
        includeMarkdown("www/about.md")
      )
    )
  )
)

# Server ------------------------------------------------------------------
server = function(input, output, session) {
  leaflet_ds = reactive(
    if(input$citySelect == "All Across US") parking
    else dplyr::filter(parking, City == input$citySelect)
  )
  output$leaflet = renderLeaflet(plot_leaflet(leaflet_ds()))
  output$krig = renderPlot(plot_krig())
  output$bar_top = renderPlot(plot_bar_top(county_mean))
  output$scatter = renderPlot(plot_scatter(county_mean))
}

shinyApp(ui, server)

