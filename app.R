library(shiny)
library(shinyjs)
library(shinydashboard)
library(leaflet)
library(htmltools)
# library(Rmosek)
library(R.matlab)
library(RColorBrewer)
library(geosphere)
library(plyr)
library(shinyBS)
library(shinyjs)
library(plotly)
library(rintrojs)
#library(shinycssloaders)

#path <- "/Users/antoine/Documents/R/Shiny Apps/ShinyApp_Project"

source('helper/helper.R', local = TRUE)
source('helper/helper_2.R', local = TRUE)

setup0 <- setup_to_shiny('setup1')    # Temporary: initialize a setup at the opening of the app 

sidebar <- dashboardSidebar(
  selectInput('select_setup', 'Select setup', c("Setup 0" = "setup0", "Setup 1" = "setup1")),
  sidebarMenu(
    menuItem("Map", tabName = "main", icon = icon("map-o")),
    menuItem("Analysis", tabName = "analysis", icon = icon("area-chart")),
    menuItem("Information", icon = icon("info-circle"), tabName = "about")
  )
)

body <- dashboardBody(
  useShinyjs(),
  introjsUI(),
  tags$style(appCSS),
  tabItems(
    source("ui/ui_main.R",  local = TRUE)$value,
    source("ui/ui_about.R",  local = TRUE)$value
  )
)

ui <- shinyUI(
  dashboardPage(skin="green",
                dashboardHeader(title = "Peer-To-Peer Markets"),
                dashboardSidebar(sidebar),
                body
  )
)

server <- function(input, output, session) {
  # Include the logic (server) for each tab
  source("server/server_main.R",  local = TRUE)$value
}

shinyApp(ui = ui, server = server, options = list(display.mode = "showcase"))