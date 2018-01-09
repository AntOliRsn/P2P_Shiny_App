tabItem(tabName = "main",
        fluidRow(
          column(width = 9,
                 fluidRow(
                       valueBoxOutput("averagePreference", width = 6),
                       valueBoxOutput("averagePreference2", width = 6)
                 )
          ),
          column(width = 3,
                 fluidRow(
                   box(title = "Simulation", width = 12,solidHeader = TRUE, status = "danger",
                       column(2,
                              uiOutput("info_simulation")
                       ),
                       column(10, align="center",
                              withBusyIndicatorUI(
                              actionButton("solverButton", label = "Run simulation")
                              )
                       )
                             #tags$style(type='text/css', "#button { vertical-align: middle; height: 50px; width: 100%; font-size: 30px;}")
                       )
                   )
                 )
        ),
        fluidRow(
          column(width = 9,
                 fluidRow(
                   box(width = NULL, solidHeader = TRUE,
                       leafletOutput("P2Pmap", height = 540)
                   )
                 )
          ),
          column(width = 3,
                 fluidRow(
                   box(title = "Preference", width = 12, status = "primary",solidHeader = TRUE,
                       column(2,
                              uiOutput("info_preference")
                       ),
                       column(10, align="center",
                         actionButton("individualPreferences", label = "Modifiy Preferences"),
                         tags$style(type='text/css', "#button { vertical-align: middle; height: 50px; width: 100%; font-size: 30px;}")
                       )
                   )
                 ),
                 fluidRow(
                   box(title = "Agent Characteristic", width = 12, status = "primary",solidHeader = TRUE,
                       uiOutput("GraphicalOptions_selectAgent"),
                       #uiOutput("individual_results"),
                       div(strong("Type:"), align = "center"),
                       div(textOutput("agent_type"), align = "center"),
                       div(strong("Preferences:"), align = "center"),
                       div(textOutput("agent_preferences"), align = "center"),
                       div(strong(textOutput("power_title")), align = "center"),
                       div(textOutput("agent_power"), align = "center"),
                       selectInput(inputId = "selectPlotType", label = NULL,
                                   choices = list("Local vs Import" = 1, "Conventionnal vs Sustainable" = 2),
                                   selected = 1),
                       div(strong(textOutput("plot_title")),align = 'center'),
                       div(plotlyOutput("individual_plot",width = "150px", height = "150px"), align = "center")
                   )
                 )#,
                 #textOutput("test")
          )
        ),
        fluidRow(
          div(tags$b("Authors:"), " Pierre Pinson, Tiago Sousa, Antoine Rosin")
        ),
        
        # Individual Preferences modal Panel Selection
        bsModal("modalExample", "Preference", "individualPreferences", size = "small",
                #column(6,
                introjsUI(),
                       selectInput(inputId = "selectPreferences_2", label = "Select Preference",
                                   choices = list("Distance Preference" = 1, "Emission Preference" = 2),
                                   selected = 1),
                  #),
                #column(6,
                       tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red}")),
                       sliderInput(inputId = "slider_global_preference",
                                   label = "Modify all the displayed preferences",
                                   min = 0, max = 1, value = 0),
                
                       #),
                uiOutput("sliders")
        )
)
