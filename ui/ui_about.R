source("ui/ui_text.R")

tabItem(tabName = "about",
        
        h2("First Version"),
        br(),
        
        
        tabsetPanel(
          tabPanel("P2P markets description",
                   
                   market_description()
                   
                  ),
          tabPanel("More info for 'energy geeks'"),
          tabPanel("App guidelines")
        ),
        
        
       
        
        
        h3("App guidelines:"),
        
        h4("Simulation"),
        p( tags$b("Solver:"), "RCI (iterative process) developped by the ELMA Group (DTU)."),
        p( tags$b("General information:"), "The solver will solve the optimisation problem given the setup characterisics and the set of preferences defined for each consumer."),
        p( tags$b("Remark:"), "If the local preferences are set to zero for most of the consumers, the simulation takes some time (~10s)."),
        
        h4("Preferences"),
        p( tags$b("General information:"), "It is possible to modify the preferences of each consumer (defined by a value between 0 and 1)."),
        p( tags$b("Preference types:")),
        tags$ul(
          tags$li("Distance: the more important the preference is, the more the agent wants to consume local power."),
          tags$li("Emission: the more important the preference is, the more the agent wants to consume green energy.")
          ),
        p( tags$b("Remark:"), "The top red slidder changes the selected preference of all the consumers of the selected group."),
        br(),
        
        h3("Authors:"),
        p("Pierre Pinson, Tiago Sousa, Antoine Rosin")
)

