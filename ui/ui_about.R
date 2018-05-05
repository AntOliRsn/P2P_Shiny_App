source("ui/ui_text.R")

tabItem(tabName = "about",
        
        h2("First Version"),
        br(),
        
        
        tabsetPanel(
          tabPanel("P2P markets description",
                   
                   market_description()
                   
                  ),
          tabPanel("More info for 'energy geeks'",
                   
                   energy_geeks()
                   
                   ),
          tabPanel("App guidelines",
                   
                   app_guidelines()
                   
                   )
        ),
        
        h3("Authors:"),
        p("Pierre Pinson, Tiago Sousa, Antoine Rosin")
)

