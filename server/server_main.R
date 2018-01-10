# Server file containing the elements that deal with the "map" panel

## Definition of the reactive variable

# Variables obtained from the simulations 
sim_variable <- reactiveValues(
  power_out = c(),
  price_out = c(),
  price_perceived_out = c(),
  individual_results = c(),
  preference = matrix(0, dim(setup0$app_setup$agent_characteristic)[1],2)
)

# Variables needed to deal with the Leaflet map
map_variable <- reactiveValues(
  map_ini = mapGeneration(setup0$map_setup),
  map = mapGeneration(setup0$map_setup),
  trade_lines_characteristics = NULL,
  trades_displayed = FALSE,
  clicked_marker = NULL
)

# Lists containing all the parameters of the current setup
setup_variable <- reactiveValues(
  lp_setup = setup0$lp_setup,
  rci_setup = setup0$rci_setup,
  map_setup = setup0$map_setup,
  app_setup = setup0$app_setup
)


#######################################################################################################################################
## Simulation elements

# Update the reactive variables when a new setup is selected
observeEvent(input$select_setup,{
  new_setup <- input$select_setup
  new_setup <- setup_to_shiny(new_setup)
  
  # Preference
  sim_variable$preference = matrix(0, dim(new_setup$app_setup$agent_characteristic)[1],2)
  sim_variable$power_out = c()
  sim_variable$price_out = c()
  sim_variable$price_perceived_out = c()
  sim_variable$individual_results = c()
  # Map 
  map_variable$map_ini = mapGeneration(new_setup$map_setup)
  map_variable$map = mapGeneration(new_setup$map_setup)
  map_variable$trades_displayed <- FALSE
  # Variables
  setup_variable$lp_setup = new_setup$lp_setup
  setup_variable$rci_setup = new_setup$rci_setup
  setup_variable$map_setup = new_setup$map_setup
  setup_variable$app_setup = new_setup$app_setup
})

# Solve the optimization problem when the "Run simulation button is clicked"
# Adapt all the reactive variables 
observeEvent(input$solverButton, {
  withBusyIndicatorServer("solverButton", {
    
    optimizationResults <- RCIsolve(sim_variable$preference, setup_variable$rci_setup)
    sim_variable$power_out <- optimizationResults$Power_out
    sim_variable$price_out <- optimizationResults$Price_out
    
    map_update_results <- mapUpdate(map = map_variable$map_ini,setup_variable$map_setup, sim_variable$power_out, sim_variable$price_out)
    map_variable$map <- map_update_results$map
    map_variable$trade_lines_characteristics <- map_update_results$trade_lines_characteristics
    map_variable$trades_displayed <- TRUE
    
    sim_variable$individual_results <- individualResultAnalysis(sim_variable$power_out,
                                                                sim_variable$price_out,
                                                                setup_variable$app_setup$agent_characteristic)
  })
})

#######################################################################################################################################
## Map elements

# Display the map 
output$P2Pmap <- renderLeaflet({
  # Initialize the leaflet map:
  map_variable$map
})

# When a click is made on an agent marker, change the line trades characteristic to focus on this agent
# Also select this agent for the "Agent characteristic" box on the right 
observeEvent(input$P2Pmap_marker_click,{
  # Change the focus on the right information panel
  agent_ID <- input$P2Pmap_marker_click$id
  updateSelectInput(session, inputId =  "GraphicalOptions_selectAgent",
                    selected = agent_ID)
  
  if (map_variable$trades_displayed){
    # Update the polylines opacity in order to focus on the selected agent trades
    agent_characteristic <- setup_variable$map_setup$agent_characteristic
    power_trade <- map_variable$trade_lines_characteristics$power_trade
    line_weight <- map_variable$trade_lines_characteristics$line_weight
    line_color <- map_variable$trade_lines_characteristics$line_color
    labels <- map_variable$trade_lines_characteristics$labels
    
    agent_selected_trades <- which(power_trade == agent_ID,arr.ind = TRUE)[,1]
    
    proxy <- leafletProxy("P2Pmap")
    
    for (i in 1:dim(power_trade)[1]){
      
      if (i %in% agent_selected_trades){
        opacity <- 1
        dashArray <- 20
      } else {
        opacity <- 0.4
        dashArray <- NULL
      }
      proxy %>% addPolylines(lng=~LONG,lat=~LAT,data=agent_characteristic[power_trade[i,1:2],],
                             color= line_color[i], weight = line_weight[i], opacity = opacity,
                             dashArray = dashArray,
                             label = HTML(labels[i]), layerId = as.character(i))
    }
  }
  
  map_variable$clicked_marker <- TRUE
})

# Necessary to make the difference between a click on the map and a click on a marker
observeEvent(input$P2Pmap_click,{
  map_variable$clicked_marker <- NULL
})

# When a click is made on the map, change the trades lines for them to have the same characteristics
observe({
  if(is.null(map_variable$clicked_marker) & map_variable$trades_displayed){
    #Reset polylines with same opacity 
    agent_characteristic <- setup_variable$map_setup$agent_characteristic
    power_trade <- map_variable$trade_lines_characteristics$power_trade
    line_weight <- map_variable$trade_lines_characteristics$line_weight
    line_color <- map_variable$trade_lines_characteristics$line_color
    labels <- map_variable$trade_lines_characteristics$labels
    
    proxy <- leafletProxy("P2Pmap")
    
    for (i in 1:dim(power_trade)[1]){
      proxy %>% addPolylines(lng=~LONG,lat=~LAT,data=agent_characteristic[power_trade[i,1:2],],
                             color= line_color[i], weight = line_weight[i], opacity = 0.6,
                             label = HTML(labels[i]), layerId = as.character(i))
    }
  }
})

#######################################################################################################################################
## Information elements

## KPI top boxes 

# Update the top "Average distance preference" box
output$averagePreference <- renderValueBox({
  valueBox(
    paste0(format(round(mean(sim_variable$preference[setup_variable$app_setup$agent_characteristic$BEHAVIOR == "cons",1])*100, 1), nsmall = 1), "%"), 
    "Average distance preference", icon = icon("exchange"),
    color = "light-blue"
  )
})

# Update the top "Average emission preference" box
output$averagePreference2 <- renderValueBox({
  valueBox(
    paste0(format(round(mean(sim_variable$preference[setup_variable$app_setup$agent_characteristic$BEHAVIOR == "cons",2])*100, 1), nsmall = 1), "%"), 
    "Average emission preference", icon = icon("recycle"),
    color = "orange"
  )
})

## Agent characteristic right box

# Define the agents that can be selected with selectInput button
output$GraphicalOptions_selectAgent<- renderUI({
  agent_characteristic <- setup_variable$app_setup$agent_characteristic
  
  choices <- agent_characteristic$AGENT_ID
  names(choices) <- agent_characteristic$NAME
  
  selectInput(inputId = "GraphicalOptions_selectAgent", label = NULL,
              choices = choices, selected = 1)
})

# Reactive variable containing the characteristics of the selected agent
agent_results<-reactive({ 
  agent_selected_ID <- input$GraphicalOptions_selectAgent
  agent_results <- sim_variable$individual_results[agent_selected_ID,]
  return(agent_results)
}) 

# Define the plot title depending of the type of the selected agent
output$plot_title <- renderText({
  ifelse(agent_results()$behavior == "prod", "Production Repartition:","Consumption repartition:")
})

# Define the text depending of the type of the selected agent
output$power_title <- renderText({
  ifelse(agent_results()$behavior == "prod", "Total Production:","Total Consumption:")
})

# Depending of the type of the selected agent print the consumption or the production
output$agent_power <- renderText({
  ifelse(agent_results()$behavior == "prod", 
         paste(as.numeric(format(round(agent_results()$total_individual_power,2), nsmall = 2)), "kWh"),
         paste(-as.numeric(format(round(agent_results()$total_individual_power,2), nsmall = 2)), "kWh"))
})

# Define the type text depending of the type of the selected agent
output$agent_type <- renderText({
  ifelse(agent_results()$behavior == "prod", "Producer","Consumer")
})

# Preferences of the selected agent
output$agent_preferences <- renderText({
  preferences <- sim_variable$preference[agent_results()$agent_id,]
  paste("Distance:",preferences[1], "Emissions:", preferences[2])
})

# Define the plot representing the local and green consumption repartition of the selected agent 
output$individual_plot <- renderPlotly({
  
  agent_results <- agent_results()
  
  if (is.null(agent_results)){
    p <- NULL
  } else {
    
    m <- list(
      l = 1,
      r = 1,
      b = 1,
      t = 1,
      pad = 1
    )
    
    behavior_coeff <- ifelse(agent_results$behavior == "prod", 1,-1)
    
    if (input$selectPlotType == 1){ # Make the "local vs imported" plot
      
      title <- ifelse(behavior_coeff == 1, "Production repartition", "Consumption repartition")
      
      p <- plot_ly(labels = c("Local power", "Imported power"), 
                   values = c(as.numeric(format(round(behavior_coeff*agent_results$total_individual_local_power,2), nsmall = 2)),
                              as.numeric(format(round(behavior_coeff*(agent_results$total_individual_power- agent_results$total_individual_local_power),2), nsmall = 2))),
                   width = 150, height = 150,
                   marker = list(colors = c("#d95f0e", "#2c7fb8")), 
                   showlegend = FALSE,
                   type = 'pie') %>%
        layout(title = FALSE,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               autosize = FALSE,
               margin = m) %>%
        config(displayModeBar = FALSE) 
      p$elementId <- NULL
    } else { # Make the "green vs conventional" plot
      title <- ifelse(behavior_coeff == 1, "Production repartition", "Consumption repartition")
      
      p <- plot_ly(labels = c("Conventional Power", "Green Power"), 
                   values = c(as.numeric(format(round(behavior_coeff*agent_results$total_conv_cons_power,2), nsmall = 2)),
                              as.numeric(format(round(behavior_coeff*(agent_results$total_individual_power - agent_results$total_conv_cons_power),2), nsmall = 2))),
                   width = 150, height = 150,
                   marker = list(colors = c("#d95f0e", "#2c7fb8")), 
                   showlegend = FALSE,
                   type = 'pie') %>%
        layout(title = FALSE,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               autosize = FALSE,
               margin = m) %>%
        config(displayModeBar = FALSE) 
      p$elementId <- NULL
    }
  }
  return(p)
})

## Information panels

# output needed to display all the informations panel
output$info_simulation <- renderUI({
  icon("info-circle")
})

addPopover(session, "info_simulation", "Simulation informations", placement = "bottom",
          content = 
"<p> <b>Solver :</b> RCI (iterative process) developped by the ELMA Group (DTU).</p> 
<p> <b>General information:</b> The solver will solve the optimisation
problem given the setup characterisics and the set of preferences defined for each consumer.  </p>
<p> <b>Remark:</b> If the local preferences are set to zero for most of the consumers, the simulation takes some time (~10s).  </p>",
          trigger = 'click',
          options=list(container= "body")
)

output$info_preference <- renderUI({
  icon("info-circle")
})

addPopover(session, "info_preference", "Preference informations", placement = "bottom",
           content = 
"<p> <b>General information:</b> It is possible to modify the preferences of each consumer (defined by a value between 0 and 1).</p>
<p> <b>Preferences:</b> <br></br> <u>Distance</u>: the more important the preference is, the more the agent wants to consume local power. 
<br></br> <u>Emission</u>: the more important the preference is, the more the agent wants to consume green energy.</p>
<p> <b>Remark:</b> The top red slidder changes the selected preference of all the consumers of the selected group.",
           trigger = 'click',
           options=list(container= "body"))


######################################################################################################################
## BSmodal window: Preference Selection 

# Observe Event related to the preference sliders of each agents
lapply(
  1:100,                              # TEMPORARY SOLUTION: Have to figure out how to create the exact number of 
  FUN = function(agent_id){           #observe event related to the number of preference sliders
    observeEvent(input[[paste("slider",agent_id,"1",sep = "_")]], {
      sim_variable$preference[agent_id,1] <- input[[paste("slider",agent_id,"1",sep = "_")]]
    })
  }
)

lapply(
  1:100,                              # TEMPORARY SOLUTION: Have to figure out how to create the exact number of 
  FUN = function(agent_id){           #observe event related to the number of preference sliders
    observeEvent(input[[paste("slider",agent_id,"2",sep = "_")]], {
      sim_variable$preference[agent_id,2] <- input[[paste("slider",agent_id,"2",sep = "_")]]
    })
  }
)

# lapply(
#   1:2,                              # TEMPORARY SOLUTION: Have to figure out how to create the exact number of 
#   FUN = function(village_id){           #observe event related to the number of preference sliders
#     observeEvent(input[[paste("selectPreference",village_id,sep = "_")]], {
#       showInd <- as.numeric(input[[paste("selectPreference",village_id,sep = "_")]])
#       hideInd <- 1:2
#       hideInd <- hideInd[!(hideInd == showInd)]
#       
#       shinyjs::hideElement(id = paste("preferenceChoice", as.character(hideInd),village_id, sep = "_"))
#       shinyjs::showElement(id = paste("preferenceChoice", as.character(showInd),village_id, sep = "_"))
#     })
#   }
# )

# Display the right preference's sliders (Concerning the local or the emission preference)
lapply(
  1:2,                              # TEMPORARY SOLUTION: Have to figure out how to create the exact number of 
  FUN = function(village_id){           #observe event related to the number of preference sliders
    observeEvent(input$selectPreferences_2, {
      showInd <- as.numeric(input$selectPreferences_2)
      hideInd <- 1:2
      hideInd <- hideInd[!(hideInd == showInd)]
      
      shinyjs::hideElement(id = paste("preferenceChoice", as.character(hideInd),village_id, sep = "_"))
      shinyjs::showElement(id = paste("preferenceChoice", as.character(showInd),village_id, sep = "_"))
    })
  }
)

# Update all the agent sliders when a global preference slider is modified
observeEvent(input$slider_global_preference, {
  agent_characteristic <- setup_variable$app_setup$agent_characteristic
  village_selected <- input$preferenceTabs
  preference_selected <- input$selectPreferences_2
  
  slider_to_modify_id <- agent_characteristic$AGENT_ID[agent_characteristic$GROUP_ID == village_selected & agent_characteristic$BEHAVIOR == "cons"]
  slider_to_modify_id <- paste("slider",slider_to_modify_id,preference_selected,sep = "_")
  
  lapply(slider_to_modify_id,function(slider_to_modify_id){
    updateSliderInput(session, slider_to_modify_id,value = input$slider_global_preference)
  })
})


# Observe related to the type of preference to display in the window
# observe({
#   showInd <- as.numeric(input$selectPreference)
#   hideInd <- 1:2
#   hideInd <- hideInd[!(hideInd == showInd)]
#   
#   shinyjs::hideElement(id = paste("preferenceChoice_", as.character(hideInd), sep = ""))
#   shinyjs::showElement(id = paste("preferenceChoice_", as.character(showInd), sep = ""))
# })


# output$individual_results <- renderUI({
#   agent_selected_ID <- input$GraphicalOptions_selectAgent
#   agent_results <- sim_variable$individual_results[agent_selected_ID,]
#   agent_preferences <- sim_variable$preference[as.integer(agent_selected_ID),]
#   
#   p(paste("<b>Type :</b>", ifelse(agent_results$behavior=="prod", "Producer", "Consumer")))
#   p(paste("Preferences:")),
#   p(paste("Distance: ", agent_preferences[1], "Emissions: ", agent_preferences[2]))
#   #HTML(paste(str1,str2,str3,sep = '<br/>'))
# })

# Main block that deal with the BSModal window organisation
# Display the sliders for all the "villages" (group)
output$sliders <- renderUI({
  agent_characteristic <- setup_variable$app_setup$agent_characteristic
  village_names <- setup_variable$app_setup$village_names
  village_ids <- setup_variable$app_setup$village_ids

  # Function for the local preference sliders
  village_tabset_pref1 <- function(village_id){
    lapply(agent_characteristic$AGENT_ID[agent_characteristic$GROUP_ID == village_id & agent_characteristic$BEHAVIOR == "cons"], function(agent_id) {
            sliderInput(inputId = paste("slider",agent_id,"1",sep = "_"),
                        label = agent_characteristic$NAME[which(agent_characteristic$AGENT_ID == agent_id)],
                        min = 0, max = 1, value = 0)
    })
  }
  
  # Function for the emission preference sliders
  village_tabset_pref2 <- function(village_id){
    lapply(agent_characteristic$AGENT_ID[agent_characteristic$GROUP_ID == village_id & agent_characteristic$BEHAVIOR == "cons"], function(agent_id) {
            sliderInput(inputId = paste("slider",agent_id,"2", sep = "_"),
                        label = agent_characteristic$NAME[which(agent_characteristic$AGENT_ID == agent_id)],
                        min = 0, max = 1, value = 0)
    })
  }
  
  myTabs = lapply(village_ids, function(village_id) {
    tabPanel(
      title = village_names[village_id], 
      value = village_id,
      fluidRow(
        column(12,
               #shinyjs::hidden(
                 div(id = paste("preferenceChoice_1",village_id,sep="_"),
                     village_tabset_pref1(village_id)
                     ),
                 #),
               shinyjs::hidden(
                 div(id = paste("preferenceChoice_2",village_id,sep="_"),
                     village_tabset_pref2(village_id)
                 )
               ),
               style = "overflow-y:scroll; max-height: 450px"
               )
        #column(3,
               #fluidRow(
                 # box(
                 #   title = "Type of preference", width = 12, solidHeader = TRUE, status = "primary",
                 #   selectInput(paste("selectPreference",village_id,sep = "_"), label = "",
                 #               choices = list("Local Preference" = 1, "Emission Preference" = 2),
                 #               selected = 1)
                 # )
              # )
               #fluidRow(
               #   box(
               #     title = "Global preference", width = 12, solidHeader = TRUE, status = "warning",
               #     "This slider modify the preference of all the group's agents",
               #     sliderInput(inputId = "slider_global_preference",
               #                 label = "",
               #                 min = 0, max = 1, value = 0.5)
               #   )
               # )
               #)
      )
    )
  })
  do.call(tabsetPanel, c(myTabs, id = "preferenceTabs"))
})



