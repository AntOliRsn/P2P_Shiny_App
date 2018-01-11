# Shiny Peer-to-Peer electricity market Application 

> *Antoine Rosin, Tiago Tousa and Pierre Pinson for [DTU Elekro](http://www.elektro.dtu.dk/english)*

The aim of the App is to raise the awareness of people about P2P electricity market. The user can visualize the exchange of energy between agents for various setups, and see the impact of individual preferences such as consuming green or local.

# Organization
The code is organized in four main folders:
- ui/ : containing all the ui*.R files that define the layout
- server/ : containing the server*.R files that deal with the reactive variables 
- helper/ : containing the helper*.R files storing various function needed to define the Leaflet map, solve the optmiization problems, analyze the results, ...
- starter/ : containing the starter_setup*.R files that defined the different setups used.

The app.R file gathered all the elements to lunch the app.

# Information

### Setup file requirments
A setup file must define 3 variables:
- an agent_characteristic data frame containing for each agent:
  - LONG: the longitude of the agent
  - LAT: the latitude of the agent 
  - NAME: the name of the agent
  - GROUP: the group of the agent (village/community/group name)
  - TYPE: the type of the agent: 1 = consumer, 2 = conventional unit, 3 = wind turbine 4 = solar plant, 5  hydro
- a prop_tot matrix containing for each agent:
  - first column: the minimum production capacity (if the agent is a consumer it will be negative)
  - second column: the maximum production capacity
  - third column: the slope of the marginal cost/demand function
  - fourth column: the origin of the marginal cost/demand function
- a prop_emi vector containing for each agent the emission factor

### Adding a new setup




