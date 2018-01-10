# SETUP FILE: 1
# CHARACTERISTICS : 4 agents, 1 village

# REMEMBER: 
# A setup file must define 3 variables:
# - an agent_characteristic data frame containing for each agent:
#   - LONG: the longitude of the agent
#   - LAT: the latitude of the agent 
#   - NAME: the name of the agent
#   - GROUP: the group of the agent (village/community/group name)
#   - TYPE: the type of the agent: 1 = consumer, 2 = conventional unit, 3 = wind turbine
#                                  4 = solar plant, 5  hydro.
# - a prop_tot matrix containing for each agent:
#   - first column: the minimum production capacity (if the agent is a consumer it will be negative)
#   - second column: the maximum production capacity
#   - third column: the slope of the marginal cost/demand function
#   - fourth column: the origin of the marginal cost/demand function
# - a prop_emi vector containing for each agent the emission factor

# Repartition of the agents in the villages
nb_agent <- c(4)

# Names of the agents
names_1 <- c('The Jensens','The Nielsens')
names_2 <- c('WindPower','FlexiFuel')

# Position of the agents
LONG_1 = c(8.319,
           8.312)
LAT_1 = c(56.427,
          56.458)

LONG_2 = c(8.126,
           8.220)
LAT_2 = c(56.487,
          56.493)

# Make data with several positions
data_1 <- data.frame(LONG=LONG_1, LAT=LAT_1, NAME = names_1)
data_2 <- data.frame(LONG=LONG_2, LAT=LAT_2, NAME = names_2)

# Definition of the output dataframe agent_characteristic
agent_characteristic <- rbind(data_1, data_2)
agent_characteristic$GROUP <- c(rep("Village",4))
agent_characteristic$TYPE <- c(1,1,3,2)

# Definition of the output dataframe prop_tot
pmin <- c(-7.1,-5.1, 7, 0)
pmax <- c(-7,-5,7,15)
a_coeff <- c(0.064,0.064, 0.040, 0.05)
b_coeff <- c(6.5,6.5,2.5,3)

prop_tot <- cbind(pmin,pmax,a_coeff,b_coeff)

# Load of the emission characteristic 
prop_emi <- c(0,0,0,0.5)
