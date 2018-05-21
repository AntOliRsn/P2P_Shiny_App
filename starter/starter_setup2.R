# SETUP FILE: 2
# CHARACTERISTICS : 4 agents, 2 villages
# Temporary Parameters

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

nb_agent <- c(3,2)

# Position of the agents
LONG_1 = c(12.146044823417697,
           12.145941537503859,
           12.14646861244205)
LAT_1 = c(55.660166209799655,
          55.65958490806486,
          55.65978492318476)

# LONG_2 = c(12.120112500000005,
#            12.120901796112094)
# LAT_2 = c(55.641509,
#           55.64171396107383)

# GPS in case we need to split incineration plant into 2, because we cannot have village with 1 single agent
LONG_2 = c(12.12011250000005,
           12.120895705032353)
LAT_2 = c(55.641509,
          55.64163161403202)

set.seed(10)  # For example setup
names_1 <- c('Pierre-Elouan','Katja','Jesper')
names_2 <- c('Incineration plant 1', 'Incineration plant 2')

# Make data with several positions
data_1 <- data.frame(LONG=LONG_1, LAT=LAT_1, NAME = names_1)
data_2 <- data.frame(LONG=LONG_2, LAT=LAT_2, NAME = names_2)

# For Svalin setup
agent_characteristic <- rbind(data_1, data_2)
agent_characteristic$GROUP <- c(rep("Svalin community",3), rep("Roskilde",2))
agent_characteristic$TYPE <- c(4,1,1,2,2) # PV, consumer, consumer, dirty plant

# Definition of the output dataframe prop_tot
pmin <- c(0,-7.01, -7.01, 0, 0)
pmax <- c(15,-7,-7,7.5, 7.5)
a_coeff <- c(0.067,0.215, 0.215, 0.067, 0.067)
b_coeff <- c(4.4, 6, 6, 3, 3)

prop_tot <- cbind(pmin,pmax,a_coeff,b_coeff)

# Load of the emission characteristic 
prop_emi <- c(0, 0, 0, 3, 3)
