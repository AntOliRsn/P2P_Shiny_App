# SETUP FILE: 1
# CHARACTERISTICS : 12 agents, 2 villages
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

nb_agent <- c(6,6)

spread_1 <- 0.035
spread_2 <- 0.035

LONG_ref_1 <- 10.40237
LAT_ref_1 <- 55.403756

LONG_ref_2 <- 10.094465
LAT_ref_2 <- 55.506962

set.seed(10)  # For exemple setup
names_1 <- c('DarkCoal','The Jensens','Windy','The Nielsens','The Hansens','ShinyPower')
names_2 <- c('The Pedersens','The Andersens','BlueWater','WindPower','The Christensens','FlexiFuel')

# Make data with several positions
data_1 <- data.frame(LONG=LONG_ref_1+spread_1*rnorm(nb_agent[1]), LAT=LAT_ref_1 +spread_1*rnorm(nb_agent[1]), NAME = names_1)
data_2 <- data.frame(LONG=LONG_ref_2 +spread_2*rnorm(nb_agent[2]), LAT=LAT_ref_2 + spread_2*rnorm(nb_agent[2]), NAME = names_2)

# For exemple setup
data_2[6,c(1,2)] <- c(10.03, 55.46)
data_1[2,1] <- c(10.41)
data_1[4,1] <- c(10.35)
data_1[5,c(1,2)] <- c(10.38, 55.42)

agent_characteristic <- rbind(data_1, data_2)
agent_characteristic$GROUP <- c(rep("Odense",6), rep("Countryside",6))
agent_characteristic$TYPE <- c(2,1,3,1,1,4,1,1,5,3,1,2)

# Load of the properties of each agent: marginal costs
prop_tot <- readMat('data/Prop_total_12_simple.mat')
prop_tot <- prop_tot$prop.tot
prop_tot <- prop_tot[,,1]

# For exemple setup
prop_tot[c(1,3,6,9,10,12),3] <- c(0.084, 0.045, 0.043, 0.06, 0.042, 0.088)
prop_tot[c(1,3,6,9,10,12),4] <- c(4, 2, 2.2, 3, 1.9, 3.9)
prop_tot[c(1,3,6,9,10,12),2] <- c(100, 12, 15, 21, 16, 100)
prop_tot[c(4,5,8,11),1] <- c(-14,-15,-19,-18)

# Load of the emission characteristic 
prop_emi <- c(0.5,0,0,0,0,0,0,0,0,0,0,0.6)
