# SETUP 0 
# 2 agents, 2 plants 

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
agent_characteristic$COLOR <- c(rep("blue",2), rep("red",2))
agent_characteristic$TYPE <- c(1,1,3,2)

# Definition of the output dataframe prop_tot
pmin <- c(-7.1,-5.1, 7, 0)
pmax <- c(-7,-5,7,15)
a_coeff <- c(0.064,0.064, 0.040, 0.05)
b_coeff <- c(6.5,6.5,2.5,3)

prop_tot <- cbind(pmin,pmax,a_coeff,b_coeff)

# Load of the emission characteristic 
prop_emi <- c(0,0,0,0.5)
