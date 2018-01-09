# set up 1
# 12 agents, 2 villages
# Temporary Parameters


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


#paste("House_",seq(1,nb_agent[1]))

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
agent_characteristic$COLOR <- c(rep("blue",6), rep("red",6))
agent_characteristic$TYPE <- c(2,1,3,1,1,4,1,1,5,3,1,2)

#village_indice <- c(0,cumsum(nb_agent))+1

# Load of gamma matrix
# gamma <- readMat('/Users/antoine/Documents/R/Shiny Apps/LP solver/gamma_12.mat')
# gamma <- gamma$gamma


# Reconstruction of the distance gamma matrix
# gamma <-matrix(1,sum(nb_agent),sum(nb_agent))
# for (i in 1:nb_village){
#   data_village <- agent_characteristic[village_indice[i]:(village_indice[i+1]-1),]
#   gamma_village <- matrix(0, nb_agent[i],nb_agent[i])
#   for (j in 1:nb_agent[i]){
#     loc_agent <- data_village[j,c("LONG","LAT")]
#     loc_other <- data_village[-c(j),c("LONG","LAT")]
#     dist_agent <- distm(loc_agent,loc_other,fun = distHaversine)
#     
#     gamma_village[j,-c(j)] <- dist_agent
#   }
#   gamma[village_indice[i]:(village_indice[i+1]-1),village_indice[i]:(village_indice[i+1]-1)] <- gamma_village
# }

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


# For each village, find the two farest agents 
# village_position_ref <- matrix(NA,nb_village,3)
# village_farest_agent <- matrix(data=NA,nb_village,2)
# 
# for (i in 1:nb_village){
#   gamma_village <- gamma[village_indice[i]:(village_indice[i+1]-1),village_indice[i]:(village_indice[i+1]-1)]
#   village_position_ref[i,3] <- max(gamma_village)
#   village_farest_agent[i,] <-c(village_indice[i]-1,village_indice[i]-1) + which(gamma_village == max(gamma_village),arr.ind = TRUE)[1,]
# }
# 
# # Find the center of the village 
# for (i in 1:nb_village){
#  agent_1 <- agent_characteristic[village_farest_agent[i,1],c('LONG','LAT')]
#  agent_2 <- agent_characteristic[village_farest_agent[i,2],c('LONG','LAT')]
#  
#  village_position_ref[i,1:2] <- c((agent_1$LONG+agent_2$LONG)/2,(agent_1$LAT+agent_2$LAT)/2)
# }
# 
# village_position_ref <- data.frame(village_position_ref)
# names(village_position_ref) <- c("LONG", "LAT", "DIST")
# village_position_ref$NAME <- c("Village 1", "Village 2")
# 
# # Standardization of the distance gamma matrix
# village_interdistance <- distm(village_position_ref[1,c("LONG","LAT")],village_position_ref[2,c("LONG","LAT")],fun = distHaversine)
# gamma[gamma !=1] <- gamma[gamma !=1]/c(village_interdistance)
# 
# 
# graphic_setup <- list('agent_characteristic' = agent_characteristic,'village_position_ref' = village_position_ref )

# Definition of all the parameters
# Display_LP <- 1
# n_agents <- 12
# #c_n_value <- 1
# indice_agent <- 1:12
# prop <- prop_tot[,,1]
# Pmin <- prop[indice_agent,1]
# Pmax <- prop[indice_agent,2]
# a_n <- prop[indice_agent,3]
# b_n <- prop[indice_agent,4]
# producers <- which(Pmax>0) 
# consumers <- which(Pmin<0)
# # c_n <- matrix(0, n_agents, 2) 
# # c_n[producers,1] <- c_n_value
# # c_n[consumers,2] <- -c_n_value
# omega_n <- matrix(list(),n_agents,2)
# s_value <- matrix(1,n_agents,1)
# # s_value(producers) <- 1
# s_value[consumers] <- 2
# gamma_mem <- gamma[indice_agent, indice_agent]                                                        
# gamma <- array(dim = c(n_agents,n_agents,2))
# gamma[,,1] <- gamma_mem
# gamma[,,2] <- gamma_mem
# 
# for (n in 1:length(producers)){
#   omega_n[[producers[n],1]] <- consumers
# }
# for (n in 1:length(consumers)){
#   omega_n[[consumers[n],2]] <- producers
# }