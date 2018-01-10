# First helper file
# Contain most of the function called by the app 

# Get the color of an agent marker depending on its type 
getColor <- function(agent_characteristic) {
  sapply(agent_characteristic$TYPE, function(TYPE) {
    if(TYPE == 1) {
      "white"
    } else if(TYPE <= 2) {
      "red"
    } else if(TYPE <= 3) {
      "green"
    } else if(TYPE <= 4) {
      "orange"
    } else {
      "blue"
    } })
}

# Get the Icon of an agent marker depending on its type 
getIcon <- function(agent_characteristic) {
  sapply(agent_characteristic$TYPE, function(TYPE) {
    if(TYPE == 1) {
      "home"
    } else if(TYPE <= 2) {
      "stats-bars"
    } else if(TYPE <= 3) {
      "ios-medical"
    } else if(TYPE <= 4) {
      "ios-sunny"
    } else {
      "waterdrop"
    } })
}

# Read a standard "setup file" and creates all the parameters needed for the RCI, LP, map, and app setups
setup_to_shiny <- function(setup){
  # Loading standardized setup file
  # It has to be in the /starter folder
  source(paste("starter/", "starter_", setup,".R", sep = ""), local = TRUE)
  
  ######## DEFINITION OF THE GRAPHIC PARAMETERS ##########
  nb_village <- length(unique(agent_characteristic$GROUP))
  nb_agent <- count(agent_characteristic, "GROUP")$freq
  village_indice <- c(0,cumsum(nb_agent))+1
  
  # Reconstruction of the distance gamma matrix
  gamma <-matrix(1,sum(nb_agent),sum(nb_agent))
  for (i in 1:nb_village){
    data_village <- agent_characteristic[village_indice[i]:(village_indice[i+1]-1),]
    gamma_village <- matrix(0, nb_agent[i],nb_agent[i])
    for (j in 1:nb_agent[i]){
      loc_agent <- data_village[j,c("LONG","LAT")]
      loc_other <- data_village[-c(j),c("LONG","LAT")]
      dist_agent <- distm(loc_agent,loc_other,fun = distHaversine)
      
      gamma_village[j,-c(j)] <- dist_agent
    }
    gamma[village_indice[i]:(village_indice[i+1]-1),village_indice[i]:(village_indice[i+1]-1)] <- gamma_village
  }
  
  # Reconstruction of the emmission gamma matrix
  gamma_emi <- matrix(rep(prop_emi, sum(nb_agent)),sum(nb_agent),sum(nb_agent),byrow = TRUE)

  # For each village, find the two farest agents 
  village_position_ref <- matrix(NA,nb_village,3)
  village_farest_agent <- matrix(data=NA,nb_village,2)
  
  for (i in 1:nb_village){
    gamma_village <- gamma[village_indice[i]:(village_indice[i+1]-1),village_indice[i]:(village_indice[i+1]-1)]
    village_position_ref[i,3] <- max(gamma_village)
    village_farest_agent[i,] <-c(village_indice[i]-1,village_indice[i]-1) + which(gamma_village == max(gamma_village),arr.ind = TRUE)[1,]
  }
  
  # Find the center of the villages 
  for (i in 1:nb_village){
    agent_1 <- agent_characteristic[village_farest_agent[i,1],c('LONG','LAT')]
    agent_2 <- agent_characteristic[village_farest_agent[i,2],c('LONG','LAT')]
    
    village_position_ref[i,1:2] <- c((agent_1$LONG+agent_2$LONG)/2,(agent_1$LAT+agent_2$LAT)/2)
  }
  
  village_position_ref <- data.frame(village_position_ref)
  names(village_position_ref) <- c("LONG", "LAT", "DIST")
  village_position_ref$NAME <- unique(agent_characteristic$GROUP)
  
  # Standardization of the distance gamma matrix
  if(nb_village == 1){
    village_interdistance <- village_position_ref$DIST
  } else if (nb_village == 2){
    village_interdistance <- distm(village_position_ref[1,c("LONG","LAT")],village_position_ref[2,c("LONG","LAT")],fun = distHaversine)
  } else {
    print("Issue, more than two villages")
  }
  gamma[gamma !=1] <- gamma[gamma !=1]/c(village_interdistance)
  
  agent_characteristic$AGENT_ID <- 1:sum(nb_agent)
  
  map_setup <- list('agent_characteristic' = agent_characteristic,'village_position_ref' = village_position_ref )
  
  ######## DEFINITION OF THE LP PARAMETERS ##########
  # Not needed if the lp solver is not used
  Display_LP <- 1
  n_agents <- sum(nb_agent)
  #c_n_value <- 1
  #indice_agent <- 1:12
  prop <- prop_tot
  Pmin <- cbind(c(prop[,1]))
  Pmax <- cbind(c(prop[,2]))
  a_n <- prop[,3]
  b_n <- prop[,4]
  producers <- which(Pmax>0) 
  consumers <- which(Pmin<0)
  # c_n <- matrix(0, n_agents, 2) 
  # c_n[producers,1] <- c_n_value
  # c_n[consumers,2] <- -c_n_value
  omega_n <- matrix(list(),n_agents,2)
  s_value <- matrix(1,n_agents,1)
  # s_value(producers) <- 1
  s_value[consumers] <- 2
  gamma_mem <- gamma                                                      
  gamma <- array(dim = c(n_agents,n_agents,2))
  gamma[,,1] <- gamma_mem
  gamma[,,2] <- gamma_mem
  
  gamma_mem <- gamma_emi                                                      
  gamma_emi <- array(dim = c(n_agents,n_agents,2))
  gamma_emi[,,1] <- gamma_mem
  gamma_emi[,,2] <- gamma_mem
  
  
  for (n in 1:length(producers)){
    omega_n[[producers[n],1]] <- consumers
  }
  for (n in 1:length(consumers)){
    omega_n[[consumers[n],2]] <- producers
  }
  
  lp_setup <- list('gamma'= gamma,
                   'gamma_emi' = gamma_emi,
                   'prop' = prop,
                   'n_agents' = n_agents,
                   #'indice_agent' = indice_agent,
                   'Pmin' = Pmin,
                   'Pmax' = Pmax,
                   'a_n' = a_n,
                   'b_n' = b_n,
                   'producers'= producers,
                   'consumers' = consumers,
                   'omega_n' = omega_n,
                   's_value' = s_value
  )
  
  ######## DEFINITION OF THE RCI PARAMETERS ##########
  
  ## RCI Parameters
  max_it <- 30000
  eps_P <- 0.01
  eps_L <- 0.001
  eps_M <- 0.0001
  eps_cons <- 1
  
  n_horizon <- 1
  fac <-1
  kappa <- 1
  iota <-1
  deviation <- 0
  
  alpha_base <- kappa*0.01/((1:max_it)^(iota*0.01)) # The first two are for a decaying alpha and beta
  beta_base <- kappa*0.1/((1:max_it)^(iota*0.1)) #(1-deviation+2*deviation*rand(1,n_agents+1));
  alpha <- alpha_base[1]*matrix(1,n_agents,n_agents)
  beta <- beta_base[1]*matrix(1,n_agents,n_agents)
  delta <- matrix(1,max_it,1)
  eta <- 0.005*matrix(1,max_it,1)
  
  # Power_loc <- array(0,dim = c(n_agents+1,n_agents+1,n_horizon))            # CHANGE HERE
  Power_loc <- matrix(0,n_agents,n_agents)  
  Power_split_loc <- array(0, dim = c(n_agents,n_agents,2,n_horizon,2))
  Price_loc <- array(0, dim = c(n_agents,n_agents,n_horizon,2))
  Price_perceived_loc <- array(0, dim = c(n_agents,n_agents,2,n_horizon))
  Power_total_loc <- array(0,dim = c(n_agents,n_horizon))
  Local_consumption_target <- array(0,dim = c(n_agents,n_agents,2,n_horizon))
  factor <- array(0, dim = c(n_agents,n_agents,2,n_horizon))
  Pi <- array(0, dim = c(n_agents,n_agents,2,n_horizon))
  Pi_2 <- array(0, dim = c(n_agents,n_agents,2,n_horizon))
  Pi_2_star <- array(0, dim = c(n_agents,n_agents,2,n_horizon))
  Old_consumption <- matrix(0,n_agents,n_horizon)
  Mu_n_loc <- array(0, dim = c(n_agents,2,n_horizon,2))
  
  # Price <- array(0, dim =c(n_agents,n_agents,n_horizon,max_it))                     # NEEDED IF WANTED TO SAVE ALL THE ITERATIONS 
  # Price_perceived <- array(0, dim = c(n_agents,n_agents,2,n_horizon,max_it))
  # Power_split <- array(0, dim = c(n_agents,n_agents,2,n_horizon,max_it))
  # Power <- array(0, dim = c(n_agents,n_agents,n_horizon,max_it))
  # Power_total <- array(0, dim = c(n_agents,n_horizon,max_it))
  # Pi_mem <- array(0, dim = c(n_agents,n_agents,2,n_horizon,max_it))
  # Mu_n <- array(0, dim = c(n_agents,2,n_horizon,max_it))
  # 
  Is_a_variable <- array(0, dim = c(n_agents,n_agents,2,n_horizon))
  for (t_loc in 1:n_horizon){
    for (n in 1:n_agents){
      for (m in omega_n[[n,s_value[n]]]){
        Is_a_variable[[n,m,s_value[n],t_loc]] <- 1
      }
    }
  }
  
  Power_scale <- matrix(1,n_agents+1,1)
  n_contact <- matrix(0,n_agents,n_agents)
  for (n in 1:n_agents){
    n_contact[n,] <- length(omega_n[[n,1]])+length(omega_n[[n,2]])
  }
  #n_contact[n_agents,] <- 1
  
  rci_setup <- list('gamma'= gamma,
                    'gamma_emi' = gamma_emi,
                   'prop' = prop,
                   'n_agents' = n_agents,
                   #'indice_agent' = indice_agent,
                   'Pmin' = Pmin,
                   'Pmax' = Pmax,
                   'a_n' = a_n,
                   'b_n' = b_n,
                   'producers'= producers,
                   'consumers' = consumers,
                   'omega_n' = omega_n,
                   's_value' = s_value,
                   'max_it' = max_it,
                   'eps_L' = eps_L,
                   'eps_P' = eps_P,
                   'eps_M' = eps_M,
                   'eps_cons' = eps_cons,
                   'n_horizon' = n_horizon,
                   'fac' = fac,
                   'kappa' = kappa,
                   'iota' = iota,
                   'deviation' = deviation,
                   'alpha_base' = alpha_base,
                   'beta_base' = beta_base,
                   'alpha' = alpha,
                   'beta' = beta,
                   'delta' = delta,
                   'eta' = eta,
                   'Power_loc' = Power_loc,
                   'Power_split_loc' = Power_split_loc,
                   'Price_loc' = Price_loc,
                   'Price_perceived_loc' = Price_perceived_loc,
                   'Power_total_loc' = Power_total_loc,
                   'Local_consumption_target' = Local_consumption_target,
                   'factor' = factor,
                   'Pi' = Pi,
                   'Pi_2' = Pi_2,
                   'Pi_2_star' = Pi_2_star,
                   'Old_consumption' = Old_consumption,
                   'Mu_n_loc' = Mu_n_loc,
                   # 'Price' = Price,
                   # 'Price_perceived' = Price_perceived,
                   # 'Power_split' = Power_split,
                   # 'Power' = Power,
                   # 'Power_total' = Power_total,
                   # 'Pi_mem' = Pi_mem,
                   # 'Mu_n' = Mu_n,
                   'Is_a_variable' = Is_a_variable,
                   'Power_scale' = Power_scale,
                   'n_contact' = n_contact
                   )
  
  ######## DEFINITION OF THE PARAMETERS NEEDED FOR THE APP LAYOUT ##########
  village_names <- unique(agent_characteristic$GROUP)
  village_ids <- 1:length(village_names)
  
  agent_characteristic <- agent_characteristic[,c("NAME","GROUP" ,"TYPE")]
  agent_characteristic$BEHAVIOR <- rep("prod", dim(agent_characteristic)[1])
  agent_characteristic$BEHAVIOR[consumers] <- "cons"
  
  agent_characteristic$PROD_TYPE <- "sust"
  agent_characteristic$PROD_TYPE[agent_characteristic$BEHAVIOR == "prod" & agent_characteristic$TYPE == 2] <- "conv"
  agent_characteristic$PROD_TYPE[agent_characteristic$BEHAVIOR == "cons"] <- NA
  
  agent_characteristic$PREFERENCES <- rep(0, dim(agent_characteristic)[1])   #NOT USED CURRENTLY
  
  GROUP_ID <- 1:length(agent_characteristic$GROUP)
  agent_characteristic$GROUP_ID <- unlist(lapply(agent_characteristic$GROUP, 
                                          function(GROUP){
                                            GROUP_ID[which(village_names==GROUP)]
                                          }))
  
  agent_characteristic$AGENT_ID <- 1:sum(nb_agent)
  
  app_setup <- list('village_names' = village_names, 
                    'village_ids' = village_ids,
                    'agent_characteristic' = agent_characteristic)
  
  return(list('map_setup' = map_setup, 
              'lp_setup' = lp_setup,
              'rci_setup' = rci_setup,
              'app_setup' = app_setup))
}

# Calculate some indvidual indicators for each agent
individualResultAnalysis <- function(power_out, price_out, agent_characteristic){
  # TO DO: PUT THE AGENT LOCAL MASK DEFINITION AT THE END OF THE SOLVER 
  # IN ORDER TO CHARGE IT ONLY ONCE !
  village_agents_id <- split(agent_characteristic$AGENT_ID, f = agent_characteristic$GROUP_ID)
  
  village_local_mask <- diag(0,dim(power_out)[1], dim(power_out)[2])
  
  for (agents_id in  village_agents_id){
    local_exchange <- t(combn(agents_id, 2))
    village_local_mask[local_exchange] <-1
  }
  village_local_mask <- village_local_mask + t(village_local_mask)
  
  conv_mask <- matrix(0,dim(power_out)[1], dim(power_out)[2])
  conv_mask[,agent_characteristic$PROD_TYPE == "conv"] <-1
  
  total_individual_power <- apply(power_out,1,sum)
  total_individual_local_power <- apply(power_out*village_local_mask,1,sum)
  total_conv_cons_power <- apply(power_out*conv_mask,1,sum)
  
  total_individual_money <- apply(power_out*price_out,1,sum)
  individual_marginal_money <- total_individual_money/total_individual_power
  
  individual_results <- data.frame('agent_id' = agent_characteristic$AGENT_ID,
                                   'behavior' = agent_characteristic$BEHAVIOR,
                                   'total_individual_power' = total_individual_power,
                                   'total_conv_cons_power' = total_conv_cons_power,
                                   'total_individual_local_power' = total_individual_local_power,
                                   'total_individual_money' = total_individual_money,
                                   'individual_marginal_money' = individual_marginal_money)
  
  return(individual_results)
}

# Generate the Leaflet Map for a given setup
mapGeneration <- function(map_setup){
  # Input description:
  # setupParameters: data frame with four arguments: LONG, LAT, COLOR, NAME, GROUP. One line by agent
  
  data <- map_setup$agent_characteristic
  village_position_ref <- map_setup$village_position_ref
  
  # setup characteristoics
  nameGroup <- unique(data$GROUP)
  colorVillage <- unique(data$COLOR)
  nbGroup <- length(nameGroup)
  
  # Icon 
  icons <- awesomeIcons(
    icon = getIcon(data),
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(data)
  )
  
  # minLong <- aggregate(data$LONG, by=list(Category=data$GROUP), FUN=min)[,2]
  # minLat <- data.frame(aggregate(data$LAT, by=list(Category=data$GROUP), FUN=min))[,2]
  # maxLong <- data.frame(aggregate(data$LONG, by=list(Category=data$GROUP), FUN=max))[,2]
  # maxLat <- data.frame(aggregate(data$LAT, by=list(Category=data$GROUP), FUN=max))[,2]
  
  # limitation <- data.frame("minLONG" = minLong, "minLAT" = minLat, "maxLONG" = maxLong, "maxLAT" = maxLat)
  #limitations_1 <- limitations_1 +c(-shift, -shift, shift, shift)
 
  # leaflet map definition
  map <- leaflet() %>% 
    # Map tiles addition
    setView(lng=village_position_ref$LONG[1], lat=village_position_ref$LAT[1], zoom=10) %>%
    addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
    addTiles(options = providerTileOptions(noWrap = TRUE), group="Map") %>%
    addCircles(data= village_position_ref, lng = ~LONG, lat = ~LAT, weight = 1,
               radius = ~(DIST/2), fillColor="blue", popup = ~NAME, fillOpacity = 0.1,
               highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = FALSE))
    
    # Village limitation addition 
    # for (i in 1:nbGroup){
    #   map <- addRectangles(map, lng1=limitation$minLONG[i] ,lat1=limitation$minLAT[i], lng2=limitation$maxLONG[i], lat2=limitation$maxLAT[i], 
    #                        fillColor = colorVillage[i] , fillOpacity = 0.2, group = nameGroup[i],
    #                        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = FALSE),
    #                        popup = nameGroup[i], labelOptions = labelOptions(noHide = T))
    # }
    # Markers addition
    # map <- addCircleMarkers(map, data=data, lng=~LONG , lat=~LAT, radius=8 , color="black",  
    #                  fillColor=~COLOR, stroke = TRUE, fillOpacity = 0.8, group=~GROUP,
    #                  popup = ~htmlEscape(NAME)) %>%
      
    map <- addAwesomeMarkers(map, data=data, lng=~LONG , lat=~LAT,  layerId =~AGENT_ID,
                             icon=icons, popup = ~htmlEscape(NAME))%>%
    
    # Control addition
    #addLayersControl(overlayGroups = nameGroup , baseGroups = c("background 1","background 2"), options = layersControlOptions(collapsed = FALSE))
    addLayersControl(baseGroups = c("Satellite","Map"), options = layersControlOptions(collapsed = FALSE))
    
  # The function returns a leaflet map object
  return(map)
}

# Update the Leaflet map after solving an optimization
mapUpdate <- function(map, map_setup, power_out, price_out){
  
  data <- map_setup$agent_characteristic
  village_position_ref <- map_setup$village_position_ref
  
  power_trade <- which(power_out > 0,arr.ind = TRUE)
  power_trade <- cbind(power_trade, power_out[which(power_out>0)])
  power_trade <- cbind(power_trade, price_out[which(power_out>0)])
  
  line_weight <- 5+power_trade[,3]/max(power_trade[,3])*5
  
  colors <- rev(colorRampPalette(brewer.pal(11,"RdYlGn"))(21))
  line_color <- colors[1+floor(line_weight*20/10)]
  
  labels <- 1:dim(power_trade)[1]
  
  for (i in 1:dim(power_trade)[1]){
    
    # /10 for exemple setup
    label <- paste('<p>',"Power: ", format(round(power_trade[i,3], 2), nsmall = 2), 'kWh','<p></p>',
                   "Price: ", format(round(power_trade[i,4], 2), nsmall = 2), 'DKK/kWh','</p>')
    
    labels[i] <- label
    
    map <- addPolylines(map, lng=~LONG,lat=~LAT,data=data[power_trade[i,1:2],],
                        color= line_color[i], weight = line_weight[i], opacity = 0.6,
                        label = HTML(label), layerId = as.character(i))
  }
  
  trade_lines_characteristics <- list("power_trade" = power_trade,
                                     "line_weight" = line_weight,
                                     "line_color" = line_color,
                                     "labels" = labels
  )
  
  return(list("map" = map, "trade_lines_characteristics" = trade_lines_characteristics))
}

# LP solver
# DO NOT WORK with shyniapp.io: it used the Rmosek package which need a C compiler
LPsolve <- function(preferences, lp_setup){
  
  # Put all the variables of the list in local
  for(i in 1:length(lp_setup)) assign(names(lp_setup)[i], lp_setup[[i]])
  
  # Construction of the reactive dependant parameters
  c_n <- matrix(0, n_agents, 2) 
  c_n[producers,1] <- preferences[producers]
  c_n[consumers,2] <- -preferences[consumers]
  
  # source(file.path(path, "starter", paste("starter_", setup,".R", sep = "")), local = TRUE)
  ## Definition of the problem
  # Construction of the variable vector
  n_variables <- (n_agents)*(n_agents)
  Variable_to_agent <- matrix(0,n_variables,3)
  Agent_to_variable <- matrix(0, n_agents, n_agents)
  count <- 1
  for (i in 1:(n_agents)){
    for (j in 1:(n_agents)){
      Variable_to_agent[count,] <- c(i,j,s_value[i])
      Agent_to_variable[i,j] <- count
      count <- count+1
    }
  }
  
  # Inequation Pnm+>0 and Pnm-<0
  A_sign <- matrix(0,n_variables,n_variables)
  n_prod <- length(producers)
  n_cons <- length(consumers)
  vect_prod <- c(t(Agent_to_variable[producers,]))
  vect_cons <- c(t(Agent_to_variable[consumers,]))
  A_sign[vect_prod,vect_prod] <- -diag(rep(1,length(vect_prod)))
  A_sign[vect_cons,vect_cons] <- diag(rep(1,length(vect_cons)))
  b_sign <- rep(0,n_variables)       
  
  # Inequation Pn_min<Pn<Pn_max
  A_boundary <- matrix(0, n_agents*2,n_variables)
  b_boundary <- matrix(0, n_agents*2, 1);
  count_ineq <- 1
  for (i in 1:n_agents){
    A_boundary[count_ineq,Agent_to_variable[i,]] <- rep(1,n_agents)
    A_boundary[count_ineq+n_agents,Agent_to_variable[i,]] <- -rep(1,n_agents)
    b_boundary[count_ineq] <- Pmax[i]
    b_boundary[count_ineq+n_agents] <- -Pmin[i]
    count_ineq <- count_ineq+1
  }
  
  # Set Pnm=0 when needed and prevent spillage
  A_zero <- diag(rep(1,n_variables))
  b_zero <- rep(0, n_variables)
  for (i in 1:n_agents){
    for (s in 1:2){
      for (m in omega_n[[i,s]]){
        A_zero[Agent_to_variable[i,m],Agent_to_variable[i,m]] <- 0
      }
    }
  }
  
  # Bilateral balance equations Pnm+Pmn=0
  n_balance_eq_hour <- n_agents*(n_agents-1)/2
  n_balance_eq <- n_balance_eq_hour
  A_eq_balance <- matrix(0,n_balance_eq,n_variables)
  b_eq_balance <- rep(0,n_balance_eq)
  Balance_eq_to_agent <- matrix(0,n_balance_eq,2)
  
  count <- 1
  i <- 1
  while (i < n_agents){
    for (j in (i+1):(n_agents)){
      A_eq_balance[count,Agent_to_variable[i,j]] <- 1
      A_eq_balance[count,Agent_to_variable[j,i]] <- 1
      Balance_eq_to_agent[count,] <- c(i,j)
      count <- count+1
    }
    i <- i + 1
  }
  # count-1-n_agents_P2P*(n_agents_P2P+1)/2;
  
  # Objective function
  H <- matrix(0,n_variables,n_variables)
  # f=zeros(n_variables,1)
  C_1 <- rep(0,n_variables)
  C_2 <- rep(0,n_variables)
  C_3 <- rep(0,n_variables)
  
  for (i in 1:n_agents){
    for (j in 1:(n_agents)){
      for (l in 1:(n_agents)){
        H[Agent_to_variable[i,j],Agent_to_variable[i,l]] <- H[Agent_to_variable[i,j],Agent_to_variable[i,l]]+a_n[i]
      }
    }
  }
  
  for (i in 1:n_variables){
    if (Variable_to_agent[i,1] < n_agents + 1){            # Not necessary
      C_1[i] <- b_n[Variable_to_agent[i,1]]
    }
  }
  
  for (i in 1:n_variables){
    C_2[i] <- gamma[Variable_to_agent[i,1],Variable_to_agent[i,2],Variable_to_agent[i,3]]*c_n[Variable_to_agent[i,1],Variable_to_agent[i,3]]
  }
  
  # for t_sh=1:n_hour_shifting
  #    for i=1:n_agents_P2P
  #         C_3(Agent_to_variable(n_agents+1,i,t_sh,1))=Market_price(t_sh);
  #         C_3(Agent_to_variable(n_agents+1,i,t_sh,2))=Market_price(t_sh);
  #     end
  # end
  
  f <- C_1+C_2+C_3
  A_ineq <- rbind(A_boundary,A_sign)
  b_ineq <- c(b_boundary,b_sign)
  A_eq <- rbind(A_eq_balance,A_zero)
  b_eq <- c(b_eq_balance,b_zero)
  lb <- rep(-Inf, n_variables)
  ub <- rep(Inf, n_variables)
  
  Hpd <- nearPD(H)
  Hpd <- as.matrix(Hpd$mat)
  
  F <- Matrix::chol(Hpd)
  
  prob <- mosek_qptoprob(F, f, A_ineq, b_ineq, A_eq, b_eq, lb, ub)
  #prob$cones[1,1]<- "QUAD"
  r <- mosek(prob, list(soldetail = 1, usesol = TRUE, verbose = 0))
  
  # Result Analysis
  ineq_nb <- dim(A_ineq)[1]
  x <- r$sol$itr$xx[1:n_variables]
  lambda <- r$sol$itr$slc[(ineq_nb+1):(ineq_nb + dim(A_eq)[1])]
  
  # Power
  Power_out <- matrix(0,n_agents,n_agents)
  for (i in 1:n_variables){
    Power_out[Variable_to_agent[i,1],Variable_to_agent[i,2]] <- x[i]
    if (abs(x[i])<0.001){
      Power_out[Variable_to_agent[i,1],Variable_to_agent[i,2]] <- 0
    }
  }
  
  # Price
  Price_out <- matrix(0,n_agents,n_agents)
  Price_perceived_out <- matrix(0, n_agents,n_agents,2)
  for (i in 1:n_balance_eq){
    if (s_value[Balance_eq_to_agent[i,1]]*s_value[Balance_eq_to_agent[i,2]]==2){
      Price_out[Balance_eq_to_agent[i,1],Balance_eq_to_agent[i,2]] <- lambda[i]
    }
  }
  Price_out <- Price_out + t(Price_out)
  
  # Function's return
  return(list("Power_out" = Power_out, "Price_out" = Price_out))
}

# RCI solver
RCIsolve <- function(preferences, rci_setup){
  
  for(i in 1:length(rci_setup)) assign(names(rci_setup)[i], rci_setup[[i]])
  # Construction of the reactive dependant parameters
  c_n <- matrix(0, n_agents, 2) 
  c_n_emi <- matrix(0, n_agents, 2) 
  
  c_n[producers,1] <- preferences[producers,1]
  c_n[consumers,2] <- -preferences[consumers,1]
  
  
  c_n_emi[producers,1] <- preferences[producers,2]
  c_n_emi[consumers,2] <- -preferences[consumers,2]
  
  sign <- c(1,-1)
  n_start <- 5000
  
  ## Optimisation lopp
  
  for (k in 1:max_it){
    
    ## Price update
    #### ISSUE: FIND A SOLUTION FOR PERMUTE
    alpha <- alpha_base[k]*matrix(1, n_agents, n_agents)
    beta <- beta_base[k]*matrix(1, n_agents, n_agents)
    Price_loc[,,,2] <- Price_loc[,,,1]-beta*(Price_loc[,,,1]-t(Price_loc[,,,1]))-alpha*(Power_loc+t(Power_loc))  ## CHANGE HERE
    
    ## Perceived Price
    for (s in 1:2){
      Price_perceived_loc[,,s,] <- Price_loc[,,,2] - (c_n[,s]%*%matrix(1,1,n_agents))*gamma[,,s] - (c_n_emi[,s]%*%matrix(1,1,n_agents))*gamma_emi[,,s]
    }
    Price_perceived_loc <- Price_perceived_loc*Is_a_variable
    
    ## pdate the estimate of inequality dual variable 1=Pmax  2=Pmin
    
    for (n in 1:n_agents){
      for (t_loc in 1:n_horizon){
        if (Pmax[n, t_loc] == Pmin[n, t_loc]){ # To deal with must take renewables 
          temp <- Mu_n_loc[n,1,t_loc,1] - Mu_n_loc[n,2,t_loc,1] + fac*eta[k]*(Power_total_loc[n,t_loc]- Pmax[n,t_loc])/Power_scale[n,t_loc]
          Mu_n_loc[n,1,t_loc,2] <- max(c(0,temp));
          Mu_n_loc[n,2,t_loc,2] <- max(c(0,-temp));
          # ERROR
        } else {
          if ((Power_total_loc[n,t_loc] - Pmax[n,t_loc]) > 0){
            Mu_n_loc[n,1,t_loc,2] <- Mu_n_loc[n,1,t_loc,1] + fac*eta[k]*(Power_total_loc[n,t_loc] - Pmax[n,t_loc])/Power_scale[n,t_loc]
          } else if ((Power_total_loc[n,t_loc] - Pmax[n,t_loc]) == 0) {
            Mu_n_loc[n,1,t_loc,2] <- Mu_n_loc[n,1,t_loc,1]
          } else {
            Mu_n_loc[n,1,t_loc,2] <- max(0,Mu_n_loc[n,1,t_loc,1] - fac*eta[k]*(Pmax[n,t_loc]-Power_total_loc[n,t_loc])/Power_scale[n,t_loc])
          }
          if ((Pmin[n,t_loc]-Power_total_loc[n,t_loc])>0){
            Mu_n_loc[n,2,t_loc,2] <- Mu_n_loc[n,2,t_loc,1]+fac*eta[k]*(Pmin[n,t_loc]-Power_total_loc[n,t_loc])/Power_scale[n,t_loc]
          } else if ((Pmin[n,t_loc]-Power_total_loc[n,t_loc]) == 0){
            Mu_n_loc[n,2,t_loc,2] <- Mu_n_loc[n,2,t_loc,1]
          } else {
            Mu_n_loc[n,2,t_loc,2] <- max(0,Mu_n_loc[n,2,t_loc,1] - fac*eta[k]*(Power_total_loc[n,t_loc] - Pmin[n,t_loc])/Power_scale[n,t_loc])
          }
        }
      }
    }
    
    ## Local consumption target per trade
    param <- 0.8
    for (s in 1:2){
      Local_consumption_target[1:n_agents,,s,] <- Is_a_variable[1:n_agents,,s,]*(Price_perceived_loc[1:n_agents,,s,] - Mu_n_loc[1:n_agents,1,,2]%*%matrix(1,1,n_agents) + Mu_n_loc[1:n_agents,2,,2]%*%matrix(1,1,n_agents) - b_n[1:n_agents]%*%matrix(1,1,n_agents))/(a_n[1:n_agents]%*%matrix(1,1,n_agents))
    }
    ## Proportionnal factor
    temp <- rowSums(abs(Power_loc[,]))%*%matrix(1,1,n_agents)
    for (s in 1:2){
      factor[,,s,] <- Is_a_variable[,,s,]*(abs(Power_split_loc[,,s,,1])+delta[k])/(temp+delta[k]*n_contact)
    }
    ## PI-update
    for (s in 1:2){
      Pi[,,s,] <- Is_a_variable[,,s,]*(Power_split_loc[,,s,,1]+factor[,,s,]*(Local_consumption_target[,,s,]-Power_total_loc[,]%*%matrix(1,1,n_agents)))
    }
    
    ## Enforce Pnm > 0
    Pi_star <- Pi
    Pi_star[,,1,t_loc] <- pmax(Pi[,,1,1],0)
    Pi_star[,,2,t_loc] <- -pmax(-Pi[,,2,1],0)
    
    ## Power update
    Power_split_loc[,,,1,2] <- Pi_star
    
    ## Stopping criteria
    epsilon_L <- sum(abs(Price_loc[,,,2] - Price_loc[,,,1]))
    epsilon_P <- sum(abs(Power_split_loc[,,,,2] - Power_split_loc[,,,,1]))
    
    ## Data saving
    # Power_split[,,,,k]<-Power_split_loc[,,,,2]                       #NEEDED IF YOU WANT TO SAVE THE INTERMEDIATE RESULTS
    # Power[,,,k]<-apply(Power_split_loc[,,,,2], MARGIN=c(1, 2), sum)
    # Price[,,,k]<-Price_loc[,,,2]
    # Price_perceived[,,,,k]<-Price_perceived_loc
    # Power_total[,,k]<-Power_total_loc
    # Pi_mem[,,,,k]<-Pi
    # Mu_n[,,,k]<-Mu_n_loc[,,,2]
    
    ## Data Update
    Price_old<-Price_loc[,,,1]    
    Power_old<-Power_loc          # CHANGE HERE
    Mu_n_old<-Mu_n_loc[,,,1]
    Price_loc[,,,1]<-Price_loc[,,,2]
    Power_split_loc[,,,,1]<-Power_split_loc[,,,,2]
    Power_loc<-apply(Power_split_loc[,,,,1], MARGIN=c(1, 2), sum)
    Power_total_loc<- cbind(c(rowSums(Power_loc))) ### Line to be change if more time step or prosumers
    Mu_n_loc[,,,1]<-Mu_n_loc[,,,2]
    
    epsilon_cons <- sum(abs(Power_loc+t(Power_loc)))
    Power_trans_loc <- (Power_split_loc[,,,,1]-aperm(Power_split_loc[,,,,1],c(2, 1, 3)))/2   ## CHANGE HERE
    ##### TAKE A LOOK AT PERMUTE
    
    ## Stopping loop
    boolean <- 1
    if (k>1){
      if (max(abs(Power_loc - Power_old)) < eps_P &          ## CHANGE HERE
          max(abs(Power_loc+t(Power_loc)))<eps_cons &        ## CHANGE HERE
          max(abs(Mu_n_old-Mu_n_loc[,,,2]))<eps_M) {
        boolean <- boolean*1
      } else {
        boolean <- 0
      }
      if (max(abs(Price_loc[,,,1] - Price_old)) < eps_L) {
        boolean <- boolean*1
      } else {
        boolean <- 0
      }
      if (boolean == 1){
        break
      }
    }
  }
  # To avoid differences due to the RCI algorithm
  Power_loc[upper.tri(Power_loc)] <- 0
  Power_loc <- Power_loc - t(Power_loc)
  
  return(list("Power_out" = Power_loc, "Price_out" = Price_loc[,,,1]))
}

