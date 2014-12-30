library(dpcR)
source("flow_functions.R")
#attempt 2 - simulate whole flow and reorder droplets to get five cases
whole_flow <- as.vector(slot(sim_ddpcr(8000, 20000, 2000, n_exp = 1, dube = TRUE), ".Data"))

#case 1: complete randomness (RAND)
#we dont have to do sample again, but for sake of randomness
c1 <- sample(whole_flow)

#case 2: regular clusters (RC)
#NOT DONE!


#case 3: unregular clusters (URC)
c3 <- generate_flow(whole_flow, probs_URC)


#case 4: less probable positive droplet at the end of experiment - position correlation (PC)
c4 <- generate_flow(whole_flow, probs_PC)
