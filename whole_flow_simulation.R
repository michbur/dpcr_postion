#attempt 2 - simulate whole flow and reorder droplets to get five cases
whole_flow <- as.vector(slot(sim_ddpcr(50, 200, 2000, n_exp = 1, dube = TRUE), ".Data"))

#case 1: complete randomness (RAND)
#we dont have to do sample again, but for sake of randomness
c1 <- sample(whole_flow)

#case 2: regular clusters (RC)
#NOT DONE!


#case 3: unregular clusters (URC)
binary_flow <- whole_flow != 0
pos_ids <- sample(which(binary_flow))
neg_ids <- sample(which(!binary_flow))

c3 <- c()
first_droplet_pos <- sample(c(TRUE, FALSE), 1)
if(first_droplet_pos) {
  c3[1] <- whole_flow[pos_ids[1]]
  pos_ids <- pos_ids[-1]
} else {
  c3[1] <- whole_flow[neg_ids[1]]
  neg_ids <- neg_ids[-1]
}

while(length(c3) < length(whole_flow)) {
  #by manipulating these probabilities clusters can be shorter or longer. The more p_bigger
  #is closer to 0.5, the shorter clusters
  p_bigger <- sample(75L:100, 1)/100
  probs_next <- if(c3[length(c3)] != 0) {
    c(p_bigger, 1 - p_bigger)
  } else {
    c(1 - p_bigger, p_bigger)
  }
  
  #this if takes care of situation in which we don't have more positive/negative droplets 
  #to fill the flow
  if(length(pos_ids) != 0 && length(neg_ids) != 0) {
    next_droplet <- sample(c(TRUE, FALSE), 1, prob = probs_next)
    if(next_droplet) {
      c3[length(c3) + 1] <- whole_flow[pos_ids[1]]
      pos_ids <- pos_ids[-1]
    } else {
      c3[length(c3) + 1] <- whole_flow[neg_ids[1]]
      neg_ids <- neg_ids[-1]
    }
  } else {
    if(length(pos_ids) != 0) {
      c3[length(c3) + 1] <- whole_flow[pos_ids[1]]
      pos_ids <- pos_ids[-1]
    } else {
      c3[length(c3) + 1] <- whole_flow[neg_ids[1]]
      neg_ids <- neg_ids[-1]
    }
  }
}

#case 4: less probable positive droplet at the end of experiment - position correlation (PC)
binary_flow <- whole_flow != 0
pos_ids <- sample(which(binary_flow))
neg_ids <- sample(which(!binary_flow))

c4 <- c()
first_droplet_pos <- sample(c(TRUE, FALSE), 1)
if(first_droplet_pos) {
  c4[1] <- whole_flow[pos_ids[1]]
  pos_ids <- pos_ids[-1]
} else {
  c4[1] <- whole_flow[neg_ids[1]]
  neg_ids <- neg_ids[-1]
}

while(length(c4) < length(whole_flow)) {
  #p_pos (probability of positive droplet) - decreases with the number of droplets
  p_pos <- (1 - length(c4)/length(whole_flow))*0.6
  #p_pos should be a function of length, on it depend how strong is correlation
  print(p_pos)
  probs_next <- c(p_pos, 1 - p_pos)
  
  if(length(pos_ids) != 0 && length(neg_ids) != 0) {
    next_droplet <- sample(c(TRUE, FALSE), 1, prob = probs_next)
    if(next_droplet) {
      c4[length(c4) + 1] <- whole_flow[pos_ids[1]]
      pos_ids <- pos_ids[-1]
    } else {
      c4[length(c4) + 1] <- whole_flow[neg_ids[1]]
      neg_ids <- neg_ids[-1]
    }
  } else {
    if(length(pos_ids) != 0) {
      c4[length(c4) + 1] <- whole_flow[pos_ids[1]]
      pos_ids <- pos_ids[-1]
    } else {
      c4[length(c4) + 1] <- whole_flow[neg_ids[1]]
      neg_ids <- neg_ids[-1]
    }
  }
}