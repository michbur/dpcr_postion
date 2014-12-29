#random clusters
whole_flow <- sample(c(0, 1))
for (i in 2L:flow_length) {
  p_bigger <- sample(75L:100, 1)/100
  probs_01 <- if(c3[i - 1] == 0) {
    c(p_bigger, 1 - p_bigger)
  } else {
    c(1 - p_bigger, p_bigger)
  }
  whole_flow[i] <- sample(c(0, 1), 1, prob = probs_01)
}

#attempt 2 - simulate whole flow and reorder droplets to get five cases
whole_flow <- as.vector(slot(sim_ddpcr(50, 200, 2000, n_exp = 1, dube = TRUE), ".Data"))

#case 1 complete randomness
#we dont have to do sample again, but for sake of randomness
c1 <- sample(whole_flow)

#case 2 regular clusters
#NOT DONE!


#case 3 unregular clusters - not sure if I implemented it correctly
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