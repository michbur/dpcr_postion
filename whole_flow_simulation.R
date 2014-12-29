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

#case 3 unregular clusters - not sure if I implemented it correctly
binary_flow <- sort(whole_flow) != 0
pos_ids <- sample(which(binary_flow))
neg_ids <- sample(which(!binary_flow))

c3 <- c()
first_droplet_pos <- sample(c(TRUE, FALSE), 1)
if(first_droplet_pos) {
  c3[1] <- pos_ids[1]
  pos_ids <- pos_ids[-1]
} else {
  c3[1] <- neg_ids[1]
  neg_ids <- neg_ids[-1]
}