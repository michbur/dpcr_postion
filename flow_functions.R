#a set of functions for simulating various flows

generate_flow <- function(in_flow, probs_next) {
  binary_flow <- in_flow != 0
  #resample droplets
  pos_ids <- sample(which(binary_flow))
  neg_ids <- sample(which(!binary_flow))
  
  out_flow <- c()
  first_droplet_pos <- sample(c(TRUE, FALSE), 1)
  if(first_droplet_pos) {
    out_flow[1] <- in_flow[pos_ids[1]]
    pos_ids <- pos_ids[-1]
  } else {
    out_flow[1] <- in_flow[neg_ids[1]]
    neg_ids <- neg_ids[-1]
  }
  
  while(length(out_flow) < length(in_flow)) {
    #if clause below takes care of situation in which we don't have more positive/negative droplets 
    #to fill the flow
    if(length(pos_ids) != 0 && length(neg_ids) != 0) {
      next_droplet <- sample(c(TRUE, FALSE), 1, prob = probs_next(in_flow, out_flow))
      if(next_droplet) {
        out_flow[length(out_flow) + 1] <- in_flow[pos_ids[1]]
        pos_ids <- pos_ids[-1]
      } else {
        out_flow[length(out_flow) + 1] <- in_flow[neg_ids[1]]
        neg_ids <- neg_ids[-1]
      }
    } else {
      if(length(pos_ids) != 0) {
        out_flow[length(out_flow) + 1] <- in_flow[pos_ids[1]]
        pos_ids <- pos_ids[-1]
      } else {
        out_flow[length(out_flow) + 1] <- in_flow[neg_ids[1]]
        neg_ids <- neg_ids[-1]
      }
    }
  }
  out_flow
}


probs_RC <- function(in_flow, out_flow) {
  #p_pos (probability of positive droplet) 
  p_pos <- sin(length(out_flow))
  p_pos <- ifelse(p_pos < 0, 0, p_pos)
  c(p_pos, 1 - p_pos)
}



probs_URC <- function(in_flow, out_flow) {
  #URC need only out_flow, in_flow is here for sake of compatibility
  #by manipulating these probabilities clusters can be shorter or longer. The more p_bigger
  #is closer to 0.5, the shorter clusters
  p_bigger <- sample(75L:100, 1)/100
  probs_next <- if(out_flow[length(out_flow)] != 0) {
    c(p_bigger, 1 - p_bigger)
  } else {
    c(1 - p_bigger, p_bigger)
  }
  probs_next
}

probs_PC <- function(in_flow, out_flow) {
  #p_pos (probability of positive droplet) - decreases with the number of droplets
  p_pos <- (1 - length(out_flow)/length(in_flow))*0.8
  #p_pos should be a function of length, on it depend how strong is correlation
  c(p_pos, 1 - p_pos)
}

#get distance of nearest neighbour
calc_distances <- function(x) {
  x <- sort(x)
  #1--2 elements
  c(abs(x[1] - x[2]),
    #elements in middle
    sapply(2L:(length(x) - 1), function (i)
      min(abs(c(x[i] - x[i - 1], x[i] - x[i + 1])))),
    #n-1--n element
    abs(x[length(x)] - x[length(x) - 1]))
}