#random clusters
whole_flow <- sample(c(0, 1))
for (i in 2L:flow_length) {
  p_bigger <- sample(75L:100, 1)/100
  probs_01 <- if(c3[i - 1] == 0) {
    c(p_bigger, 1 - p_bigger)
  } else {
    c(1 - p_bigger, p_bigger)
  }
  c3[i] <- sample(c(0, 1), 1, prob = probs_01)
}