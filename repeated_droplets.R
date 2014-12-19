flow_length <- 150
flow_vals <- 50

#case 1 complete randomness
c1 <- sample(1L:flow_length, flow_vals)
plot(c1, y = rep(1, flow_vals))

#case 2 regular clusters
probs <- sin(1:flow_length)
probs[probs < 0] <- 0
c2 <- sample(1L:flow_length, flow_vals, prob = probs)
plot(c2, y = rep(1, flow_vals))

#case 3 unregular clusters - not sure if I implemented it correctly
probs <- sin(1:(4*flow_length))
probs[probs < 0] <- 0
c3 <- sample(1L:flow_length, flow_vals, prob = probs[sample(1L:(4*flow_length), 
                                                            flow_length)])
plot(c3, y = rep(1, flow_vals))

plot(x = c(c1, c2, c3), y = c(rep(1, flow_vals), rep(2, flow_vals), rep(3, flow_vals)), 
     xlab = "Position", ylab = "", yaxt = "n")
axis(2, c(1, 2, 3), labels = c("RAND", "RC", "URC"))

#positiona
dat <- matrix(c(sort(c1), sort(c2), sort(c3)), ncol = 3)
colnames(dat) <- c("RAND", "RC", "URC")


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

#cumulative distribution function of distance
plot(density(calc_distances(c1)))
#mean nearest neighbour distance
mean(calc_distances(c1))
