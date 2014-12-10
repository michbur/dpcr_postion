#case 1 complete randomness
plot(sample(1L:45, 15), y = rep(1, 15))

#case 2 regular clusters
probs <- sin(1:45)
probs[probs < 0] <- 0
plot(sample(1L:45, 15, prob = probs), y = rep(1, 15))

#case 3 unregular clusters
