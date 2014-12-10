#case 1 complete randomness
c1 <- sample(1L:45, 15)
plot(c1, y = rep(1, 15))

#case 2 regular clusters
probs <- sin(1:45)
probs[probs < 0] <- 0
c2 <- sample(1L:45, 15, prob = probs)
plot(c2, y = rep(1, 15))

#case 3 unregular clusters - not sure if I implemented it correctly
probs <- sin(1:180)
probs[probs < 0] <- 0
c3 <- sample(1L:45, 15, prob = probs[sample(1L:180, 45)])
plot(c3, y = rep(1, 15))

plot(c(c1, c2, c3), y = c(rep(1, 15), rep(2, 15), rep(3, 15)), xlab = "Position", ylab = "", 
     yaxt = "n")
axis(2, c(1, 2, 3), labels = c("RAND", "RC", "URC"))
