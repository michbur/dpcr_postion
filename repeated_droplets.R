flow_length <- 45
flow_vals <- 15

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
c3 <- sample(1L:flow_length, flow_vals, prob = probs[sample(1L:(4*flow_length), flow_length)])
plot(c3, y = rep(1, flow_vals))


#case 4 less probable positive droplet at the end of experiment - position correlation (PC)
c4 <- sample(1L:flow_length, flow_vals, prob = 1L:flow_length/flow_length/2)
plot(c4, y = rep(1, flow_vals))


plot(x = c(c1, c2, c3, c4), y = c(rep(1, flow_vals), 
                                  rep(2, flow_vals), 
                                  rep(3, flow_vals), 
                                  rep(4, flow_vals)), 
     xlab = "Position", ylab = "", yaxt = "n")
axis(2, c(1, 2, 3, 4), labels = c("RAND", "RC", "URC", "PC"))

#positiona
dat <- matrix(c(sort(c1), sort(c2), sort(c3), sort(c4)), ncol = 4)
colnames(dat) <- c("RAND", "RC", "URC", "PC")


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

dists <- apply(dat, 2, calc_distances)
library(reshape2)
library(ggplot2)
mdists <- melt(dists)

#cumulative distribution function of distance
ggplot(mdists, aes(x = value)) + geom_density() + facet_wrap(~ Var2)
#observation: RC has multimodal distribution

#observation - PC can be detected by simple correlation test
apply(dists, 2, function(i)
  cor.test(x = i, y = 1L:flow_vals))

#does mean nearest neighbour distance does not differentiate between cases?
apply(dists, 2, mean)
anova(lm(value ~ Var2, data = mdists[, 2L:3]))
#not

library(multcomp)
pois_model <- glm(value ~ Var2, data = mdists[, 2L:3], family = poisson)
multi_comp <- glht(pois_model, linfct = mcp(Var2 = "Tukey"))
groups <- cld(multi_comp)
summary(multi_comp)
#cannot distinguish between URC and RC - it maybe because of the simulation