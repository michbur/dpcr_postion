library(dpcR)
source("flow_functions.R")
#attempt 2 - simulate whole flow and reorder droplets to get five cases
whole_flow <- as.vector(slot(sim_ddpcr(7500, 20000, 2000000, n_exp = 1, dube = TRUE), ".Data"))

#case 1: complete randomness (RAND)
#we dont have to do sample again, but for sake of randomness
c1 <- sample(whole_flow)

#case 2: regular clusters (RC)
c2 <- generate_flow(whole_flow, probs_RC)
#problem - too many positives at the end of flow

#case 3: unregular clusters (URC)
c3 <- generate_flow(whole_flow, probs_URC)


#case 4: less probable positive droplet at the end of experiment - position correlation (PC)
c4 <- generate_flow(whole_flow, probs_PC)


#issue c3 and c4 leaves long streak of empty droplets at the end. Fix generation.
dat <- do.call(cbind, lapply(list(c1, c2, c3, c4), function(i)
  which(i != 0)))

colnames(dat) <- c("RAND", "RC", "URC", "PC")

dists <- apply(dat, 2, calc_distances)

library(reshape2)
library(ggplot2)
mdists <- melt(dists)

#cumulative distribution function of distance
ggplot(mdists, aes(x = value)) + geom_density() + facet_wrap(~ Var2)
ggplot(mdists, aes(x = value, fill = Var2)) + geom_density(alpha = 0.5)

apply(dists, 2, function(i)
  cor.test(x = i, y = 1L:nrow(dists)))


apply(dists, 2, mean)
anova(lm(value ~ Var2, data = mdists[, 2L:3]))


library(multcomp)
pois_model <- glm(value ~ Var2, data = mdists[, 2L:3], family = poisson)
multi_comp <- glht(pois_model, linfct = mcp(Var2 = "Tukey"))
groups <- cld(multi_comp)
summary(multi_comp)