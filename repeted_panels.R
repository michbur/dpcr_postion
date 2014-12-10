library(dpcR)

move_partitions <- function(bpan) {
  mpan <- matrix(slot(bpan, ".Data"), nrow = 45)
  #old positions
  old_pos <- which(mpan == 1, arr.ind = TRUE)
  
  #random movement
  movement_row <- sample(-1L:1, sum(mpan), replace = TRUE)
  movement_col <- sample(-1L:1, sum(mpan), replace = TRUE)
  
  #new positions
  new_pos <- old_pos
  new_pos[, "row"] <- new_pos[, "row"] + movement_row
  new_pos[, "col"] <- new_pos[, "col"] + movement_col
  #remove impossible moves
  new_pos[new_pos < 1] <- 1
  new_pos[, "col"][new_pos[, "col"] > 17] <- 17
  new_pos[, "row"][new_pos[, "row"] > 45] <- 45
  
  #create new panel
  new_panel <- matrix(0, nrow = 45, ncol = 17)
  for (i in 1L:nrow(new_pos)) 
    new_panel[new_pos[i, "row"], new_pos[i, "col"]] <- 1
  
  res <- bpan
  slot(res, ".Data") <- matrix(as.vector(new_panel), ncol = 1)
  res
}

#our pattern
pan <- sim_adpcr(m = 200, n = 765, times = 1000, pos_sums = FALSE,
                 n_panels = 1)
#binarize
bpan <- binarize(pan)

reps <- lapply(1L:4, function(dummy) 
  adpcr2ppp(move_partitions(bpan), 45, 17))
