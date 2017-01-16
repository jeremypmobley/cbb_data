


# clean the seed function
clean_seed <- function(seedguy){
  seedguy <- gsub("W","",seedguy)
  seedguy <- gsub("X","",seedguy)
  seedguy <- gsub("Y","",seedguy)
  seedguy <- gsub("Z","",seedguy)
  seedguy <- gsub("a","",seedguy)
  seedguy <- gsub("b","",seedguy)
  seedguy <- as.numeric(seedguy)
}



# function to calculate the logloss given preds, actuals
calc_logloss <- function(preds, actuals){
  loglossguy <- (actuals * log(preds)) + ((1-actuals)*(log(1-preds)))
  return(-1/length(loglossguy) * sum(loglossguy))
}



