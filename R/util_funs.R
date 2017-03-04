


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




# function to calculate the new elo rating for teams, based on:
# #https://metinmediamath.wordpress.com/2013/11/27/how-to-calculate-the-elo-rating-including-example/
calc_new_elo_rating <- function(w_elo, l_elo, k=32) {
  # requires the winning and losing team's previous ratings
  r_w <- 10^(w_elo/400)
  r_l <- 10^(l_elo/400)
  e_w <- r_w / (r_w + r_l)
  e_l <- r_l / (r_w + r_l)
  new_w_elo <- w_elo + (k * (1-e_w))
  new_l_elo <- l_elo + (k * (0-e_l))
  return(c(new_w_elo,new_l_elo))
}
# examples:
#calc_new_elo_rating(2000,2400)
#calc_new_elo_rating(2400,2000)




