


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


