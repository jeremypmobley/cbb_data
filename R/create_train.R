

#################################################
# create training data set of past tournament games
#################################################


# assumes tourney_compact_results is loaded

create_train <- function(tourney_compact_results) {
  
  train <- tourney_compact_results
  train$Wloc <- NULL  # remove unnecessary field
  
  # exclude play-in games from train
  #train <- train[train$Daynum!=134,]
  #train <- train[train$Daynum!=135,]
  
  # add game id field to train dataframe
  train$id <- ifelse(test = train$Wteam<train$Lteam, 
                     yes = paste0(train$Season,'_',train$Wteam,'_',train$Lteam),
                     no = paste0(train$Season,'_',train$Lteam,'_',train$Wteam))
  
  # outcome is 1 if lower team id won
  train$outcome <- ifelse(train$Wteam<train$Lteam,1,0)
  
  # add fields for low/high team id
  train$low_team_id <- ifelse(test = train$Wteam<train$Lteam,                             
                              yes = train$Wteam,
                              no = train$Lteam)
  
  train$high_team_id <- ifelse(test = train$Wteam<train$Lteam, 
                               yes = train$Lteam,
                               no = train$Wteam)
  
  wins_per_year <- data.frame(table(regular_season_compact_results$Wteam,regular_season_compact_results$Season))
  names(wins_per_year) <- c('team_id','season','wins')
  losses_per_year <- data.frame(table(regular_season_compact_results$Lteam,regular_season_compact_results$Season))
  names(losses_per_year) <- c('team_id','season','losses')
  
  records_per_year <- merge(wins_per_year, losses_per_year)
  records_per_year$win_pct <- records_per_year$wins/(records_per_year$wins + records_per_year$losses)
  rm(wins_per_year, losses_per_year)
  
  # add win/loss pct data to train
  train <- merge(x = train, y = records_per_year, by.x=c("low_team_id", "Season"), by.y = c("team_id", "season"), all.x=TRUE)
  names(train)[length(train)-2] <- "low_id_wins"
  names(train)[length(train)-1] <- "low_id_losses"
  names(train)[length(train)] <- "low_id_win_pct"
  
  train <- merge(x = train, y = records_per_year, by.x=c("high_team_id", "Season"), by.y = c("team_id", "season"), all.x=TRUE)
  names(train)[length(train)-2] <- "high_id_wins"
  names(train)[length(train)-1] <- "high_id_losses"
  names(train)[length(train)] <- "high_id_win_pct"
  
  rm(records_per_year)
  
  # create diff variable
  train$win_pct_diff <- train$low_id_win_pct - train$high_id_win_pct
  
  return(train)
}








# Add in tourney seeds 

# # load in tourney seeds lookup
# tourney_seeds <- read.csv("TourneySeeds.csv")
# 
# source("C:/Users/Jeremy/Documents/GitHub/cbb_data/R/util_funs.R")
# 
# 
# 
# # add in tourney seeds
# train <- merge(x = train, y = tourney_seeds, by.x = c('low_team_id', "Season"), by.y = c('Team', "Season"))
# names(train)[length(names(train))] <- "low_id_team_seed"
# 
# train <- merge(x = train, y = tourney_seeds, by.x = c('high_team_id', "Season"), by.y = c('Team', "Season"))
# names(train)[length(names(train))] <- "high_id_team_seed"
# 
# 
# train$low_id_team_seed <- clean_seed(train$low_id_team_seed)
# train$high_id_team_seed <- clean_seed(train$high_id_team_seed)
# 
# # create seed_diff var
# train$seed_diff <- train$low_id_team_seed - train$high_id_team_seed
# 
# # seed benchmark pred
# train$seed_benchmark_pred <- 0.5 - (train$seed_diff * 0.03)




