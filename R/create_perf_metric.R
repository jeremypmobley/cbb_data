

# Next steps:
# move all logic from this create_perf_metric script to create_train



#################################################################################
### LOAD DATA
#################################################################################

# set wd to be repo home
setwd("~/GitHub/cbb_data/data")

teams <- read.csv("Teams.csv")
regular_season_compact_results <- read.csv("RegularSeasonCompactResults.csv")
# tourney_compact_results <- read.csv("TourneyCompactResults.csv")



### performance metric development

# subset df to only 2015 results
reg_season_results <- regular_season_compact_results[regular_season_compact_results$Season==2015,]
# add in high, low team id fields
reg_season_results$low_team_id <- ifelse(test = reg_season_results$Wteam<reg_season_results$Lteam,                             
                            yes = reg_season_results$Wteam,
                            no = reg_season_results$Lteam)

reg_season_results$high_team_id <- ifelse(test = reg_season_results$Wteam<reg_season_results$Lteam, 
                             yes = reg_season_results$Lteam,
                             no = reg_season_results$Wteam)



mpm_base <- 1000
teams_list <- data.frame(teamid = unique(union(reg_season_results$Wteam, reg_season_results$Lteam)), 
                         mpm = mpm_base)
teams_list <- merge(x = teams_list, y = teams, by.x = c("teamid"), by.y = c("Team_Id"))
# loop through each game in season
for (game in 1:nrow(reg_season_results)) {
  winning_team <- reg_season_results$Wteam[game]
  losing_team <- reg_season_results$Lteam[game]
  win_loc <- reg_season_results$Wloc[game]
  #score_differential <- reg_season_results$Wscore[game] - reg_season_results$Lscore[game]
  score_differential <- (reg_season_results$Wscore[game] - reg_season_results$Lscore[game]) / 3
  winning_team_mpm <- teams_list[teams_list$teamid==winning_team,"mpm"]
  losing_team_mpm <- teams_list[teams_list$teamid==losing_team,"mpm"]
  home_multiplier <- 0.9
  away_multiplier <- 1.0
  winning_team_adjustment <- score_differential * (losing_team_mpm/mpm_base)
  losing_team_adjustment <- score_differential * (winning_team_mpm/mpm_base)
  if (win_loc == "N") {
    teams_list[teams_list$teamid==winning_team,"mpm"] <- teams_list[teams_list$teamid==winning_team,"mpm"] + (1 * winning_team_adjustment)
    teams_list[teams_list$teamid==losing_team,"mpm"] <- teams_list[teams_list$teamid==losing_team,"mpm"] - (1 * losing_team_adjustment)
  }
  if (win_loc == "A") {
    teams_list[teams_list$teamid==winning_team,"mpm"] <- teams_list[teams_list$teamid==winning_team,"mpm"] + (1 * away_multiplier * winning_team_adjustment)
    teams_list[teams_list$teamid==losing_team,"mpm"] <- teams_list[teams_list$teamid==losing_team,"mpm"] - (1 * home_multiplier * losing_team_adjustment)
  }
  if (win_loc == "H") {
    teams_list[teams_list$teamid==winning_team,"mpm"] <- teams_list[teams_list$teamid==winning_team,"mpm"] + (1 * home_multiplier * winning_team_adjustment)
    teams_list[teams_list$teamid==losing_team,"mpm"] <- teams_list[teams_list$teamid==losing_team,"mpm"] - (1 * away_multiplier * losing_team_adjustment)
  }
  # print Kentucky mpm after each game
  if (winning_team == 1246) {
    print(paste0(game, " ",winning_team_mpm))
  }
}

head(teams_list[order(-teams_list$mpm),], n = 10)

#hist(teams_list$mpm)







teams_list <- data.frame(teamid = unique(union(regular_season_compact_results$Wteam, regular_season_compact_results$Lteam)), 
                         wins = 0, losses = 0)
#teams_list <- merge(x = teams_list, y = teams, by.x = c("teamid"), by.y = c("Team_Id"))

for (game in 1:nrow(regular_season_compact_results)) {
  winning_team <- regular_season_compact_results$Wteam[game]
  losing_team <- regular_season_compact_results$Lteam[game]
  teams_list[teams_list$teamid==winning_team,"wins"] <- teams_list[teams_list$teamid==winning_team,"wins"] + 1
  teams_list[teams_list$teamid==losing_team,"losses"] <- teams_list[teams_list$teamid==losing_team,"losses"] + 1
}

for (game in 1:nrow(reg_season_results)) {
  winning_team <- reg_season_results$Wteam[game]
  losing_team <- reg_season_results$Lteam[game]
  teams_list[teams_list$teamid==winning_team,"wins"] <- teams_list[teams_list$teamid==winning_team,"wins"] + 1
  teams_list[teams_list$teamid==losing_team,"losses"] <- teams_list[teams_list$teamid==losing_team,"losses"] + 1
}



# put training data set in correct order to loop through
train <- train[order(train$Season, train$Daynum),]

# initialize empty fields
train$low_id_reg_season_wins <- 0
train$high_id_reg_season_wins <- 0


new_train <- data.frame()
# loop through each season in the data set
for (season in 1985) {
#for (season in unique(train$Season)) {
  print(paste0(season))
  # create teams list control structure df to keep track of records along the way
  teams_list <- data.frame(teamid = unique(union(train[train$Season==season,"Wteam"], 
                                                 train[train$Season==season,"Lteam"])), 
                           wins = 0, losses = 0)
  # loop through each day of the season
  for (day in seq(1,200)) {
    # create a subset dataframe of only games played that day
    games_that_day <- train[train$Season==season & train$Daynum==day,]
    # loop through each game that day
    for (game in 1:nrow(games_that_day)) {
      if (nrow(games_that_day)>0) {
        # update train record
        games_that_day$low_id_reg_season_wins[game] <- teams_list[teams_list$teamid==games_that_day$low_team_id[game],"wins"]
        games_that_day$high_id_reg_season_wins[game] <- teams_list[teams_list$teamid==games_that_day$high_team_id[game],"wins"]
        
        # update teams_list control structure with outcome of game
        winning_team <- games_that_day$Wteam[game]
        losing_team <- games_that_day$Lteam[game]    
        teams_list[teams_list$teamid==winning_team,"wins"] <- teams_list[teams_list$teamid==winning_team,"wins"] + 1
        teams_list[teams_list$teamid==losing_team,"losses"] <- teams_list[teams_list$teamid==losing_team,"losses"] + 1
      }
    }
    # add updated training records to new_train
    new_train <- rbind(new_train, games_that_day)
  }
}





# next steps:
# factor in score differential into multiplier
# add in adjustment for overtime
#   ot_multiplier <- 1.0
# optimize home/away multipliers








