

#################################################
# create training data set
#################################################


setwd("C:/Users/Jeremy/Documents/GitHub/cbb_data/data/2017")
tourney_compact_results <- read.csv("TourneyCompactResults.csv")
regular_season_compact_results <- read.csv("RegularSeasonCompactResults.csv")  
teams <- read.csv("Teams.csv")
source("~/GitHub/cbb_data/R/util_funs.R")



# combine historical regular season and tournament results
train <- rbind(tourney_compact_results, regular_season_compact_results)

# add indicator field for tourney_gm
train$tourney_gm <- ifelse(test = train$Daynum > 133, yes = 1, no = 0)

# add game id field to train dataframe - tourney identifier only, not unique for regular season games
train$id <- ifelse(test = train$Wteam<train$Lteam, 
                   yes = paste0(train$Season,'_',train$Wteam,'_',train$Lteam),
                   no = paste0(train$Season,'_',train$Lteam,'_',train$Wteam))

# add outcome field - outcome is 1 if lower team id won
train$outcome <- ifelse(train$Wteam < train$Lteam, 1, 0)

# add in score_diff field for final score differential
train$score_diff <- ifelse(train$Wteam<train$Lteam, 
                           train$Wscore - train$Lscore, 
                           train$Lscore - train$Wscore)

# add fields for low/high team id
train$low_team_id <- ifelse(test = train$Wteam<train$Lteam, 
                            yes = train$Wteam, no = train$Lteam)

train$high_team_id <- ifelse(test = train$Wteam<train$Lteam, 
                             yes = train$Lteam, no = train$Wteam)


# Walk through data set day by day to calculate ongoing advanced metrics

# put training data set in correct order to loop through
train <- train[order(train$Season, train$Daynum),]

start_time <- Sys.time()
train_adv <- data.frame()
# loop through each season in the data set
#for (season in 1985) {
for (season in unique(train$Season)) {
  print(paste0(season))
  season_train <- train[train$Season==season,]
  # create teams list control structure df to keep track of records along the way
  teams_list <- data.frame(teamid = unique(union(season_train$Wteam, season_train$Lteam)), 
                           wins = 0, losses = 0, elo = 1000)
  # loop through each day of the season
  gamedays <- sort(unique(season_train$Daynum))
  for (day in gamedays) {
    # create a subset dataframe of only games played that day
    games_that_day <- season_train[season_train$Daynum==day,]

    # loop through each game that day
    for (game in 1:nrow(games_that_day)) {

      # create new fields in games_that_day for teams stats coming into the game
        # used as features in modeling to predict outcome of game
      # wins
      games_that_day$low_id_wins[game] <- teams_list[teams_list$teamid==games_that_day$low_team_id[game],"wins"]
      games_that_day$high_id_wins[game] <- teams_list[teams_list$teamid==games_that_day$high_team_id[game],"wins"]
      # losses
      games_that_day$low_id_losses[game] <- teams_list[teams_list$teamid==games_that_day$low_team_id[game],"losses"]
      games_that_day$high_id_losses[game] <- teams_list[teams_list$teamid==games_that_day$high_team_id[game],"losses"]
      # elo
      games_that_day$low_id_elo[game] <- teams_list[teams_list$teamid==games_that_day$low_team_id[game],"elo"]
      games_that_day$high_id_elo[game] <- teams_list[teams_list$teamid==games_that_day$high_team_id[game],"elo"]
      
      # only update for non-tourney games to create the proper training data set as of pre-tourney
      
      if (day < 133) {
        # update teams_list control structure with outcome of game
        winning_team <- games_that_day$Wteam[game]
        losing_team <- games_that_day$Lteam[game]
        # update wins
        teams_list[teams_list$teamid==winning_team,"wins"] <- teams_list[teams_list$teamid==winning_team,"wins"] + 1
        # update losses
        teams_list[teams_list$teamid==losing_team,"losses"] <- teams_list[teams_list$teamid==losing_team,"losses"] + 1
        # calculate new elo ratings
        new_ratings <- calc_new_elo_rating(teams_list[teams_list$teamid==winning_team,"elo"],
                                           teams_list[teams_list$teamid==losing_team,"elo"])
        # update elo ratings
        teams_list[teams_list$teamid==winning_team, "elo"] <- new_ratings[1]
        teams_list[teams_list$teamid==losing_team, "elo"] <- new_ratings[2]
      }
    }
    # add updated training records to train_adv
    train_adv <- rbind(train_adv, games_that_day)
  }
}


# look at teams_list
#teams_list <- merge(teams_list, teams, by.x = c("teamid"), by.y=c("Team_Id"))
#teams_list_sorted <- teams_list[order(teams_list$elo),]


# add field for elo_diff
train_adv$elo_diff <- train_adv$low_id_elo - train_adv$high_id_elo
#train_adv$elo_diff_sq <- (train_adv$low_id_elo - train_adv$high_id_elo)^2


train_adv_tourney <- train_adv[train_adv$tourney_gm == 1,]



# Final Step: Write training data csv back to git repo /data directory

# write train data set out to /data folder in git repo
#setwd("C:/Users/Jeremy/Documents/GitHub/cbb_data/data")
#write.csv(train_adv, "~/GitHub/cbb_data/data/train_adv.csv", row.names=FALSE)










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






# full ppp approach

setwd("C:/Users/Jeremy/Documents/GitHub/cbb_data/data/2017")
tourney_detailed_results <- read.csv("TourneyDetailedResults.csv")
regular_season_detailed_results <- read.csv("RegularSeasonDetailedResults.csv")

train <- rbind(tourney_detailed_results, regular_season_detailed_results)

# add possession fields
train$wposs <- train$Wfga - train$Wor + train$Wto + 0.475*train$Wfta
train$lposs <- train$Lfga - train$Lor + train$Lto + 0.475*train$Lfta
train$avgposs <- (train$wposs + train$lposs)/2
# calc points per possession (ppp)
train$wppp <- train$Wscore / train$avgposs
train$lppp <- train$Lscore / train$avgposs




# add indicator field for tourney_gm
train$tourney_gm <- ifelse(test = train$Daynum > 133, yes = 1, no = 0)

# add game id field to train dataframe - tourney identifier only, not unique for regular season games
train$id <- ifelse(test = train$Wteam<train$Lteam, 
                   yes = paste0(train$Season,'_',train$Wteam,'_',train$Lteam),
                   no = paste0(train$Season,'_',train$Lteam,'_',train$Wteam))

# add outcome field - outcome is 1 if lower team id won
train$outcome <- ifelse(train$Wteam < train$Lteam, 1, 0)

# add in score_diff field for final score differential
train$score_diff <- ifelse(train$Wteam<train$Lteam, 
                           train$Wscore - train$Lscore, 
                           train$Lscore - train$Wscore)

# add fields for low/high team id
train$low_team_id <- ifelse(test = train$Wteam<train$Lteam, 
                            yes = train$Wteam, no = train$Lteam)

train$high_team_id <- ifelse(test = train$Wteam<train$Lteam, 
                             yes = train$Lteam, no = train$Wteam)




# Walk through data set day by day to calculate ongoing advanced metrics

# put training data set in correct order to loop through
train <- train[order(train$Season, train$Daynum),]


# dev vars
season <- 2003
day <- gamedays[1]

train_adv <- data.frame()
# loop through each season in the data set
for (season in 2003) {
  #for (season in unique(train$Season)) {
  print(paste0(season))
  season_train <- train[train$Season==season,]
  # create teams list control structure df to keep track of records along the way
  teams_list <- data.frame(teamid = unique(union(season_train$Wteam, season_train$Lteam)), 
                           wins = 0, losses = 0,  # needed to calculate running average
                           raw_off_ppp = 0, raw_def_ppp = 0, 
                           avg_off_perf_idx = 1.0, avg_def_perf_idx = 1.0)
  # loop through each day of the season
  gamedays <- sort(unique(season_train$Daynum))
  for (day in gamedays) {
    # create a subset dataframe of only games played that day
    games_that_day <- season_train[season_train$Daynum==day,]
    
    # loop through each game that day
    for (game in 1:nrow(games_that_day)) {
      
      # create new fields in games_that_day for teams stats coming into the game
      # used as features in modeling to predict outcome of game
      # wins
      games_that_day$low_id_wins[game] <- teams_list[teams_list$teamid==games_that_day$low_team_id[game],"wins"]
      games_that_day$high_id_wins[game] <- teams_list[teams_list$teamid==games_that_day$high_team_id[game],"wins"]
      # losses
      games_that_day$low_id_losses[game] <- teams_list[teams_list$teamid==games_that_day$low_team_id[game],"losses"]
      games_that_day$high_id_losses[game] <- teams_list[teams_list$teamid==games_that_day$high_team_id[game],"losses"]

      # raw ppp
      games_that_day$low_id_raw_off_ppp[game] <- teams_list[teams_list$teamid==games_that_day$low_team_id[game],"raw_off_ppp"]
      games_that_day$high_id_raw_off_ppp[game] <- teams_list[teams_list$teamid==games_that_day$high_team_id[game],"raw_off_ppp"]
      games_that_day$low_id_raw_def_ppp[game] <- teams_list[teams_list$teamid==games_that_day$low_team_id[game],"raw_def_ppp"]
      games_that_day$high_id_raw_def_ppp[game] <- teams_list[teams_list$teamid==games_that_day$high_team_id[game],"raw_def_ppp"]
      
      # update teams_list control structure with outcome of game
      winning_team <- games_that_day$Wteam[game]
      losing_team <- games_that_day$Lteam[game]
      # update wins
      teams_list[teams_list$teamid==winning_team,"wins"] <- teams_list[teams_list$teamid==winning_team,"wins"] + 1
      # update losses
      teams_list[teams_list$teamid==losing_team,"losses"] <- teams_list[teams_list$teamid==losing_team,"losses"] + 1
      # update ppp
      teams_list[teams_list$teamid==winning_team, "raw_off_ppp"] <- teams_list[teams_list$teamid==winning_team, "raw_off_ppp"] * (wins + losses) + 
      #teams_list[teams_list$teamid==losing_team, "elo"] <- teams_list[teams_list$teamid==losing_team,"elo"] - (teams_list[teams_list$teamid==winning_team,"elo"])/10
    }
    # add updated training records to train_adv
    train_adv <- rbind(train_adv, games_that_day)
  }
}


