


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


# next steps:
# build models based on mpm 
# factor in score differential into multiplier
# add in adjustment for overtime
#   ot_multiplier <- 1.0
# optimize home/away multipliers



