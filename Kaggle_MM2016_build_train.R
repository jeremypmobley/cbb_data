

### Kaggle March Madness 2016 ###

### Author: Jeremy Mobley ###


# Building training data sets



#################################################################################
### LOAD DATA
#################################################################################
setwd("~/Desktop/kaggle/march_madness_2016")

#tourney_slots <- read.csv("TourneySlots.csv")
tourney_seeds <- read.csv("TourneySeeds.csv")
tourney_compact_results <- read.csv("TourneyCompactResults.csv")
teams <- read.csv("Teams.csv")
#sample_sub <- read.csv("SampleSubmission.csv")
regular_season_compact_results <- read.csv("RegularSeasonCompactResults.csv")
regular_season_detailed_results <- read.csv("RegularSeasonDetailedResults.csv")
conferences <- read.csv("TeamConferences.csv")



#################################################################################
###### create training data set for tournament games since 2003 #########
#################################################################################

train <- tourney_compact_results
train$Wloc <- NULL  # remove unnecessary field
train <- train[train$Season>=2003,]
# exclude play-in games from train
train <- train[train$Daynum!=134,]
train <- train[train$Daynum!=135,]
row.names(train) <- NULL

train$id <- ifelse(test = train$Wteam<train$Lteam, 
                   yes = paste0(train$Season,'_',train$Wteam,'_',train$Lteam),
                   no = paste0(train$Season,'_',train$Lteam,'_',train$Wteam))

# outcome is 1 if lower team id won
train$outcome <- ifelse(train$Wteam<train$Lteam,1,0)
train$pointsdiff_outcome <- ifelse(train$Wteam<train$Lteam,
                                   train$Wscore - train$Lscore,
                                   train$Lscore - train$Wscore)

train$low_team_id <- ifelse(test = train$Wteam<train$Lteam, 
                            yes = train$Wteam,
                            no = train$Lteam)

train$high_team_id <- ifelse(test = train$Wteam<train$Lteam, 
                             yes = train$Lteam,
                             no = train$Wteam)



# add team names
train <- merge(x = train, y = teams, by.x = c('low_team_id'), by.y = c('Team_Id'))
names(train)[length(names(train))] <- "low_id_team_name"

train <- merge(x = train, y = teams, by.x = c('high_team_id'), by.y = c('Team_Id'))
names(train)[length(names(train))] <- "high_id_team_name"




# add in tourney seeds
train <- merge(x = train, y = tourney_seeds, by.x = c('low_team_id', "Season"), by.y = c('Team', "Season"))
names(train)[length(names(train))] <- "low_id_team_seed"

train <- merge(x = train, y = tourney_seeds, by.x = c('high_team_id', "Season"), by.y = c('Team', "Season"))
names(train)[length(names(train))] <- "high_id_team_seed"


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

train$low_id_team_seed <- clean_seed(train$low_id_team_seed)
train$high_id_team_seed <- clean_seed(train$high_id_team_seed)

# create seed_diff var
train$seed_diff <- train$low_id_team_seed - train$high_id_team_seed


# create round var
train$round <- 0
train$round <- ifelse(test = train$Daynum %in% c(136, 137), yes = 1, no = train$round)
train$round <- ifelse(test = train$Daynum %in% c(138, 139), yes = 2, no = train$round)
train$round <- ifelse(test = train$Daynum %in% c(143, 144), yes = 3, no = train$round)
train$round <- ifelse(test = train$Daynum %in% c(145, 146), yes = 4, no = train$round)
train$round <- ifelse(test = train$Daynum %in% c(152), yes = 5, no = train$round)
train$round <- ifelse(test = train$Daynum %in% c(154), yes = 6, no = train$round)


# summary field
train$summary <- ifelse(test = train$outcome==1, 
                        yes = paste0(train$Season, ": ", "R", train$round, " ", train$low_id_team_seed, " ", levels(train$low_id_team_name)[train$low_id_team_name], " def ", train$high_id_team_seed, " ", levels(train$high_id_team_name)[train$high_id_team_name], " ", train$Wscore, "-", train$Lscore),
                        no = paste0(train$Season, ": ", "R", train$round, " ", train$high_id_team_seed, " ", levels(train$high_id_team_name)[train$high_id_team_name], " def ", train$low_id_team_seed, " ", levels(train$low_id_team_name)[train$low_id_team_name], " ", train$Wscore, "-", train$Lscore))


# add in conference fields
train <- merge(x = train, y = conferences, by.x=c("Season", "low_team_id"), by.y=c("season", "team_id"))
names(train)[length(train)] <- "low_id_conference"

train <- merge(x = train, y = conferences, by.x=c("Season", "high_team_id"), by.y=c("season", "team_id"))
names(train)[length(train)] <- "high_id_conference"



# clean up environment
rm(teams, tourney_seeds, clean_seed, tourney_compact_results)

#################################################################################













######### Create raw PPP for all years #########


'''
# create ppp metric for all games in regular_season_detailed_results
#regular_season_detailed_results$wposs <- regular_season_detailed_results$Wfga - regular_season_detailed_results$Wor + regular_season_detailed_results$Wto + 0.475*regular_season_detailed_results$Wfta
#regular_season_detailed_results$lposs <- regular_season_detailed_results$Lfga - regular_season_detailed_results$Lor + regular_season_detailed_results$Lto + 0.475*regular_season_detailed_results$Lfta
#regular_season_detailed_results$avgposs <- (regular_season_detailed_results$wposs + regular_season_detailed_results$lposs)/2
#regular_season_detailed_results$wppp <- regular_season_detailed_results$Wscore / regular_season_detailed_results$avgposs
#regular_season_detailed_results$lppp <- regular_season_detailed_results$Lscore / regular_season_detailed_results$avgposs


# create data frame to track adjusted ppp metrics
ppp_losers <- data.frame(team_id = regular_season_detailed_results$Lteam,
season = regular_season_detailed_results$Season)
ppp_winners <- data.frame(team_id = regular_season_detailed_results$Wteam,
season = regular_season_detailed_results$Season)
ppp_all <- rbind(ppp_losers, ppp_winners)
ppp <- unique(ppp_all)
rm(ppp_winners, ppp_losers, ppp_all)

ppp$raw_avg_off_ppp=0
ppp$raw_avg_def_ppp=0


# loop through each team, season to calc raw ppp metrics
for(i in 1:nrow(ppp)){
teamid <- ppp$team_id[i]  # loop through team_ids
season <- ppp$season[i]  # loop through seasons
# create subset of games for team
reg_season_results <- regular_season_detailed_results[regular_season_detailed_results$Wteam==teamid | regular_season_detailed_results$Lteam==teamid,]
reg_season_results <- reg_season_results[reg_season_results$Season==season,]

# calculate metrics
reg_season_results$off_ppp <- ifelse(test = reg_season_results$Wteam==teamid, 
yes = reg_season_results$wppp, no = reg_season_results$lppp)
reg_season_results$def_ppp <- ifelse(test = reg_season_results$Wteam==teamid, 
yes = reg_season_results$lppp, no = reg_season_results$wppp)
# update data frame
ppp$raw_avg_off_ppp[i] <- mean(reg_season_results$off_ppp)
ppp$raw_avg_def_ppp[i] <- mean(reg_season_results$def_ppp)
if (i==round(nrow(ppp)/4)) print("25% done")
if (i==round(nrow(ppp)/2)) print("50% done")
if (i==round(nrow(ppp)*3/4)) print("75% done")  
}


write.csv(ppp, "ppp_raw_all_teams.csv", row.names=FALSE)
'''


ppp <- read.csv("ppp_raw_all_teams.csv")


# add raw ppp data to train
train <- merge(x = train, y = ppp, by.x=c("low_team_id", "Season"), by.y = c("team_id", "season"), all.x=TRUE)
names(train)[length(train)-1] <- "low_id_raw_avg_off_ppp"
names(train)[length(train)] <- "low_id_raw_avg_def_ppp"

train <- merge(x = train, y = ppp, by.x=c("high_team_id", "Season"), by.y = c("team_id", "season"), all.x=TRUE)
names(train)[length(train)-1] <- "high_id_raw_avg_off_ppp"
names(train)[length(train)] <- "high_id_raw_avg_def_ppp"


#rm(ppp, regular_season_detailed_results)

#################################################################################











############################################################################################################
# Win/Loss percentage

# calculate raw win/loss percentage per team per year
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

# create diff variable
train$win_pct_diff <- train$low_id_win_pct - train$high_id_win_pct



################################
# add in conference avg win_pct
################################
records_per_year <- records_per_year[!is.na(records_per_year$win_pct),]
records_per_year <- merge(x = records_per_year, y = conferences, by.x=c("season", "team_id"), by.y=c("season", "team_id"))


all_conf_raw_win_pct_avgs <- aggregate(x = records_per_year$win_pct, 
                                   by = list(records_per_year$conference, records_per_year$season), FUN = mean)
names(all_conf_raw_win_pct_avgs) <- c("conference", "season", "avg_conf_raw_win_pct")

all_conf_raw_win_pct_medians <- aggregate(x = records_per_year$win_pct, 
                                       by = list(records_per_year$conference, records_per_year$season), FUN = median)
names(all_conf_raw_win_pct_medians) <- c("conference", "season", "median_conf_raw_win_pct")


# add to train
train <- merge(x = train, y = all_conf_raw_win_pct_avgs, by.x=c("low_id_conference", "Season"), by.y = c("conference", "season"), all.x=TRUE)
names(train)[length(train)] <- "low_id_avg_conf_raw_win_pct"
train <- merge(x = train, y = all_conf_raw_win_pct_avgs, by.x=c("high_id_conference", "Season"), by.y = c("conference", "season"), all.x=TRUE)
names(train)[length(train)] <- "high_id_avg_conf_raw_win_pct"

train <- merge(x = train, y = all_conf_raw_win_pct_medians, by.x=c("low_id_conference", "Season"), by.y = c("conference", "season"), all.x=TRUE)
names(train)[length(train)] <- "low_id_median_conf_raw_win_pct"
train <- merge(x = train, y = all_conf_raw_win_pct_medians, by.x=c("high_id_conference", "Season"), by.y = c("conference", "season"), all.x=TRUE)
names(train)[length(train)] <- "high_id_median_conf_raw_win_pct"


# create diff variables
train$avg_conf_raw_win_pct_diff <- train$low_id_avg_conf_raw_win_pct - train$high_id_avg_conf_raw_win_pct
train$median_conf_raw_win_pct_diff <- train$low_id_median_conf_raw_win_pct - train$high_id_median_conf_raw_win_pct



#rm(records_per_year, all_conf_raw_win_pct_avgs, all_conf_raw_win_pct_medians)

############################################################################################################







# conference records
regular_season_compact_results <- merge(x = regular_season_compact_results, y = conferences, by.x=c("Season", "Wteam"), by.y=c("season", "team_id"))
names(regular_season_compact_results)[length(regular_season_compact_results)] <- "Wteam_conference"
regular_season_compact_results <- merge(x = regular_season_compact_results, y = conferences, by.x=c("Season", "Lteam"), by.y=c("season", "team_id"))
names(regular_season_compact_results)[length(regular_season_compact_results)] <- "Lteam_conference"

conf_regular_season_compact_results <- regular_season_compact_results[regular_season_compact_results$Wteam_conference==regular_season_compact_results$Lteam_conference,]

conf_wins_per_year <- data.frame(table(conf_regular_season_compact_results$Wteam,conf_regular_season_compact_results$Season))
names(conf_wins_per_year) <- c('team_id','season','conf_wins')
conf_losses_per_year <- data.frame(table(conf_regular_season_compact_results$Lteam,conf_regular_season_compact_results$Season))
names(conf_losses_per_year) <- c('team_id','season','conf_losses')
conf_records_per_year <- merge(conf_wins_per_year, conf_losses_per_year)
conf_records_per_year$conf_win_pct <- conf_records_per_year$conf_wins/(conf_records_per_year$conf_wins + conf_records_per_year$conf_losses)
rm(conf_wins_per_year, conf_losses_per_year)


# add conf_win_pct data to train
train <- merge(x = train, y = conf_records_per_year, by.x=c("low_team_id", "Season"), by.y = c("team_id", "season"), all.x=TRUE)
names(train)[length(train)-2] <- "low_id_conf_wins"
names(train)[length(train)-1] <- "low_id_conf_losses"
names(train)[length(train)] <- "low_id_conf_win_pct"

train <- merge(x = train, y = conf_records_per_year, by.x=c("high_team_id", "Season"), by.y = c("team_id", "season"), all.x=TRUE)
names(train)[length(train)-2] <- "high_id_conf_wins"
names(train)[length(train)-1] <- "high_id_conf_losses"
names(train)[length(train)] <- "high_id_conf_win_pct"







# margin of victory
regular_season_compact_results$victorymargin <- regular_season_compact_results$Wscore - regular_season_compact_results$Lscore

winning_margins <- aggregate(x = regular_season_compact_results$victorymargin, by=list(regular_season_compact_results$Wteam,regular_season_compact_results$Season), FUN=sum)
names(winning_margins) <- c('teamid','season','winning_tot_margin')
losing_margins <- aggregate(x = regular_season_compact_results$victorymargin, by=list(regular_season_compact_results$Lteam,regular_season_compact_results$Season), FUN=sum)
names(losing_margins) <- c('teamid','season','losing_tot_margins')
allmargins <- merge(x = winning_margins, y = losing_margins)
allmargins$total_margin <- allmargins$winning_tot_margin - allmargins$losing_tot_margins

allmargins$winning_tot_margin <- NULL
allmargins$losing_tot_margins <- NULL




# add to train
train <- merge(x = train, y = allmargins, by.x=c("low_team_id", "Season"), by.y = c("teamid", "season"), all.x=TRUE)
names(train)[length(train)] <- "low_id_total_margin"
train <- merge(x = train, y = allmargins, by.x=c("high_team_id", "Season"), by.y = c("teamid", "season"), all.x=TRUE)
names(train)[length(train)] <- "high_id_total_margin"

train$low_id_avg_margin <- train$low_id_total_margin / (train$low_id_wins + train$low_id_losses)
train$high_id_avg_margin <- train$high_id_total_margin / (train$high_id_wins + train$high_id_losses)


# create diff variable
train$avg_margin_diff <- train$low_id_avg_margin - train$high_id_avg_margin



#rm(winning_margins, losing_margins, allmargins, regular_season_compact_results)
#rm(conf_records_per_year, conf_regular_season_compact_results)













############################################################################################################

#### Calculate end of regular season elo rating for each team for each season

library("data.table")
library("PlayerRatings")

elo_regular_season_compact_results <- fread("RegularSeasonCompactResults.csv")


all_ratings <- data.frame()
for (season in unique(elo_regular_season_compact_results$Season)){
  seasonDataDt <- elo_regular_season_compact_results[Season == season, .(Daynum, Wteam, Lteam, Wloc)]
  resultVector <- rep(1, nrow(seasonDataDt))
  advantageVector <- as.numeric(seasonDataDt$Wloc == "H")
  seasonDataDf <- data.frame(yearDay = seasonDataDt$Daynum,
                             tid1 = seasonDataDt$Wteam, 
                             tid2 = seasonDataDt$Lteam, 
                             result = resultVector)
  EloRatings <- elo(x = seasonDataDf, gamma = advantageVector)
  EloRatingsDf <- data.frame(EloRatings$ratings)
  EloRatingsDf <- EloRatingsDf[,c("Player", "Rating")]
  names(EloRatingsDf) <- c("team_id", "elo_reg_season_end")
  EloRatingsDf$Season <- season
  
  fideRatings <- fide(x = seasonDataDf, gamma = advantageVector)
  fideRatingsDf <- data.frame(fideRatings$ratings)
  fideRatingsDf <- fideRatingsDf[,c("Player", "Rating")]
  names(fideRatingsDf) <- c("team_id", "fide_reg_season_end")  
  fideRatingsDf$Season <- season
  
  glickoRatings <- glicko(x = seasonDataDf, gamma = advantageVector)
  glickoRatingsDf <- data.frame(glickoRatings$ratings)
  glickoRatingsDf <- glickoRatingsDf[,c("Player", "Rating")]
  names(glickoRatingsDf) <- c("team_id", "glicko_reg_season_end")  
  glickoRatingsDf$Season <- season
  
  all_loop_ratings <- merge(x = fideRatingsDf, y = EloRatingsDf)
  all_loop_ratings <- merge(x = all_loop_ratings, y = glickoRatingsDf)
  
  all_ratings <- rbind(all_ratings, all_loop_ratings)
}

all_ratings$ratings_mean <- (all_ratings$fide_reg_season_end + all_ratings$elo_reg_season_end + all_ratings$glicko_reg_season_end) / 3


# clean up environment
rm(elo_regular_season_compact_results, EloRatings, EloRatingsDf, resultVector, advantageVector, season, seasonDataDf, seasonDataDt, 
   fideRatings, fideRatingsDf, glickoRatings, glickoRatingsDf, all_loop_ratings)


# add in ratings vars to training data set
train <- merge(x = train, y = all_ratings, by.x=c("high_team_id", "Season"), by.y=c("team_id", "Season"), all.x=TRUE)
names(train)[length(train)-3] <- "high_id_fide_reg_season_end"
names(train)[length(train)-2] <- "high_id_elo_reg_season_end"
names(train)[length(train)-1] <- "high_id_glicko_reg_season_end"
names(train)[length(train)] <- "high_id_ratings_mean"

train <- merge(x = train, y = all_ratings, by.x=c("low_team_id", "Season"), by.y=c("team_id", "Season"), all.x=TRUE)
names(train)[length(train)-3] <- "low_id_fide_reg_season_end"
names(train)[length(train)-2] <- "low_id_elo_reg_season_end"
names(train)[length(train)-1] <- "low_id_glicko_reg_season_end"
names(train)[length(train)] <- "low_id_ratings_mean"


# create diff variables
train$elo_reg_season_diff <- train$high_id_elo_reg_season_end - train$low_id_elo_reg_season_end
train$fide_reg_season_diff <- train$high_id_fide_reg_season_end - train$low_id_fide_reg_season_end
train$glicko_reg_season_diff <- train$high_id_glicko_reg_season_end - train$low_id_glicko_reg_season_end
train$ratings_means_diff <- train$low_id_ratings_mean - train$high_id_ratings_mean





################################
# add in conference avg ratings
################################
all_ratings <- merge(x = all_ratings, y = conferences, by.x=c("team_id", "Season"), by.y=c("team_id", "season"))

all_conf_ratings_avgs <- aggregate(x = cbind(all_ratings$elo_reg_season_end, all_ratings$fide_reg_season_end, 
                                             all_ratings$glicko_reg_season_end, all_ratings$ratings_mean), 
                                   by = list(all_ratings$conference, all_ratings$Season), FUN = mean)

names(all_conf_ratings_avgs) <- c("conference", "season", "conf_avg_elo", "conf_avg_fide", "conf_avg_glicko", "conf_avg_ratings_mean")

# add into training data set
train <- merge(x = train, y = all_conf_ratings_avgs, by.x=c("high_id_conference", "Season"), by.y=c("conference", "season"), all.x=TRUE)
names(train)[length(train)-3] <- "high_id_conf_avg_elo"
names(train)[length(train)-2] <- "high_id_conf_avg_fide"
names(train)[length(train)-1] <- "high_id_conf_avg_glicko"
names(train)[length(train)] <- "high_id_conf_avg_ratings_mean"

train <- merge(x = train, y = all_conf_ratings_avgs, by.x=c("low_id_conference", "Season"), by.y=c("conference", "season"), all.x=TRUE)
names(train)[length(train)-3] <- "low_id_conf_avg_elo"
names(train)[length(train)-2] <- "low_id_conf_avg_fide"
names(train)[length(train)-1] <- "low_id_conf_avg_glicko"
names(train)[length(train)] <- "low_id_conf_avg_ratings_mean"


# create diff variables
train$conf_avg_elo_diff <- train$high_id_conf_avg_elo - train$low_id_conf_avg_elo
train$conf_avg_fide_diff <- train$high_id_conf_avg_fide - train$low_id_conf_avg_fide
train$conf_avg_glicko_diff <- train$high_id_conf_avg_glicko - train$low_id_conf_avg_glicko
train$conf_avg_ratings_mean_diff <- train$high_id_conf_avg_ratings_mean - train$low_id_conf_avg_ratings_mean




#rm(all_ratings, all_conf_ratings_avgs, conferences)



############################################################################################################














############################################################################################################
### Massey ordinals ###

# http://www.masseyratings.com/cb/compare.htm


library(reshape2)


# read in data
massey_ordinals <- read.csv("massey_ordinals_2003-2015.csv")

# create data frame of only last day
massey_lastday <- massey_ordinals[massey_ordinals$rating_day_num==133,]
massey_lastday$rating_day_num <- NULL

fullfields <- c("BOB", "MOR", "RTH", "SAG", "WOL")

#massey_complete_means is an average of fullfields

# reshape data
massey_last_reshaped <- dcast(data = massey_lastday, season + team ~ sys_name)
# create rowMeans variable of all fields, complete fields
massey_last_reshaped$all_means <- rowMeans(massey_last_reshaped, na.rm = TRUE)
massey_last_reshaped$complete_means <- rowMeans(massey_last_reshaped[,fullfields], na.rm = TRUE)


#View(massey_last_reshaped[massey_last_reshaped$season==2015,])
#massey_last_reshaped <- massey_last_reshaped[,colSums(is.na(massey_last_reshaped))<nrow(massey_last_reshaped)]


# identify massey ordinals to use for last 4 years
columns_to_use <- c()
for(column in names(massey_last_reshaped[3:length(massey_last_reshaped)])){
  if(sum(is.na(massey_last_reshaped[massey_last_reshaped$season >= 2012,column]))==0){
    columns_to_use <- c(columns_to_use, column)
  }
}

massey_last_reshaped$recent_means <- rowMeans(massey_last_reshaped[,columns_to_use], na.rm = TRUE)


# working
### compute principal components on massey recent means variables

# model_formula <- as.formula(paste("~", paste(columns_to_use, collapse= "+")))
# 
# massey_pca <- prcomp(model_formula, data=massey_last_reshaped[massey_last_reshaped$season >= 2012,])
# massey_comps <- data.frame(massey_pca$x)





# create data frame of only metrics to add by team by season
massey <- massey_last_reshaped[,c(c("season", "team", "all_means", "complete_means", "recent_means"), fullfields)]



# add in to train
train <- merge(x = train, y = massey, by.x=c("high_team_id", "Season"), by.y=c("team", "season"), all.x=TRUE)
names(train)[length(train)-7] <- "high_id_massey_all_means"
names(train)[length(train)-6] <- "high_id_massey_complete_means"
names(train)[length(train)-5] <- "high_id_massey_recent_means"
names(train)[length(train)-4] <- "high_id_massey_BOB"
names(train)[length(train)-3] <- "high_id_massey_MOR"
names(train)[length(train)-2] <- "high_id_massey_RTH"
names(train)[length(train)-1] <- "high_id_massey_SAG"
names(train)[length(train)] <- "high_id_massey_WOL"

train <- merge(x = train, y = massey, by.x=c("low_team_id", "Season"), by.y=c("team", "season"), all.x=TRUE)
names(train)[length(train)-7] <- "low_id_massey_all_means"
names(train)[length(train)-6] <- "low_id_massey_complete_means"
names(train)[length(train)-5] <- "low_id_massey_recent_means"
names(train)[length(train)-4] <- "low_id_massey_BOB"
names(train)[length(train)-3] <- "low_id_massey_MOR"
names(train)[length(train)-2] <- "low_id_massey_RTH"
names(train)[length(train)-1] <- "low_id_massey_SAG"
names(train)[length(train)] <- "low_id_massey_WOL"


# create diff variables
train$massey_all_means_diff <- train$high_id_massey_all_means - train$low_id_massey_all_means
#train$massey_all_means_diff_squared <- (train$high_id_massey_all_means - train$low_id_massey_all_means)^2

train$massey_complete_means_diff <- train$high_id_massey_complete_means - train$low_id_massey_complete_means
#train$massey_complete_means_diff_squared <- (train$high_id_massey_complete_means - train$low_id_massey_complete_means)^2

train$massey_recent_means_diff <- train$high_id_massey_recent_means - train$low_id_massey_recent_means


train$massey_BOB_diff <- train$high_id_massey_BOB - train$low_id_massey_BOB
train$massey_MOR_diff <- train$high_id_massey_MOR - train$low_id_massey_MOR
train$massey_RTH_diff <- train$high_id_massey_RTH - train$low_id_massey_RTH
train$massey_SAG_diff <- train$high_id_massey_SAG - train$low_id_massey_SAG
train$massey_WOL_diff <- train$high_id_massey_WOL - train$low_id_massey_WOL




#rm(massey_comps, column, columns_to_use, massey_pca, model_formula)

#rm(massey_lastday, massey_last_reshaped, massey_ordinals, massey, fullfields)

####################################################################################






















###########################################################################
# add pointspreads to train data set


pointspreads <- read.csv("ThePredictionTrackerPointspreads.csv")
train <- merge(x = train, y = pointspreads, by.x=c("Daynum", "Season", "Wteam"), by.y=c("daynum", "season", "wteam"), all.x=TRUE)
train <- train[,!names(train) %in% c("wscore", "line_wloc", "lteam", "lscore", "wloc", "numot")]

line_fields <- c("line", "lineavg", "linesag", "linesage", "linesagp", "linepom", "linemoore", "linepugh", "lineopen", "linedok", "linefox", "linepig")

# convert to being in perspective of low_team_id
for(field in line_fields){
  train[,field] <- ifelse(test = train$Lteam<train$Wteam, 
                                            yes = -1 * train[,field], 
                                            no = train[,field])
}


train$all_line_means <- rowMeans(train[,line_fields], na.rm = TRUE)


#rm(field, line_fields, pointspreads)


################################


# create model to convert line to probability
pointspreads <- read.csv("ThePredictionTrackerPointspreads.csv")
regular_season_compact_results <- read.csv("RegularSeasonCompactResults.csv")

reg_season_pointspreads <- merge(x = regular_season_compact_results, y = pointspreads, by.x=c("Season", "Daynum", "Wteam"), by.y=c("season", "daynum", "wteam"))

# remove records with null pointspreads
reg_season_pointspreads <- reg_season_pointspreads[!is.na(reg_season_pointspreads$line),]
# create outcome variable
reg_season_pointspreads$outcome <- ifelse(reg_season_pointspreads$Wteam<reg_season_pointspreads$Lteam,1,0)

line_fields <- c("line", "lineavg", "linesag", "linesage", "linesagp", "linepom", "linemoore", "linepugh", "lineopen", "linedok", "linefox", "linepig")

# convert to being in perspective of low_team_id
for(field in line_fields){
  reg_season_pointspreads[,field] <- ifelse(test = reg_season_pointspreads$Lteam<reg_season_pointspreads$Wteam, 
                                            yes = -1 * reg_season_pointspreads[,field], 
                                            no = reg_season_pointspreads[,field])
}


# remove records with line < -40 or > 40
reg_season_pointspreads <- reg_season_pointspreads[reg_season_pointspreads$line>-40,]
reg_season_pointspreads <- reg_season_pointspreads[reg_season_pointspreads$line<40,]

reg_season_pointspreads$all_line_means <- rowMeans(reg_season_pointspreads[,line_fields], na.rm=TRUE)




# create model to convert line to probability
line_to_prob_model <- glm(outcome ~ line,
                          data = reg_season_pointspreads,
                          family = binomial("logit"))

avg_line_to_prob_model <- glm(outcome ~ all_line_means,
                              data = reg_season_pointspreads,
                              family = binomial("logit"))


# add pointspread_predictions to train data set
train$line_preds <- predict(object = line_to_prob_model, newdata = train, type="response")
train$avg_line_preds <- predict(object = avg_line_to_prob_model, newdata = train, type="response")



# add model_lines var
train$model_lines <- ifelse(test = train$Daynum %in% c(136,137), 
                            yes = train$line, 
                            no = -9999)

train$model_lines <- ifelse(test = is.na(train$line),
                            yes = -9999,
                            no = train$line)



#rm(pointspreads, field, line_fields, regular_season_compact_results, reg_season_pointspreads)

###########################################################################















#### Add in Net Prophet prediction

net_prophet_sub <- read.csv("steal-submission-phase1.csv")
names(net_prophet_sub) <- c("Id", "net_prophet_pred")

# add in to train
train <- merge(x = train, y = net_prophet_sub, by.x=c("id"), by.y=c("Id"), all.x=TRUE)

#rm(net_prophet_sub)









# write out training data set
train <- train[order(train$id),]  # reorder train data set
#write.csv(train, "train.csv", row.names=FALSE)

#save.image(file="train_environment.Rdata")




































########################################################
# Next steps:
########################################################

# feature engineering
#  - add in adjusted kenpom ppp metrics
#  - add in conference metrics

# recreate elo to include pointspread weightings
# getAnywhere(elo)

# PCA?
















# PCA
##########################
### compute principal components on 2 line variables
line_pca <- prcomp(~line + lineavg, data=train, na.action = na.omit)
line_comps <- data.frame(line_pca$x)

# add lines first principal component to train
train <- merge(x = train, y = line_comps, by = "row.names", all.x=TRUE)
train$Row.names <- NULL
train$PC2 <- NULL
train <- train[order(train$id),]
names(train)[length(train)] <- "line_pc"
##########################












# build Rshiny.Rdata environment for shiny app
setwd("~/Desktop/kaggle/march_madness_2016")  # at work
train <- read.csv("train.csv")


feature.names <- c("win_pct_diff", "elo_reg_season_diff", 
                   "fide_reg_season_diff",
                   "glicko_reg_season_diff",
                   "massey_all_means_diff", 
                   "massey_complete_means_diff",
                   "massey_BOB_diff", "massey_MOR_diff",
                   "massey_RTH_diff", "massey_SAG_diff", 
                   "massey_WOL_diff",
                   "seed_diff",
                   "conf_avg_ratings_mean_diff",
                   "ratings_means_diff",
                   "low_id_massey_all_means", "high_id_massey_all_means",
                   "low_id_massey_complete_means", "high_id_massey_complete_means",
                   "low_id_ratings_mean", "high_id_ratings_mean"
)



train <- train[,c(feature.names, "outcome", "summary", "id", "Season")]




library(XML)
library(RCurl)

webpage <- "https://www.kaggle.com/c/march-machine-learning-mania-2015/leaderboard"

pagedata <- getURL(webpage)
tables <- readHTMLTable(pagedata, header = FALSE)
leaderboard <- tables$"leaderboard-table"
leaderboard <- leaderboard[2:nrow(leaderboard),c("V1", "V4")]
names(leaderboard) <- c("Place", "Logloss")
row.names(leaderboard) <- NULL

rm(pagedata, tables, webpage, feature.names)







