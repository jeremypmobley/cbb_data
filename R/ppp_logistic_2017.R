
# Author: Jeremy Mobley
# Feb 2017

# logistic regression 
# using team's raw regular season average points per possession as features
# trained on past tournament games


library(data.table)

setwd("C:/Users/Jeremy/Documents/GitHub/cbb_data/data/2017/")  # this works locally, needs to be updated for kernel


######################################################
# Step 1: calculate each team's raw average points per possession for each regular season
  # need both offensive and defensive ppp
######################################################

regular_season_detailed_results <- fread("RegularSeasonDetailedResults.csv")

# add possession fields
regular_season_detailed_results$wposs <- regular_season_detailed_results$Wfga - regular_season_detailed_results$Wor + regular_season_detailed_results$Wto + 0.475*regular_season_detailed_results$Wfta
regular_season_detailed_results$lposs <- regular_season_detailed_results$Lfga - regular_season_detailed_results$Lor + regular_season_detailed_results$Lto + 0.475*regular_season_detailed_results$Lfta
regular_season_detailed_results$avgposs <- (regular_season_detailed_results$wposs + regular_season_detailed_results$lposs)/2
# calc points per possession (ppp)
regular_season_detailed_results$wppp <- regular_season_detailed_results$Wscore / regular_season_detailed_results$avgposs
regular_season_detailed_results$lppp <- regular_season_detailed_results$Lscore / regular_season_detailed_results$avgposs


# create regular season average ppp for each team
win_ppp <- regular_season_detailed_results[,.(avg_ppp_off_win = mean(wppp), 
                                              avg_ppp_def_win = mean(lppp), 
                                              win_count=.N), 
                                           by=.(Wteam,Season)]

loss_ppp <- regular_season_detailed_results[,.(avg_ppp_def_loss = mean(wppp), 
                                              avg_ppp_off_loss = mean(lppp), 
                                              loss_count=.N), 
                                           by=.(Lteam,Season)]

all_ppp <- merge(x=win_ppp, y=loss_ppp, by.x = c("Wteam", "Season"), by.y = c("Lteam", "Season"), all=TRUE)

all_ppp[is.na(all_ppp)] <- 0  # replace all missing values with 0 (for winless and undefeated regular season teams)

all_ppp$avg_ppp_off <- ((all_ppp$avg_ppp_off_win * all_ppp$win_count) + (all_ppp$avg_ppp_off_loss * all_ppp$loss_count)) / (all_ppp$win_count + all_ppp$loss_count)
all_ppp$avg_ppp_def <- ((all_ppp$avg_ppp_def_win * all_ppp$win_count) + (all_ppp$avg_ppp_def_loss * all_ppp$loss_count)) / (all_ppp$win_count + all_ppp$loss_count)

######################################################



######################################################
# Step 1A: calculate regular season end elo rating for each team for each season to use as features
######################################################

library("PlayerRatings")


elo_regular_season_compact_results <- fread("RegularSeasonCompactResults.csv")

elo_ratings <- data.frame()
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
  
  elo_ratings <- rbind(elo_ratings, EloRatingsDf)
}

######################################################

######################################################
# Step 1C: Adding massey ordinals
######################################################

massey_ordinals <- fread("massey_ordinals_2003-2016.csv")
sagarin_eoy <- massey_ordinals[massey_ordinals$sys_name=="SAG" & massey_ordinals$rating_day_num==133,]




######################################################
# Step 2: Create training data set of past tournament games
######################################################
tourney_compact_results <- fread("TourneyCompactResults.csv")
tourney_compact_results$outcome <- ifelse(tourney_compact_results$Wteam<tourney_compact_results$Lteam,1,0)  # outcome 1 if lower id team won

# add fields for low/high team id
tourney_compact_results$low_team_id <- ifelse(test = tourney_compact_results$Wteam<tourney_compact_results$Lteam,                             
                            yes = tourney_compact_results$Wteam,
                            no = tourney_compact_results$Lteam)

tourney_compact_results$high_team_id <- ifelse(test = tourney_compact_results$Wteam<tourney_compact_results$Lteam, 
                             yes = tourney_compact_results$Lteam,
                             no = tourney_compact_results$Wteam)

# add game id field to tourney_compact_results dataframe
tourney_compact_results$id <- ifelse(test = tourney_compact_results$Wteam<tourney_compact_results$Lteam, 
                   yes = paste0(tourney_compact_results$Season,'_',tourney_compact_results$Wteam,'_',tourney_compact_results$Lteam),
                   no = paste0(tourney_compact_results$Season,'_',tourney_compact_results$Lteam,'_',tourney_compact_results$Wteam))


# data table merge tourney compact results with all_ppp to create train data set
setkey(tourney_compact_results,"Season", "low_team_id")
setkey(all_ppp,"Season", "Wteam")
train <- tourney_compact_results[all_ppp[ , c("Season", "Wteam", "avg_ppp_def", "avg_ppp_off"), with=FALSE], nomatch=0]
colnames(train)[ncol(train)] <- "low_id_avg_ppp_def"
colnames(train)[ncol(train)-1] <- "low_id_avg_ppp_off"

setkey(train,"Season", "high_team_id")
setkey(all_ppp,"Season", "Wteam")
train <- train[all_ppp[ , c("Season", "Wteam", "avg_ppp_def", "avg_ppp_off"), with=FALSE], nomatch=0]
colnames(train)[ncol(train)] <- "high_id_avg_ppp_def"
colnames(train)[ncol(train)-1] <- "high_id_avg_ppp_off"

# add diff fields
train$low_id_off_diff <- train$low_id_avg_ppp_off - train$high_id_avg_ppp_def
train$low_id_def_diff <- train$high_id_avg_ppp_off - train$low_id_avg_ppp_def



# add in elo ratings
train <- merge(x=train, y=elo_ratings, by.x=c("Season", "low_team_id"), by.y=c("Season", "team_id"), all.x=TRUE)
colnames(train)[ncol(train)] <- "low_id_elo"
train <- merge(x=train, y=elo_ratings, by.x=c("Season", "high_team_id"), by.y=c("Season", "team_id"), all.x=TRUE)
colnames(train)[ncol(train)] <- "high_id_elo"

train$elo_diff <- train$low_id_elo - train$high_id_elo


# add in sagarin_eoy field
train <- merge(x=train, y=sagarin_eoy, 
               by.x=c("Season", "low_team_id"), by.y=c("season", "team"), all.x=TRUE)
colnames(train)[ncol(train)] <- "low_id_sag"
train$sys_name <- NULL
train$rating_day_num <- NULL
train <- merge(x=train, y=sagarin_eoy, 
               by.x=c("Season", "high_team_id"), by.y=c("season", "team"), all.x=TRUE)
colnames(train)[ncol(train)] <- "high_id_sag"
train$sys_name <- NULL
train$rating_day_num <- NULL

train$sag_diff <- train$low_id_sag - train$high_id_sag




######################################################


######################################################
# Step 3: Build logistic regression model
######################################################

# function to calculate the logloss given preds, actuals
calc_logloss <- function(preds, actuals){
  loglossguy <- (actuals * log(preds)) + ((1-actuals)*(log(1-preds)))
  return(-1/length(loglossguy) * sum(loglossguy))
}

test_years <- seq(from = 2013, to = 2016)


#features_to_include <- list("low_id_avg_ppp_def", "low_id_avg_ppp_off", "high_id_avg_ppp_def", "high_id_avg_ppp_off")
#features_to_include <- list("elo_diff")
#features_to_include <- list("low_id_avg_ppp_def", "low_id_avg_ppp_off", "high_id_avg_ppp_def", "high_id_avg_ppp_off", "elo_diff")
#features_to_include <- list("sag_diff")
features_to_include <- list("elo_diff", "sag_diff")

model_formula <- as.formula(paste0("outcome ~ ", paste(features_to_include, collapse=" + ")))
loglosses <- c()
allpreds <- data.frame()
for (season in test_years){
  setkey(train,"Season", "Daynum", "Wteam")
  model1 <- glm(model_formula, 
                data = train[train$Season < season,],  # only train on years prior to season being predicted
                family = binomial("logit"))
  preds <- predict(model1, train[train$Season == season & train$Daynum > 135,], type = "response")
  answerkeyguy <- train[train$Season == season & train$Daynum > 135,"outcome", with=FALSE]  # only evaluate tourney games
  preds_df <- data.frame(id = train[train$Season == season & train$Daynum > 135,"id", with=FALSE], pred = preds)
  allpreds <- rbind(allpreds, preds_df)
  pred_evaluator <- data.frame(preds = preds, outcome=answerkeyguy)
  seasonlogloss <- calc_logloss(pred_evaluator$preds, pred_evaluator$outcome)
  print(paste0(season, ": ", seasonlogloss))
  loglosses <- c(loglosses, seasonlogloss)
}

mean(loglosses)



# create submission file from predictions
sample_submission <- read.csv("sample_submission.csv")
sample_submission$pred <- NULL
sample_submission_preds <- merge(x=sample_submission, y=allpreds, by.x=c("id"), by.y=c("id"), all.x=TRUE)
sample_submission_preds[is.na(sample_submission_preds)] <- 0

# write out prediction file
setwd("C:/Users/Jeremy/Documents/GitHub/cbb_data/data/2017/submissions")
write.csv(sample_submission_preds, "first_test_sub_7Feb17.csv",row.names=FALSE)




# create results df for plot
results_df <- data.frame(test_years=as.factor(test_years), avg_log_loss=loglosses)



