

#################################################
### LOAD DATA ###
#################################################
setwd("C:/Users/Jeremy/Desktop/Kaggle/kaggle_march_madness/march-machine-learning-mania-2016-v2/march-machine-learning-mania-2016-v2")
tourney_compact_results <- read.csv("TourneyCompactResults.csv")
regular_season_compact_results <- read.csv("RegularSeasonCompactResults.csv")

#################################################
# create training data set
#################################################

train <- tourney_compact_results
#train$Wloc <- NULL  # remove unnecessary field
#train <- train[train$Season>=2003,]
# exclude play-in games from train
train <- train[train$Daynum!=134,]
train <- train[train$Daynum!=135,]

train$id <- ifelse(test = train$Wteam<train$Lteam, 
                   yes = paste0(train$Season,'_',train$Wteam,'_',train$Lteam),
                   no = paste0(train$Season,'_',train$Lteam,'_',train$Wteam))

# outcome is 1 if lower team id won
train$outcome <- ifelse(train$Wteam<train$Lteam,1,0)

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

# create diff variable
train$win_pct_diff <- train$low_id_win_pct - train$high_id_win_pct

###################################

### MODELING ###



pretest_seasons <- seq(from = 2012, to = 2015)
all_seasons <- seq(from = 2003, to = 2015)


# Create logistic model to predict outcome

model_formula <- as.formula("outcome ~ win_pct_diff")

loglosses <- c()
for (season in all_seasons){
  model1 <- glm(model_formula, 
                data = train[train$Season < season,],  # only train on years prior to season being predicted
                family = binomial("logit"))
  
  preds <- predict(model1, train[train$Season == season,], type = "response")
  answerkeyguy <- train[train$Season == season,"outcome"]
  loglossguy <- (answerkeyguy * log(preds)) + ((1-answerkeyguy)*(log(1-preds)))
  seasonlogloss <- -1/length(loglossguy) * sum(loglossguy)
  print(paste0(season, ": ", seasonlogloss))
  loglosses <- c(loglosses, seasonlogloss)
}
print(paste0("Average logloss: ", mean(loglosses)))











