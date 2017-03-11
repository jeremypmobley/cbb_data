  

#################################################
### LOAD TRAINING DATA ###
#################################################
source("~/GitHub/cbb_data/R/util_funs.R")
setwd("C:/Users/Jeremy/Documents/GitHub/cbb_data/data")
train <- read.csv("~/GitHub/cbb_data/data/train_adv.csv")
#################################################


library(ggplot2)




### MODELING ###

# create years to evaluate list
test_years <- seq(from = 2003, to = 2016)

# Create logistic model based on win_pct_diff to predict outcome
# set model formula

features_to_include <- list("elo_diff")
model_formula <- as.formula(paste0("outcome ~ ", paste(features_to_include, collapse=" + ")))
loglosses <- c()
for (season in test_years){
  model1 <- glm(model_formula, 
                data = train[train$Season < season & train$Daynum > 100,],  # only train on years prior to season being predicted
                family = binomial("logit"))
  
  preds <- predict(model1, train[train$Season == season & train$tourney_gm == 1,], type = "response")  # only predict tourney games
  answerkeyguy <- train[train$Season == season & train$tourney_gm == 1,"outcome"]  # only evaluate tourney games
  seasonlogloss <- calc_logloss(preds, answerkeyguy)
  print(paste0(season, ": ", seasonlogloss))
  loglosses <- c(loglosses, seasonlogloss)
}
print(paste0("Average logloss: ", mean(loglosses)))


# create results df for plot
results_df <- data.frame(test_years=as.factor(test_years), avg_log_loss=loglosses)

# plot of model evaluation
ggplot(results_df, aes(x=test_years, y = avg_log_loss, group=1)) + 
  geom_line() +
  geom_hline(yintercept=0.69, col='red') +
  labs(title="Average tournament log loss",x="Year", y = "Log Loss")


# read in kaggle results
kaggle_results_df <- read.csv("~/Github/cbb_data/data/kaggle_results_df.csv")
kaggle_results_winners_df <- kaggle_results_df[kaggle_results_df$rank==1,c("year", "score")]
names(kaggle_results_winners_df) <- c("test_years", "avg_log_loss")
kaggle_results_winners_df$test_years <- as.factor(kaggle_results_winners_df$test_years)


# plot of model evaluation with kaggle winners in blue
ggplot(results_df, aes(x=test_years, y = avg_log_loss, group=1)) + 
  geom_line() +
  geom_hline(yintercept=0.69, col='red') +
  geom_line(data = kaggle_results_winners_df, aes(x=test_years, y = avg_log_loss), col = "blue") +
  labs(title="Average tournament log loss",x="Year", y = "Log Loss")








# Function to evaluate model performance
eval_model <- function(model, test_years){
  for (season in test_years){
    preds <- predict(model, train[train$Season == season & train$tourney_gm == 1,], type = "response")
    answerkeyguy <- train[train$Season == season,"outcome"]
    seasonlogloss <- calc_logloss(preds, answerkeyguy)
    print(paste0(season, ": ", seasonlogloss))
  }
}





#################################################
# plot out results using ggvis with hover over
#################################################

library(ggvis)
results_df %>% 
  ggvis(~test_years, ~avg_log_loss) %>%
  layer_lines() %>%
  layer_points()


# Function for the tooltip
getData <- function(dat){
  paste(paste("Year:", as.character(dat$test_years)),
        paste("Avg Log Loss:", dat$avg_log_loss),
        sep = "<br />")
}

results_df %>% 
  ggvis(~test_years, ~avg_log_loss) %>%
  layer_lines() %>%
  layer_points() %>%
  add_tooltip(getData)

#################################################



########################
# Create seed benchmark df logloss results
# 0.5 + (seed diff * .03)
########################

# load in tourney seeds lookup
tourney_seeds <- read.csv("TourneySeeds.csv")


# add in tourney seeds
train <- merge(x = train, y = tourney_seeds, by.x = c('low_team_id', "Season"), by.y = c('Team', "Season"))
names(train)[length(names(train))] <- "low_id_team_seed"

train <- merge(x = train, y = tourney_seeds, by.x = c('high_team_id', "Season"), by.y = c('Team', "Season"))
names(train)[length(names(train))] <- "high_id_team_seed"


train$low_id_team_seed <- clean_seed(train$low_id_team_seed)
train$high_id_team_seed <- clean_seed(train$high_id_team_seed)

# create seed_diff var
train$seed_diff <- train$low_id_team_seed - train$high_id_team_seed

# seed benchmark pred
train$seed_benchmark_pred <- 0.5 - (train$seed_diff * 0.03)

seed_benchmark_loglosses <- c()
for (season in test_years){
  preds <- train[train$Season == season,"seed_benchmark_pred"]
  answerkeyguy <- train[train$Season == season,"outcome"]
  loglossguy <- (answerkeyguy * log(preds)) + ((1-answerkeyguy)*(log(1-preds)))
  seasonlogloss <- -1/length(loglossguy) * sum(loglossguy)
  print(paste0(season, ": ", seasonlogloss))
  seed_benchmark_loglosses <- c(seed_benchmark_loglosses, seasonlogloss)
}
print(paste0("Average logloss: ", mean(seed_benchmark_loglosses)))

# create seed benchmark df
seed_benchmark_results_df <- data.frame(test_years=test_years, avg_log_loss=seed_benchmark_loglosses)
seed_benchmark_results_df$test_years <- as.factor(seed_benchmark_results_df$test_years)



# plot of model evaluation with kaggle winners and seed benchmark
ggplot(results_df, aes(x=test_years, y = avg_log_loss, group=1)) + 
  geom_line() +
  geom_hline(yintercept=0.69, col='red') +
  geom_line(data = kaggle_results_winners_df, aes(x=test_years, y = avg_log_loss), col = "blue") +
  geom_line(data = seed_benchmark_results_df, aes(x=test_years, y = avg_log_loss), col = "green") +
  labs(title="Average tournament log loss",x="Year", y = "Log Loss")







# add in avg top 10 kaggle results to plot
kaggle_top10_results_df <- kaggle_results_df[kaggle_results_df$rank<11,c("year", "score")]
kaggle_avg_top10_results_df <- aggregate(x = kaggle_top10_results_df$score, 
          by = list(kaggle_top10_results_df$year), FUN = mean)
names(kaggle_avg_top10_results_df) <- c("test_years", "avg_log_loss")
kaggle_avg_top10_results_df$test_years <- as.factor(kaggle_avg_top10_results_df$test_years)



# plot of model evaluation with kaggle winners, seed benchmark, kaggle top 10
ggplot(results_df, aes(x=test_years, y = avg_log_loss, group=1)) + 
  geom_line() +
  geom_hline(yintercept=0.69, col='red') +
  geom_line(data = kaggle_results_winners_df, aes(x=test_years, y = avg_log_loss), col = "blue") +
  geom_line(data = kaggle_avg_top10_results_df, aes(x=test_years, y = avg_log_loss), col = "light blue") +
  labs(title="Average tournament log loss",x="Year", y = "Log Loss")







##### looking into log loss
calc_logloss(preds = 0.99, actuals = 1)
logloss_df <- data.frame(preds=seq(from = 0.01, to = 1.00, by = 0.01), logloss=0)
logloss_df$logloss <- calc_logloss(preds = logloss_df$preds[2], actuals = 1)
for(i in 1:nrow(logloss_df)){
  logloss_df$logloss[i] <- calc_logloss(preds = logloss_df$preds[i], actuals = 1)
}
plot(logloss_df, xlab = "Prediction", ylab="Log loss", main="Log loss by Prediction", type='l')
###################################








##### kNN modeling ####

library(FNN)
knnmodel <- knn(train = train[2:nrow(train),c("high_id_elo", "low_id_elo")], 
                cl = train$outcome[2:nrow(train)],
                test = train[1,c("high_id_elo", "low_id_elo")], k= 8, prob=TRUE)

attr(knnmodel, "nn.index")
attr(knnmodel, "prob")

train[2:nrow(train),][attr(knnmodel, "nn.index"),c("high_id_elo", "low_id_elo", "outcome")]
train[1,c("high_id_elo", "low_id_elo")]



##################################################


knnmodel <- knn(train = train[train$tourney_gm == 0, c("high_id_elo", "low_id_elo")], 
                cl = train$outcome[train$tourney_gm == 0],
                test = train[train$tourney_gm == 1, c("high_id_elo", "low_id_elo")], 
                k = 15, prob=TRUE)

preds <- attr(knnmodel, "prob")
head(preds)
preds <- preds - 0.00001

answerkeyguy <- train[train$Season == season & train$tourney_gm == 1,"outcome"]  # only evaluate tourney games
calc_logloss(preds, answerkeyguy)




