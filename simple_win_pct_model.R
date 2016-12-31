

#################################################
### LOAD DATA ###
#################################################
setwd("C:/Users/Jeremy/Documents/GitHub/cbb_data/data")
tourney_compact_results <- read.csv("TourneyCompactResults.csv")
regular_season_compact_results <- read.csv("RegularSeasonCompactResults.csv")


source("C:/Users/Jeremy/Documents/GitHub/cbb_data/R/create_train.R")

### LOAD TRAINING DATA ###
train <- create_train(tourney_compact_results, regular_season_compact_results)

# View Training data
View(train)


### MODELING ###

# create years to evaluate list
test_years <- seq(from = 2003, to = 2015)

# Create logistic model based on win_pct_diff to predict outcome
# set model formula
model_formula <- as.formula("outcome ~ win_pct_diff -1")

loglosses <- c()
for (season in test_years){
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


results_df <- data.frame(test_years=test_years, avg_log_loss=loglosses)
plot(results_df, type = 'line', ylim = c(0.4,0.7))



# Look at model predictions by win pct
model1
predict.glm(model1, newdata = data.frame(win_pct_diff=0.3))
1/(1+exp(-1*5.133*0.05))
plot(1/(1+exp(-1*5.133*seq(0,1, by = 0.02))), ylab = "pct predicted winning", main="Plot of logistic model win_pct_diff to probability of winning")






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

source("C:/Users/Jeremy/Documents/GitHub/cbb_data/R/util_funs.R")



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
seed_benchmark_results_df <- data.frame(test_years=test_years, avg_log_loss=loglosses)

# add seed benchmark line to results plot
lines(seed_benchmark_results_df, col='red')




# read in kaggle results
kaggle_results_df <- read.csv("~/Github/cbb_data/data/kaggle_results_df.csv")
kaggle_results_plot_df <- kaggle_results_df[kaggle_results_df$rank==1,c("year", "score")]
names(kaggle_results_plot_df) <- c("test_years", "avg_log_loss")

# add kaggle winners to results plot
lines(kaggle_results_plot_df, col='green')

# add in avg top 10 kaggle results to plot
kaggle_top10_results_df <- kaggle_results_df[kaggle_results_df$rank<11,c("year", "score")]
kaggle_avg_top10_results_df <- aggregate(x = kaggle_top10_results_df$score, 
          by = list(kaggle_top10_results_df$year), FUN = mean)
names(kaggle_avg_top10_results_df) <- c("test_years", "avg_log_loss")
lines(kaggle_avg_top10_results_df, col='blue')


# add legend to plot
legend("bottomleft", legend = c("model", "seed_benchmark", "kaggle_avg_top10", "kaggle_winners"), 
       fill=c("black", "red", "blue", "green"))








##### kNN modeling ####

library(FNN)
knnmodel <- knn(train = train[2:nrow(train),c("high_id_win_pct", "low_id_win_pct")], 
                cl = train$outcome[2:nrow(train)],
                test = train[1,c("high_id_win_pct", "low_id_win_pct")], k= 5, prob=T)

indices <- attr(knnmodel, "nn.index")
indices

train[2:nrow(train),][indices,c("high_id_win_pct", "low_id_win_pct")]
train[1,c("high_id_win_pct", "low_id_win_pct")]





