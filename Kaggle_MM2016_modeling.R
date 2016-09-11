

### Kaggle March Madness 2016 ###

### Author: Jeremy Mobley ###



# 0.438 won 2015
# 0.529 won 2014


library(xgboost)

# set up parallel back-end
library(doParallel)
getDoParWorkers()
registerDoParallel(cores=detectCores())
getDoParWorkers()



#LOAD DATA
setwd("~/Desktop/kaggle/march_madness_2016")  # at work
#train <- read.csv("train.csv")
load("train_environment.Rdata")





# utility variables
pretest_seasons <- seq(from = 2012, to = 2015)
all_seasons <- seq(from = 2003, to = 2015)






############################  MODELING  #############################

# logistic - outcome


# create list of input features
logistic_features <- c(#"win_pct_diff", 
                        #"elo_reg_season_diff", 
                        #"fide_reg_season_diff", 
                        #"glicko_reg_season_diff",
                        #"massey_all_means_diff", 
                        #"massey_complete_means_diff",
                        #"massey_BOB_diff", 
                       "massey_MOR_diff", 
                       #"massey_RTH_diff", 
                       #"massey_SAG_diff", 
                       #"massey_WOL_diff",
                        #"seed_diff", 
                        "ratings_means_diff"
                       )

# create formula variable
model_formula <- as.formula(paste("outcome ~ ", paste(logistic_features, collapse= "+")))


# build single model of all years
model1 <- glm(model_formula
              , data = train
              , family = binomial("logit"))

summary(model1)




# build models for each year, excluding season to predict

logistic_preds <- c()
loglosses <- c()
for (season in all_seasons){
  model1 <- glm(model_formula
                #, data = train[train$Season < season,]  # only train on years prior to season being predicted
                , data = train[train$Season != season,]
                , family = binomial("logit"))
  
  preds <- predict(model1, train[train$Season == season,], type = "response")
  logistic_preds <- c(logistic_preds, preds)
  answerkeyguy <- train[train$Season == season,"outcome"]
  loglossguy <- (answerkeyguy * log(preds)) + ((1-answerkeyguy)*(log(1-preds)))
  seasonlogloss <- -1/length(loglossguy) * sum(loglossguy)
  print(paste0(season, ": ", seasonlogloss))
  loglosses <- c(loglosses, seasonlogloss)
}
print(paste0("Average logloss: ", mean(loglosses)))

# add logistic_preds to train
train$logistic_preds <- logistic_preds











# ensemble of logistic regression models predicting outcome - NOT HELPFUL
"""
loglosses <- c()
for (season in all_seasons){
  model1 <- glm(outcome ~
                  + elo_reg_season_diff
                #  + massey_all_means_diff
                + massey_complete_means_diff
                #, data = train[train$Season < season,]  # only train on years prior to season being predicted
                , data = train[train$Season != season,]
                , family = binomial("logit"))
  
  preds1 <- predict(model1, train[train$Season == season,], type = "response")
  
  model2 <- glm(outcome ~
                  + elo_reg_season_diff
                  + massey_all_means_diff
                # + massey_complete_means_diff
                #, data = train[train$Season < season,]  # only train on years prior to season being predicted
                , data = train[train$Season != season,]
                , family = binomial("logit"))
  
  preds2 <- predict(model2, train[train$Season == season,], type = "response")
  
  preds <- (preds1 + preds2)/2  
  answerkeyguy <- train[train$Season == season,"outcome"]
  loglossguy <- (answerkeyguy * log(preds)) + ((1-answerkeyguy)*(log(1-preds)))
  seasonlogloss <- -1/length(loglossguy) * sum(loglossguy)
  print(paste0(season, ": ", seasonlogloss))
  loglosses <- c(loglosses, seasonlogloss)
}
print(paste0("Average logloss: ", mean(loglosses)))
"""


#################################################################################























#################################################################################

#### xgboost - outcome ####



feature.names <- c(
  #"round",
  # seed
  "low_id_team_seed", "high_id_team_seed", "seed_diff", 
  # win_pct, avg_margin
  "low_id_win_pct", "high_id_win_pct", "win_pct_diff", 
  "low_id_avg_margin", "high_id_avg_margin", "avg_margin_diff",          
  "low_id_avg_conf_raw_win_pct", "high_id_avg_conf_raw_win_pct", "avg_conf_raw_win_pct_diff", 
  "low_id_median_conf_raw_win_pct", "high_id_median_conf_raw_win_pct", "median_conf_raw_win_pct_diff", 
  # ppp metrics
  "low_id_raw_avg_off_ppp", "high_id_raw_avg_off_ppp",
  "low_id_raw_avg_def_ppp", "high_id_raw_avg_def_ppp", 
  # elo / fide/ glicko ratings
  "low_id_ratings_mean", "high_id_ratings_mean", "ratings_means_diff",
  "low_id_conf_avg_ratings_mean", "high_id_conf_avg_ratings_mean", "conf_avg_ratings_mean_diff",
  "low_id_elo_reg_season_end", "high_id_elo_reg_season_end", "elo_reg_season_diff", 
  "low_id_fide_reg_season_end", "high_id_fide_reg_season_end", "fide_reg_season_diff", 
  "low_id_glicko_reg_season_end", "high_id_glicko_reg_season_end", "glicko_reg_season_diff",
  # massey ordinals
  "low_id_massey_complete_means", "high_id_massey_complete_means", "massey_complete_means_diff",
  "low_id_massey_BOB", "high_id_massey_BOB", "massey_BOB_diff",   
  "low_id_massey_MOR", "high_id_massey_MOR", "massey_MOR_diff", 
  "low_id_massey_RTH", "high_id_massey_RTH", "massey_RTH_diff", 
  "low_id_massey_SAG", "high_id_massey_SAG", "massey_SAG_diff", 
  "low_id_massey_WOL", "high_id_massey_WOL", "massey_WOL_diff",
  "model_lines"
)


feature.names <- c(#"low_id_massey_complete_means", "high_id_massey_complete_means",
  "massey_complete_means_diff"
  , "model_lines" 
  , "conf_avg_ratings_mean_diff"
                   )



# create a single xgboost model
clf <- xgboost(data        = data.matrix(train[,feature.names]),
               label       = train$outcome,
               nrounds     = 26,
               eta         = 0.21,
               max_depth   = 2,
               objective   = "reg:logistic",
               eval_metric = "logloss", missing="NAN")

xgb.plot.importance(xgb.importance(feature_names = feature.names, model=clf))




# build models for each year, excluding season to predict

xgb_preds <- c()
loglosses <- c()
for (season in all_seasons){
  clf <- xgboost(data        = data.matrix(train[train$Season != season,feature.names]),
                 label       = train[train$Season != season,"outcome"],
                 nrounds     = 17,
                 eta         = 0.27,
                 max_depth   = 2,
                 objective   = "reg:logistic",
                 eval_metric = "logloss", verbose=FALSE, missing="NAN")
  
  preds <- predict(clf, data.matrix(train[train$Season == season,feature.names]), missing="NAN")
  xgb_preds <- c(xgb_preds, preds)
  answerkeyguy <- train[train$Season == season,"outcome"]
  loglossguy <- (answerkeyguy * log(preds)) + ((1-answerkeyguy)*(log(1-preds)))
  seasonlogloss <- -1/length(loglossguy) * sum(loglossguy)
  print(paste0(season, ": ", seasonlogloss))
  loglosses <- c(loglosses, seasonlogloss)
}
print(paste0("Average logloss: ", mean(loglosses)))


# add xgb_preds to train
train$xgb_preds_outcome <- xgb_preds








# optimize xgboost parameters

start_time <- Sys.time()
max_depth_list <- seq(from = 2, to = 7, by = 1)  # max tree depth
eta_list <- seq(from = 0.05, to = 0.35, by = 0.02)  # step size of boosting step
nround_list <- seq(from = 5, to = 40, by = 3)  # number of iterations
xgbresults <- expand.grid(max_depth=max_depth_list, eta=eta_list, nrounds=nround_list)

newresults <- foreach(k = seq(nrow(xgbresults)), .combine=rbind, .packages='xgboost') %dopar% {
  
  loglosses <- c()
  for (season in all_seasons){
    clf <- xgboost(data        = data.matrix(train[train$Season != season,feature.names]),
                   label       = train[train$Season != season,"outcome"],
                   nrounds     = xgbresults$nrounds[k],
                   eta         = xgbresults$eta[k],
                   max_depth   = xgbresults$max_depth[k],
                   objective   = "reg:logistic",
                   eval_metric = "logloss", verbose=FALSE, missing="NAN")
    
    preds <- predict(clf, data.matrix(train[train$Season == season,feature.names]), missing="NAN")
    
    answerkeyguy <- train[train$Season == season,"outcome"]
    loglossguy <- (answerkeyguy * log(preds)) + ((1-answerkeyguy)*(log(1-preds)))
    seasonlogloss <- -1/length(loglossguy) * sum(loglossguy)
    #print(paste0(season, ": ", seasonlogloss))
    loglosses <- c(loglosses, seasonlogloss)
  }
  #print(paste0("Average logloss: ", mean(loglosses)))
  mean(loglosses)
  
}

xgbresults$logloss <- newresults
xgbresults <- xgbresults[order(xgbresults$logloss),]
Sys.time() - start_time

View(xgbresults)























#################
# xgboost modeling point differential
#################

# create a single xgboost model to predict point differential, convert to probs
clf <- xgboost(data        = data.matrix(train[,feature.names]),
               label       = train$pointsdiff_outcome,
               nrounds     = 23,
               eta         = 0.15,
               max_depth   = 2,
               objective   = "reg:linear",
               eval_metric = "logloss", missing="NAN")

xgb.plot.importance(xgb.importance(feature_names = feature.names, model=clf))



# build point differential models for each year, excluding season to predict, convert to probs
xgb_preds <- c()
loglosses <- c()
for (season in all_seasons){
  clf <- xgboost(data        = data.matrix(train[train$Season != season,feature.names]),
                 label       = train[train$Season != season,"pointsdiff_outcome"],
                 nrounds     = 23,
                 eta         = 0.15,
                 max_depth   = 2,
                 objective   = "reg:linear",
                 eval_metric = "rmse", verbose=FALSE, missing="NAN")
  
  pointspread_preds <- predict(object = clf, newdata = data.matrix(train[train$Season == season,feature.names]), missing="NAN")
  
  preds <- predict(object = line_to_prob_model, newdata = data.frame(line=pointspread_preds), type="response")  
  xgb_preds <- c(xgb_preds, preds)
  
  answerkeyguy <- train[train$Season == season,"outcome"]
  loglossguy <- (answerkeyguy * log(preds)) + ((1-answerkeyguy)*(log(1-preds)))
  seasonlogloss <- -1/length(loglossguy) * sum(loglossguy)
  print(paste0(season, ": ", seasonlogloss))
  loglosses <- c(loglosses, seasonlogloss)
}
print(paste0("Average logloss: ", mean(loglosses)))


# add xgb_preds to train
train$xgb_preds_pointdiff <- xgb_preds




# optimize xgboost parameters

start_time <- Sys.time()
max_depth_list <- seq(from = 2, to = 7, by = 1)  # max tree depth
eta_list <- seq(from = 0.05, to = 0.35, by = 0.02)  # step size of boosting step
nround_list <- seq(from = 5, to = 40, by = 3)  # number of iterations
xgbresults <- expand.grid(max_depth=max_depth_list, eta=eta_list, nrounds=nround_list)

newresults <- foreach(k = seq(nrow(xgbresults)), .combine=rbind, .packages='xgboost') %dopar% {
  
  loglosses <- c()
  for (season in all_seasons){
    clf <- xgboost(data        = data.matrix(train[train$Season != season,feature.names]),
                   label       = train[train$Season != season,"pointsdiff_outcome"],
                   nrounds     = xgbresults$nrounds[k],
                   eta         = xgbresults$eta[k],
                   max_depth   = xgbresults$max_depth[k],
                   objective   = "reg:linear",
                   eval_metric = "rmse", verbose=FALSE, missing="NAN")
    
    pointspread_preds <- predict(object = clf, newdata = data.matrix(train[train$Season == season,feature.names]), missing="NAN")
    
    preds <- predict(object = line_to_prob_model, newdata = data.frame(line=pointspread_preds), type="response")  
    
    answerkeyguy <- train[train$Season == season,"outcome"]
    loglossguy <- (answerkeyguy * log(preds)) + ((1-answerkeyguy)*(log(1-preds)))
    seasonlogloss <- -1/length(loglossguy) * sum(loglossguy)
    loglosses <- c(loglosses, seasonlogloss)
  }
  mean(loglosses)
  
}

xgbresults$logloss <- newresults
xgbresults <- xgbresults[order(xgbresults$logloss),]
Sys.time() - start_time

View(xgbresults)



#####################################################################################















# DOES NOT WORK YET
######################################################################################################
##### Random Forest #######
'''
library(randomForest)

feature.names <- c("low_id_raw_avg_off_ppp", "low_id_raw_avg_def_ppp", "high_id_raw_avg_off_ppp", "high_id_raw_avg_def_ppp", "low_id_win_pct", "high_id_win_pct")

for (season in all_seasons){
# build model excluding season to predict  
rf <- randomForest(train[train$Season != season,feature.names],
y=as.factor(train[train$Season != season,outcome]),
ntree=10,
replace=TRUE, type="prob")

preds <- predict(rf, newdata = train[train$Season == season,feature.names], type="prob")

answerkeyguy <- train[train$Season != season,"outcome"]
loglossguy <- (answerkeyguy * log(model1$fitted.values)) + ((1-answerkeyguy)*(log(1-model1$fitted.values)))
seasonlogloss <- -1/length(loglossguy) * sum(loglossguy)
print(paste0(season, ": ", seasonlogloss))
}
'''
######################################################################################################














########################################################
# WORKING MODELING stuff


loglosses <- c()
for (season in all_seasons){
  model1 <- glm(model_formula
                #, data = train[train$Season < season,]  # only train on years prior to season being predicted
                , data = train[train$Season != season,]
                , family = binomial("logit"))
  
  clf <- xgboost(data        = data.matrix(train[train$Season != season,feature.names]),
                 label       = train[train$Season != season,"outcome"],
                 nrounds     = 11,
                 eta         = 0.25,
                 max_depth   = 2,
                 objective   = "reg:logistic",
                 eval_metric = "logloss", verbose=FALSE, missing="NAN")
  
  clf2 <- xgboost(data        = data.matrix(train[train$Season != season,feature.names]),
                 label       = train[train$Season != season,"pointsdiff_outcome"],
                 nrounds     = 23,
                 eta         = 0.15,
                 max_depth   = 2,
                 objective   = "reg:linear",
                 eval_metric = "rmse", verbose=FALSE, missing="NAN")
    
  loop_train <- train[train$Season == season,]
  loop_train$preds1 <- predict(clf, data.matrix(train[train$Season == season,feature.names]), missing="NAN")
  loop_train$preds2 <- predict(model1, train[train$Season == season,], type = "response")
  
  pointspread_preds <- predict(object = clf2, newdata = data.matrix(train[train$Season == season,feature.names]), missing="NAN")
  loop_train$preds3 <- predict(object = line_to_prob_model, newdata = data.frame(line=pointspread_preds), type="response")
  
  #loop_train$preds <- (loop_train$preds1 + loop_train$preds2 + loop_train$preds3) / 3
  loop_train$preds <- (loop_train$preds1 + loop_train$preds3) / 2
  
  loop_train$real_preds <- ifelse(test = loop_train$Daynum %in% c(136, 137) & !is.na(loop_train$line),
                                  yes = (loop_train$preds+loop_train$line_preds)/2,  # avg of my pred and line_preds
                                  #yes = loop_train$line_preds,
                                  no = loop_train$preds)

#   if (season %in% pretest_seasons){
#     loop_train$real_preds <- (loop_train$real_preds + loop_train$net_prophet_pred)/2
#   }

  
#   loop_train$real_preds <- ifelse(test = season %in% pretest_seasons,
#                                   yes = (loop_train$real_preds + loop_train$net_prophet_pred)/2,  # avg of my pred, line_preds, and netprophet preds
#                                   no = loop_train$real_preds)
    
  preds <- loop_train$real_preds
  
  answerkeyguy <- loop_train[loop_train$Season == season,"outcome"]
  loglossguy <- (answerkeyguy * log(preds)) + ((1-answerkeyguy)*(log(1-preds)))
  total_loglosses <- sum(loglossguy)
  seasonlogloss <- -1/length(loglossguy) * sum(loglossguy)
  print(paste0(season, ": ", seasonlogloss))
  loglosses <- c(loglosses, seasonlogloss)
  
}
print(paste0("Average logloss: ", format(mean(loglosses))))




########################################################


















########################################################

# ensemble all logistic and xgboost outcome and pointspread models together
prediction_summary <- data.frame()
loglosses <- c()
for (season in all_seasons){
  # build models excluding season to predict
  model1 <- glm(outcome ~ 
                  win_pct_diff 
                + elo_reg_season_diff
                + massey_complete_means_diff
                , data = train[train$Season != season,]
                , family = binomial("logit"))
  
  clf <- xgboost(data        = data.matrix(train[train$Season != season,feature.names]),
                 label       = train[train$Season != season,"outcome"],
                 nrounds     = 11,
                 eta         = 0.25,
                 max_depth   = 2,
                 objective   = "reg:logistic",
                 eval_metric = "logloss", verbose=FALSE, missing="NAN")
  
  
  preds1 <- predict(model1, train[train$Season == season,], type = "response")
  preds2 <- predict(clf, data.matrix(train[train$Season == season,feature.names]), missing="NAN")
  
  clf2 <- xgboost(data        = data.matrix(train[train$Season != season,feature.names]),
                 label       = train[train$Season != season,"pointsdiff_outcome"],
                 nrounds     = 23,
                 eta         = 0.15,
                 max_depth   = 2,
                 objective   = "reg:linear",
                 eval_metric = "rmse", verbose=FALSE, missing="NAN")
  
  pointspread_preds <- predict(object = clf2, newdata = data.matrix(train[train$Season == season,feature.names]), missing="NAN")
  preds3 <- predict(object = line_to_prob_model, newdata = data.frame(line=pointspread_preds), type="response") 
  
  preds <- rowMeans(cbind(preds1, preds2, preds3))
  #preds <- rowMeans(cbind(preds1, preds3))
  
  answerkeyguy <- train[train$Season == season,"outcome"]
  
  loop_train <- train[train$Season == season,]
  #gamesummary <- levels(train[train$Season == season,"summary"])[train[train$Season == season,"summary"]]
  
  loglossguy <- (answerkeyguy * log(preds)) + ((1-answerkeyguy)*(log(1-preds)))
  seasonlogloss <- -1/length(loglossguy) * sum(loglossguy)
  gamesummary <- data.frame(season=season, gamesummary=loop_train$summary, loglossguy=loglossguy, preds=preds, # linepreds=loop_train$line_preds,
                            #win_pct_diff=loop_train$win_pct_diff,
                            elo_reg_season_diff=loop_train$elo_reg_season_diff,
                            massey_complete_means_diff=loop_train$massey_complete_means_diff)
  
  print(paste0(season, ": ", seasonlogloss))
  loglosses <- c(loglosses, seasonlogloss)
  prediction_summary <- rbind(prediction_summary,gamesummary)
}
print(paste0("Average logloss: ", mean(loglosses)))
#print(paste0("Average pretest logloss: ", mean(tail(loglosses,4))))

prediction_summary <- prediction_summary[order(prediction_summary$loglossguy),]

########################################################











########################################################
# understand the effect of manually adjusting one prediction

train <- train[order(train$id),]  # reorder train data set

loglosses <- c()
for (season in all_seasons){
  clf <- xgboost(data        = data.matrix(train[train$Season < season,feature.names]),
                 label       = train[train$Season < season,"outcome"],
                 nrounds     = 11,
                 eta         = 0.25,
                 max_depth   = 2,
                 objective   = "reg:logistic",
                 eval_metric = "logloss", verbose=FALSE, missing="NAN")
  
  preds <- predict(clf, data.matrix(train[train$Season == season,feature.names]), missing="NAN")
  
  #if(season==2015){preds[8] <- 0.6}  # GA state over Baylor upset
  #if(season==2015){preds[62] <- 0.6}  # UCLA over SMU
  #if(season==2015){preds[35] <- 0.6}  # UAB over Iowa State
  #if(season==2015){preds[54] <- 0.6}  # ND over Northeastern
  #if(season==2015){preds[34] <- 0.6}  # Wichita St. over IU
    
  # create upper and lower boundaries for predictions
  #preds[preds < 0.05] <- 0.05
  #preds[preds > 0.95] <- 0.95
  
  answerkeyguy <- train[train$Season == season,"outcome"]
  loglossguy <- (answerkeyguy * log(preds)) + ((1-answerkeyguy)*(log(1-preds)))
  seasonlogloss <- -1/length(loglossguy) * sum(loglossguy)
  print(paste0(season, ": ", seasonlogloss))
  loglosses <- c(loglosses, seasonlogloss)
}
print(paste0("Average logloss: ", mean(loglosses)))


###########

train <- train[order(train$id),]
answerkeyguy <- train[train$Season == 2015,]
answerkeyguy[8,]
answerkeyguy[62,]
answerkeyguy[34,]


preds <- predict(clf, data.matrix(train[train$Season == 2015,feature.names]))
preds[8]
preds[62]
preds[45]

# no adjustment = 0.47757
# adjust to 0.5 = 0.47123
# adjust to 0.4 = 0.46833
# adjust to 0.3 = 0.46589
# adjust to 0.2 = 0.46377
# adjust to 0.1 = 0.46190
#adjust to 0.01 = 0.46038

########################################################




















################################################
###### STACKING ###### 
################################################





stacking_train <- train[train$Season>=2012,c("summary", "Season", "logistic_preds", "xgb_preds_outcome",  "xgb_preds_pointdiff", "net_prophet_pred",
                                             "elo_diff_pred", "line_preds", "outcome")]

#stacking_train$net_prophet_abs_diff <- abs(stacking_train$net_prophet_pred - stacking_train$outcome)
#stacking_train <- stacking_train[order(stacking_train$net_prophet_abs_diff),]


# create stacked model across all 4 pretest years
glm_stack <- glm(outcome ~ logistic_preds + 
                   xgb_preds_outcome + 
                   xgb_preds_pointdiff + 
                   net_prophet_pred
                 , data = stacking_train
                 ,family = binomial("logit"))
summary(glm_stack)


# create stacked models from out of season training set
glm_stacked_preds <- c()
loglosses <- c()
for (season in pretest_seasons){
  
  glm_stack <- glm(outcome ~ logistic_preds + 
                     #xgb_preds_outcome + 
                     xgb_preds_pointdiff + 
                     net_prophet_pred, 
                   data = stacking_train[stacking_train$Season != season,],
                   family = binomial("logit"))
  
  preds <- predict(glm_stack, stacking_train[stacking_train$Season == season,], type="response")
  
  glm_stacked_preds <- c(glm_stacked_preds, preds)
  answerkeyguy <- stacking_train[stacking_train$Season == season,"outcome"]
  loglossguy <- (answerkeyguy * log(preds)) + ((1-answerkeyguy)*(log(1-preds)))
  seasonlogloss <- -1/length(loglossguy) * sum(loglossguy)
  print(paste0(season, ": ", seasonlogloss))
  loglosses <- c(loglosses, seasonlogloss)
}
print(paste0("Average logloss: ", mean(loglosses, na.rm=TRUE)))

# add to stacking_train
stacking_train$glm_stacked_preds <- glm_stacked_preds










stacking_feature_names <- c("logistic_preds", "net_prophet_pred", "xgb_preds_pointdiff", "xgb_preds_outcome")


# create a stacked xgboost model
clf <- xgboost(data        = data.matrix(stacking_train[,stacking_feature_names]),
               label       = stacking_train$outcome,
               nrounds     = 23,
               eta         = 0.15,
               max_depth   = 2,
               objective   = "reg:linear",
               eval_metric = "logloss", missing="NAN")

xgb.plot.importance(xgb.importance(feature_names = stacking_feature_names, model=clf))




# build point differential models for each year, excluding season to predict, convert to probs
xgb_preds <- c()
loglosses <- c()
for (season in all_seasons){
  
  clf <- xgboost(data        = data.matrix(stacking_train[stacking_train$Season != season,stacking_feature_names]),
                 label       = stacking_train[stacking_train$Season != season, "outcome"],
                 nrounds     = 5,
                 eta         = 0.25,
                 max_depth   = 2,
                 objective   = "reg:linear",
                 eval_metric = "logloss", verbose=FALSE, missing="NAN")
  
    
  preds <- predict(object = clf, newdata = data.matrix(stacking_train[stacking_train$Season == season,stacking_feature_names]), missing="NAN")  
  xgb_preds <- c(xgb_preds, preds)
  
  answerkeyguy <- stacking_train[stacking_train$Season == season,"outcome"]
  loglossguy <- (answerkeyguy * log(preds)) + ((1-answerkeyguy)*(log(1-preds)))
  seasonlogloss <- -1/length(loglossguy) * sum(loglossguy)
  print(paste0(season, ": ", seasonlogloss))
  loglosses <- c(loglosses, seasonlogloss)
}
print(paste0("Average logloss: ", mean(loglosses, na.rm=TRUE)))

# add xgb_preds to stacking_train
stacking_train$xgb_stack_preds <- xgb_preds






# optimize stacke xgboost parameters

start_time <- Sys.time()
max_depth_list <- seq(from = 2, to = 7, by = 1)  # max tree depth
eta_list <- seq(from = 0.05, to = 0.35, by = 0.02)  # step size of boosting step
nround_list <- seq(from = 5, to = 40, by = 3)  # number of iterations
xgbresults <- expand.grid(max_depth=max_depth_list, eta=eta_list, nrounds=nround_list)

newresults <- foreach(k = seq(nrow(xgbresults)), .combine=rbind, .packages='xgboost') %dopar% {
  
  loglosses <- c()
  for (season in pretest_seasons){
    clf <- xgboost(data        = data.matrix(stacking_train[stacking_train$Season != season,stacking_feature_names]),
                   label       = stacking_train[stacking_train$Season != season,"outcome"],
                   nrounds     = xgbresults$nrounds[k],
                   eta         = xgbresults$eta[k],
                   max_depth   = xgbresults$max_depth[k],
                   objective   = "reg:linear",
                   eval_metric = "rmse", verbose=FALSE, missing="NAN")
    
    preds <- predict(object = clf, newdata = data.matrix(stacking_train[stacking_train$Season == season,stacking_feature_names]), missing="NAN")  
    
    answerkeyguy <- stacking_train[stacking_train$Season == season,"outcome"]
    loglossguy <- (answerkeyguy * log(preds)) + ((1-answerkeyguy)*(log(1-preds)))
    seasonlogloss <- -1/length(loglossguy) * sum(loglossguy)
    loglosses <- c(loglosses, seasonlogloss)
  }
  mean(loglosses)
  
}

xgbresults$logloss <- newresults
xgbresults <- xgbresults[order(xgbresults$logloss),]
Sys.time() - start_time

View(xgbresults)









# evaluating the predictions
loglosses <- c()
for (season in all_seasons){
  answerkeyguy <- train[train$Season == season,"outcome"]
  #preds <- rowMeans(train[train$Season == season,c("logistic_preds", "xgb_preds_outcome", "net_prophet_pred")], na.rm = TRUE)
  preds <- (train[train$Season == season,"xgb_preds_pointdiff"] * 0.1) + (train[train$Season == season,"net_prophet_pred"] * 0.9)
    
  #preds <- train[train$Season == season,"net_prophet_pred"]  # only net_prophet_pred 0.507 average on pretest
  loglossguy <- (answerkeyguy * log(preds)) + ((1-answerkeyguy)*(log(1-preds)))
  seasonlogloss <- -1/length(loglossguy) * sum(loglossguy)
  print(paste0(season, ": ", seasonlogloss))
  loglosses <- c(loglosses, seasonlogloss)
}
print(paste0("Average logloss: ", mean(loglosses, na.rm=TRUE)))
print(paste0("Average pretest logloss: ", mean(tail(loglosses,4))))























########################################################
# Next steps:
########################################################

# build better models

# build shiny app for prediction summary
# - similarities among games, matchups, teams

# get random forests working












