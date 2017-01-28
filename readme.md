# College Basketball Modeling ##

## Objective: Predict winner of NCAA men's college basketball games

This code repo will aid in the building of models to predict the outcome of NCAA men's college basketball games.

The short-term goals for this project are:

1. Better understand how accurately college basketball games can be predicted
2. Better understand which team aggregate stats are the most important predictors
3. Prepare a model for the upcoming tournament

There may be plans in the future to extend this modeling work beyond team aggregate stats to include player-level game data, recruiting data, coaches, venue, distance traveled, and other extenuating factors.


## Evaluation Metric:
Minimize the Log loss

The goal is to build a model to predict the probability that Team A will win against Team B.  

How do we know how close we are to predicting truth?

| Pred | Logloss |
| ---- |:-----:|
| 0.01 | 4.61  | 
| 0.05 | 3.00  | 
| 0.10 | 2.30  | 
| 0.50 | 0.69  | 
| 0.90 | 0.11  | 
| 0.95 | 0.05  | 
| 0.99 | 0.01  | 



If Team A is predicted to win with 1% probability but does pull off the upset the logloss value is 4.61.  

The metric is symmetric.

The upper bound for any model is an average log loss of 0.69.  If we knew nothing about any of the teams and simply predicted every game as a coin flip the model would score a 0.69.

https://www.kaggle.com/c/march-machine-learning-mania-2016/details/evaluation

### Kaggle Competition:
Kaggle hosts an annual competition for the NCAA tournament:

https://www.kaggle.com/c/march-machine-learning-mania-2016

### Data
Modeling work is based on aggregate team game data provided through the kaggle competition.

## How this repo is structured:
1. Raw data is stored in the /data folder
2. The create_train.R script creates the training data set
3. The modeling.R script builds models to predict game outcomes 
	- Includes code to evaluate those models

All R code is stored in the /R folder.

All modeling output is stored in the /output folder.


## Next steps:
* README
	* Finish explanation of evaluation metric
* Training data
	* Build better features!
		* Include PPP metrics
		* Calculate adjusted PPP metrics
* Modeling
	* Set up model evaluation framework
	* Build out knn modeling
	* Build pointspread predictions models
* Expand modeling beyond team aggregate data
	* Individual player level data
		* Where to get this data?
	* Pointspread data
	* Recruiting data
	* Coach
	* Venue / distance traveled