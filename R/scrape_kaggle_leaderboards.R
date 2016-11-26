
# Script to scrape the past three years leaderboard results from the March Madness Kaggle competitions
#   - write them out to a csv for later use

# Author: Jeremy Mobley
# 2016


library(XML)
library(RCurl)


# create dataframe of webpages of years to loop through to scrape
webpage_2016 <- "https://www.kaggle.com/c/march-machine-learning-mania-2016/leaderboard"
webpage_2015 <- "https://www.kaggle.com/c/march-machine-learning-mania-2015/leaderboard"
webpage_2014 <- "https://www.kaggle.com/c/march-machine-learning-mania/leaderboard"
webpages <- c(webpage_2016, webpage_2015, webpage_2014)
years <- c(2016, 2015, 2014)
webpages <- data.frame(webpage = webpages, years = years)


# loop through webpages and scrape leaderboard into a dataframe
kaggle_results_df <- data.frame(score=NULL, year=NULL, rank=NULL)
for (i in 1:nrow(webpages)){
  webpage <- webpages$webpage[i]
  year <- webpages$year[i]
  pagedata <- getURL(webpage)
  tables <- readHTMLTable(pagedata, header = FALSE)
  leaderboard_df <- data.frame(score=tables$"leaderboard-table"$V4[2:nrow(tables$"leaderboard-table")])
  leaderboard_df$score <- as.numeric(as.character(leaderboard_df$score))
  leaderboard_df$year <- year
  leaderboard_df$rank <- rank(leaderboard_df$score)
  kaggle_results_df <- rbind(kaggle_results_df, leaderboard_df)
}


# write out kaggle results as csv file to data in github repo
write.csv(kaggle_results_df, "~/Github/cbb_data/data/kaggle_results_df.csv")
