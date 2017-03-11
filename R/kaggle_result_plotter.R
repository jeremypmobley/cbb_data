

# Script to plot distribution of leaderboard


# read in the data from csv saved in data folder
kaggle_results_df <- read.csv("~/Github/cbb_data/data/kaggle_results_df.csv")



### PLOT THE RESULTS ###

# only plot top 100 scores
top_num_to_plot <- 250
#plot(kaggle_results_df$score[1:top_num_to_plot])

years <- c(2014, 2015, 2016)


# plot all three years
par(mfrow=c(1,3)) 
for (year in years){
  leaderboard_df <- kaggle_results_df[kaggle_results_df$year == year,]
  plot(leaderboard_df$score[1:top_num_to_plot], 
       ylab = "logloss", 
       xlab = "", 
       ylim = c(0.4,0.6), main = year
      
       )
}





# plot last two years
two_years <- c(2015, 2016)

par(mfrow=c(1,length(two_years))) 
for (year in two_years){
  leaderboard_df <- kaggle_results_df[kaggle_results_df$year == year,]
  top_num_to_plot <- round(0.7 * nrow(leaderboard_df))
  plot(leaderboard_df$score[1:top_num_to_plot], 
       ylab = "Log loss", 
       xlab = "Competition Rank", 
       ylim = c(0.4,0.6), main = year
  )
  #abline(v = 10, col="blue")
}





# save plot out as png file
#png(filename = "kaggle_top_100_logloss.png", width = 1080, height = 480, units = "px")
#pdf(file = "kaggle_top_100_logloss.pdf")
#dev.off()







"""
# Kaggle points projection (on day of competition close, no time decay)
num_teammates <- 1
100000 / sqrt(num_teammates) * myplace^(-0.75) * log(1 + log(num_competitors)) * exp(-1/500)
"""



