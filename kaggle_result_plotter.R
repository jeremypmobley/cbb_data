

# Script to scrape Kaggle leaderboard and plot distribution of leaderboard


library(XML)
library(RCurl)


# set webpage to scrape
webpage <- "https://www.kaggle.com/c/march-machine-learning-mania-2016/leaderboard"

# read in HTML tables from webpage
pagedata <- getURL(webpage)
tables <- readHTMLTable(pagedata, header = FALSE)


# create dataframe of score values
leaderboard_df <- data.frame(score=tables$"leaderboard-table"$V4[2:nrow(tables$"leaderboard-table")])


myrow <- tables$"leaderboard-table"[tables$"leaderboard-table"$V3=='jeremypmobley',]
myscore <- as.numeric(as.character(myrow$V4))

# what place am I currently?
myplace <- as.numeric(as.character(myrow$V1))
myplace

# how many competitors are there?
num_competitors <- length(allscores_list)
num_competitors

# what percentile of competitors am I in?
my_pctile <- myplace/num_competitors
my_pctile

# how far behind am I behind leader?
abs((allscores_list[1] - myscore)/allscores_list[1])

# how far behind am I behind top-10?
abs((allscores_list[10] - myscore)/allscores_list[10])


# only plot top X% of entries
#pct_to_plot <- round(my_pctile,1) + 0.1
pct_to_plot <- 0.50
plot(allscores_list[1:round(pct_to_plot*num_competitors)])
abline(v=myplace, col='red')



# Points projection (on day of competition close, no time decay)
# assumes no teammates
num_teammates <- 1
100000 / sqrt(num_teammates) * myplace^(-0.75) * log(1 + log(num_competitors)) * exp(-1/500)





# Bring in end date from scrape




