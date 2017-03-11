


# first look at 2017 files

setwd("C:/Users/Jeremy/Documents/GitHub/cbb_data/data/2017/")

teams <- read.csv("Teams.csv")
View(teams)
# team id lookup

seasons <- read.csv("Seasons.csv")
# contains date of season start, tournament region labels

sample_submission <- read.csv("sample_submission.csv")
# sample submission for round 1
# 2278 predictions for years 2013-2016

team_spellings <- read.csv("TeamSpellings.csv")
# team id lookup with multiple alternate spellings

team_conferences <- read.csv("TeamConferences_Thru2017.csv")
# team conference lookup per year

seed_round_slots <- read.csv("SeedRoundSlots.csv")
# what is early_daynum, late_daynum???

team_coaches <- read.csv("TeamCoaches_PrelimThru2017Day87.csv")
# coach for each team for each year


tourney_seeds <- read.csv("TourneySeeds.csv")
tourney_slots <- read.csv("TourneySlots.csv")

tourney_detailed_results <- read.csv("TourneyDetailedResults.csv")
tourney_compact_results <- read.csv("TourneyCompactResults.csv")
# detailed results only go back to 2004

reg_season_detailed_results <- read.csv("RegularSeasonDetailedResults.csv")
reg_season_compact_results <- read.csv("RegularSeasonCompactResults.csv")
# detailed results only go back to 2004


#### Pointspreads data

pointspreads <- read.csv("ThePredictionTrackerPointspreads.csv")
# pointspreads data only through 2015, need 2016 for prelim round model

sbr_lines <- read.csv("SBRLines.csv")
# lines for games going back to 2009, not sure of the source



massey_ordinals <- read.csv("massey_ordinals_2003-2016.csv")
head(massey_ordinals)



