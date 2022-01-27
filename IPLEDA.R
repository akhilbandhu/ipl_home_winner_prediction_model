# Akhil Anand
# MATH 2140
# Applied Statistics

# Project IPL
# First idea is to see how we can use all the characteristics from both data sets
# to see if we can predict the winner of the match
# Winner will be the response variable

#Libraries Used
library(stringr)

# Let's tackle the deliveries dataset
# this is every single delivery bowled in a match
# what all information can we get from this dataset
# Ideas
# 1. Get the total number of extras bowled in the given match
# 2. Get the number of deliveries bowled in the match
# 3. Get if the match had a super over
# 4. Get the number of wickets taken in the match
# 5. Get the runs scored in the match

# first split the data set based on innings
deliveries_1 <- subset(deliveries, deliveries$inning == 1)
deliveries_2 <- subset(deliveries, deliveries$inning == 2)
deliveries_3 <- subset(deliveries, deliveries$inning == 3)
deliveries_4 <- subset(deliveries, deliveries$inning == 4)

# Idea #1
# The column extra runs can be used for this
# Will have to loop through the data set
# keep adding runs to a counter 
# store the total extra runs in a vector
# there are 636 matches played

extras_inning1 <- rep(0, 636)
extras_inning2 <- rep(0, 636)

# getting the extra runs for innings 1 
# will be adding this to the data set matches as a variable
for(i in 1:636) {
  match <- deliveries_1[which(deliveries_1[,1] == i),]
  extras_inning1[i] <- sum(match$extra_runs)
}

# getting the extra runs for innings 2
# will be adding this as well to the matches data set
for(i in 1:636) {
  match <- deliveries_2[which(deliveries_2[,1] == i),]
  extras_inning2[i] <- sum(match$extra_runs)
}

# Idea 2
# get the number of deliveries bowled in a match
deliveries_bowled_innings1 <- rep(0,636)
deliveries_bowled_innings2 <- rep(0,636)

# innings 1
for(i in 1:636) {
  match <- deliveries_1[which(deliveries_1[,1] == i),]
  deliveries_bowled_innings1[i] <- nrow(match)
}

# innings 2
for(i in 1:636) {
  match <- deliveries_2[which(deliveries_2[,1] == i),]
  deliveries_bowled_innings2[i] <- nrow(match)
}

# Idea 3
# get the subset and check if any of the super over columns is a 1
super_over <- rep(0,636)
deliveries_3$match_id
super_over[c(34,126,190,388,401,476,536)] = 1


# Idea 4
# Number of wickets taken in a match
# per innings
wickets_innings1 <- rep(0,636)
wickets_innings2 <- rep(0,636)


# Idea 5 
# Get the runs scored in the match 
# per innings
total_runs_innings1 <- rep(0,636)
total_runs_innings2 <- rep(0,636)

# innings 1
for(i in 1:636) {
  match <- deliveries_1[which(deliveries_1[,1] == i),]
  total_runs_innings1[i] <- sum(match$total_runs)
}

# innings 2
for(i in 1:636) {
  match <- deliveries_2[which(deliveries_2[,1] == i),]
  total_runs_innings2[i] <- sum(match$total_runs)
}


# Adding all the new columns to the matches data set
# adding the extras per innings
matches$extras_innings1 <- extras_inning1
matches$extras_innings2 <- extras_inning2

# adding deliveries bowled, will maybe have to change it to net overs bowled
matches$deliveries_innings1 <- deliveries_bowled_innings1
matches$deliveries_innings2 <- deliveries_bowled_innings2

# adding idea 3
# adding if match had super over 
matches$super_over <- super_over

# adding idea 5
# total runs per innings
matches$total_runs_innings1 <- total_runs_innings1
matches$total_runs_innings2 <- total_runs_innings2

# making a subset of data 
# keeping the columns I want
# will make a couple more adjustments to this
fmatches <- matches[,-c(1,2,4,14,16,17,18)]
View(fmatches) 

# changing the names to the acronyms
# Kolkata Knight Riders -> KKR
# Rising Pune Supergiants -> RPS
# and it keeps going
# using the stringr library

fmatches$team1 <- as.character(fmatches$team1)
fmatches$team2 <- as.character(fmatches$team2)
fmatches$toss_winner <- as.character(fmatches$toss_winner)
fmatches$winner <- as.character(fmatches$winner)
fmatches$city <- as.character(fmatches$city)

for(i in 1:nrow(fmatches)) {
  if(fmatches$team1[i] == "Chennai Super Kings") {
    fmatches$team1[i] = "CSK"
  } 
  if(fmatches$team1[i] == "Deccan Chargers") {
    fmatches$team1[i] = "DC"
  }
  if(fmatches$team1[i] == "Delhi Daredevils") {
    fmatches$team1[i] = "DD"
  }
  if(fmatches$team1[i] == "Gujarat Lions") {
    fmatches$team1[i] = "GL"
  }
  if(fmatches$team1[i] == "Kings XI Punjab") {
    fmatches$team1[i] = "KXIP"
  }
  if(fmatches$team1[i] == "Kochi Tuskers Kerala") {
    fmatches$team1[i] = "KTK"
  }
  if(fmatches$team1[i] == "Kolkata Knight Riders") {
    fmatches$team1[i] = "KKR"
  }
  if(fmatches$team1[i] == "Mumbai Indians") {
    fmatches$team1[i] = "MI"
  }
  if(fmatches$team1[i] == "Pune Warriors") {
    fmatches$team1[i] = "PW"
  }
  if(fmatches$team1[i] == "Rajasthan Royals") {
    fmatches$team1[i] = "RR"
  }
  if(fmatches$team1[i] == "Rising Pune Supergiant" || fmatches$team1[i] == "Rising Pune Supergiants") {
    fmatches$team1[i] = "RPS"
  }
  if(fmatches$team1[i] == "Royal Challengers Bangalore") {
    fmatches$team1[i] = "RCB"
  }
  if(fmatches$team1[i] == "Sunrisers Hyderabad") {
    fmatches$team1[i] = "SRH"
  }
}

# changing team 2 to acronyms
for(i in 1:nrow(fmatches)) {
  if(fmatches$team2[i] == "Chennai Super Kings") {
    fmatches$team2[i] = "CSK"
  } 
  if(fmatches$team2[i] == "Deccan Chargers") {
    fmatches$team2[i] = "DC"
  }
  if(fmatches$team2[i] == "Delhi Daredevils") {
    fmatches$team2[i] = "DD"
  }
  if(fmatches$team2[i] == "Gujarat Lions") {
    fmatches$team2[i] = "GL"
  }
  if(fmatches$team2[i] == "Kings XI Punjab") {
    fmatches$team2[i] = "KXIP"
  }
  if(fmatches$team2[i] == "Kochi Tuskers Kerala") {
    fmatches$team2[i] = "KTK"
  }
  if(fmatches$team2[i] == "Kolkata Knight Riders") {
    fmatches$team2[i] = "KKR"
  }
  if(fmatches$team2[i] == "Mumbai Indians") {
    fmatches$team2[i] = "MI"
  }
  if(fmatches$team2[i] == "Pune Warriors") {
    fmatches$team2[i] = "PW"
  }
  if(fmatches$team2[i] == "Rajasthan Royals") {
    fmatches$team2[i] = "RR"
  }
  if(fmatches$team2[i] == "Rising Pune Supergiant" || fmatches$team2[i] == "Rising Pune Supergiants") {
    fmatches$team2[i] = "RPS"
  }
  if(fmatches$team2[i] == "Royal Challengers Bangalore") {
    fmatches$team2[i] = "RCB"
  }
  if(fmatches$team2[i] == "Sunrisers Hyderabad") {
    fmatches$team2[i] = "SRH"
  }
}

# changing the names of toss winners
for(i in 1:nrow(fmatches)) {
  if(fmatches$toss_winner[i] == "Chennai Super Kings") {
    fmatches$toss_winner[i] = "CSK"
  } 
  if(fmatches$toss_winner[i] == "Deccan Chargers") {
    fmatches$toss_winner[i] = "DC"
  }
  if(fmatches$toss_winner[i] == "Delhi Daredevils") {
    fmatches$toss_winner[i] = "DD"
  }
  if(fmatches$toss_winner[i] == "Gujarat Lions") {
    fmatches$toss_winner[i] = "GL"
  }
  if(fmatches$toss_winner[i] == "Kings XI Punjab") {
    fmatches$toss_winner[i] = "KXIP"
  }
  if(fmatches$toss_winner[i] == "Kochi Tuskers Kerala") {
    fmatches$toss_winner[i] = "KTK"
  }
  if(fmatches$toss_winner[i] == "Kolkata Knight Riders") {
    fmatches$toss_winner[i] = "KKR"
  }
  if(fmatches$toss_winner[i] == "Mumbai Indians") {
    fmatches$toss_winner[i] = "MI"
  }
  if(fmatches$toss_winner[i] == "Pune Warriors") {
    fmatches$toss_winner[i] = "PW"
  }
  if(fmatches$toss_winner[i] == "Rajasthan Royals") {
    fmatches$toss_winner[i] = "RR"
  }
  if(fmatches$toss_winner[i] == "Rising Pune Supergiant" || fmatches$toss_winner[i] == "Rising Pune Supergiants") {
    fmatches$toss_winner[i] = "RPS"
  }
  if(fmatches$toss_winner[i] == "Royal Challengers Bangalore") {
    fmatches$toss_winner[i] = "RCB"
  }
  if(fmatches$toss_winner[i] == "Sunrisers Hyderabad") {
    fmatches$toss_winner[i] = "SRH"
  }
}

# changing winner names to acronyms
for(i in 1:nrow(fmatches)) {
  if(fmatches$winner[i] == "Chennai Super Kings") {
    fmatches$winner[i] = "CSK"
  } 
  if(fmatches$winner[i] == "Deccan Chargers") {
    fmatches$winner[i] = "DC"
  }
  if(fmatches$winner[i] == "Delhi Daredevils") {
    fmatches$winner[i] = "DD"
  }
  if(fmatches$winner[i] == "Gujarat Lions") {
    fmatches$winner[i] = "GL"
  }
  if(fmatches$winner[i] == "Kings XI Punjab") {
    fmatches$winner[i] = "KXIP"
  }
  if(fmatches$winner[i] == "Kochi Tuskers Kerala") {
    fmatches$winner[i] = "KTK"
  }
  if(fmatches$winner[i] == "Kolkata Knight Riders") {
    fmatches$winner[i] = "KKR"
  }
  if(fmatches$winner[i] == "Mumbai Indians") {
    fmatches$winner[i] = "MI"
  }
  if(fmatches$winner[i] == "Pune Warriors") {
    fmatches$winner[i] = "PW"
  }
  if(fmatches$winner[i] == "Rajasthan Royals") {
    fmatches$winner[i] = "RR"
  }
  if(fmatches$winner[i] == "Rising Pune Supergiant" || fmatches$winner[i] == "Rising Pune Supergiants") {
    fmatches$winner[i] = "RPS"
  }
  if(fmatches$winner[i] == "Royal Challengers Bangalore") {
    fmatches$winner[i] = "RCB"
  }
  if(fmatches$winner[i] == "Sunrisers Hyderabad") {
    fmatches$winner[i] = "SRH"
  }
}



# Things to do
# 1. Change names to their short hands
# 2. The goal of the first project would be to predict if home team wins or not
# 3. Another idea would be to use multi-nomial logistic regression to predict which team wins
# this could be a good idea
# For the first project, I can use knn's and trees
# should get the home team first before doing all the changes above, will need to rerun this crap
# two data set split
# one for predicting whether home team wins and the other predict which team wins


# Let's tackle the project of predicting home team winner
# there is some more cleaning to attend to
# got to change winner into a boolean
# logic:
#       if the team which wins plays in their home ground, we make it a 1
#       else we make it a 0. we can compare to the first column
# let's name it match data 1 and 2
# match_data 1 will be used for the first project and the other will be used for second multinomial project
# instead of team 1 and team 2 we can which team did the home team play against
# for not technically at home stadiums, let's make home team "team 1"

match_data1 <- fmatches
match_data2 <- fmatches

# let's do the transformations on match_data1

home_team_winner <- rep(0,636)
for(i in 1:nrow(fmatches)) {
  if(fmatches$city[i] == "Hyderabad" && fmatches$winner[i] == "SRH") {
    home_team[i] <- 1
  }
  else if(fmatches$city[i] == "Pune" && fmatches$winner[i] == "RPS") {
    home_team[i] <- 1
  }
  else if(fmatches$city[i] == "Pune" && fmatches$winner[i] == "PW") {
    home_team[i] <- 1
  }
  else if(fmatches$city[i] == "Ahmedabad" && fmatches$winner[i] == "GL") {
    home_team[i] <- 1
  }
  else if(fmatches$city[i] == "Rajkot" && fmatches$winner[i] == "GL") {
    home_team[i] <- 1
  }
  else if(fmatches$city[i] == "Kanpur" && fmatches$winner[i] == "GL") {
    home_team[i] <- 1
  }
  else if(fmatches$city[i] == "Hyderabad" && fmatches$winner[i] == "DC") {
    home_team[i] <- 1
  }
  else if(fmatches$city[i] == "Delhi" && fmatches$winner[i] == "DD") {
    home_team[i] <- 1
  }
  else if(fmatches$city[i] == "Raipur" && fmatches$winner[i] == "DD") {
    home_team[i] <- 1
  }
  else if(fmatches$city[i] == "Ranchi" && fmatches$winner[i] == "KKR") {
    home_team[i] <- 1
  }
  else if(fmatches$city[i] == "Kolkata" && fmatches$winner[i] == "KKR") {
    home_team[i] <- 1
  }
  else if(fmatches$city[i] == "Bangalore" && fmatches$winner[i] == "RCB") {
    home_team[i] <- 1
  }
  else if(fmatches$city[i] == "Chennai" && fmatches$winner[i] == "CSK") {
    home_team[i] <- 1
  }
  else if(fmatches$city[i] == "Cuttack" && fmatches$winner[i] == "KTK") {
    home_team[i] <- 1
  }
  else if(fmatches$city[i] == "Dharamsala" && fmatches$winner[i] == "KXIP") {
    home_team[i] <- 1
  }
  else if(fmatches$city[i] == "Indore" && fmatches$winner[i] == "KXIP") {
    home_team[i] <- 1
  }
  else if(fmatches$city[i] == "Mumbai" && fmatches$winner[i] == "MI") {
    home_team[i] <- 1
  }
  else if(fmatches$city[i] == "Visakhapatnam" && fmatches$winner[i] == "MI") {
    home_team[i] <- 1
  }
  else if(fmatches$city[i] == "Jaipur" && fmatches$winner[i] == "RR") {
    home_team[i] <- 1
  }
  else{
    home_team[i] <- 0
  }
}

fmatches$team1 <- factor(fmatches$team1)
fmatches$team2 <- factor(fmatches$team2)
fmatches$toss_winner <- as.factor(fmatches$toss_winner)
fmatches$winner <- as.factor(fmatches$winner)
fmatches$city <- as.factor(fmatches$city)

# okay now we have the home team winner column, let's replace winner with that
names(fmatches)
fmatches[,8] <- home_team

# let's run the same code for the toss winnner
home_team_toss_winner <- rep(0,636)
for(i in 1:nrow(fmatches)) {
  if(fmatches$city[i] == "Hyderabad" && fmatches$toss_winner[i] == "SRH") {
    home_team_toss_winner[i] <- 1
  }
  else if(fmatches$city[i] == "Pune" && fmatches$toss_winner[i] == "RPS") {
    home_team_toss_winner[i] <- 1
  }
  else if(fmatches$city[i] == "Pune" && fmatches$toss_winner[i] == "PW") {
    home_team_toss_winner[i] <- 1
  }
  else if(fmatches$city[i] == "Ahmedabad" && fmatches$toss_winner[i] == "GL") {
    home_team_toss_winner[i] <- 1
  }
  else if(fmatches$city[i] == "Rajkot" && fmatches$toss_winner[i] == "GL") {
    home_team_toss_winner[i] <- 1
  }
  else if(fmatches$city[i] == "Kanpur" && fmatches$toss_winner[i] == "GL") {
    home_team_toss_winner[i] <- 1
  }
  else if(fmatches$city[i] == "Hyderabad" && fmatches$toss_winner[i] == "DC") {
    home_team_toss_winner[i] <- 1
  }
  else if(fmatches$city[i] == "Delhi" && fmatches$toss_winner[i] == "DD") {
    home_team_toss_winner[i] <- 1
  }
  else if(fmatches$city[i] == "Raipur" && fmatches$toss_winner[i] == "DD") {
    home_team_toss_winner[i] <- 1
  }
  else if(fmatches$city[i] == "Ranchi" && fmatches$toss_winner[i] == "KKR") {
    home_team_toss_winner[i] <- 1
  }
  else if(fmatches$city[i] == "Kolkata" && fmatches$toss_winner[i] == "KKR") {
    home_team_toss_winner[i] <- 1
  }
  else if(fmatches$city[i] == "Bangalore" && fmatches$toss_winner[i] == "RCB") {
    home_team_toss_winner[i] <- 1
  }
  else if(fmatches$city[i] == "Chennai" && fmatches$toss_winner[i] == "CSK") {
    home_team_toss_winner[i] <- 1
  }
  else if(fmatches$city[i] == "Cuttack" && fmatches$toss_winner[i] == "KTK") {
    home_team_toss_winner[i] <- 1
  }
  else if(fmatches$city[i] == "Dharamsala" && fmatches$toss_winner[i] == "KXIP") {
    home_team_toss_winner[i] <- 1
  }
  else if(fmatches$city[i] == "Indore" && fmatches$toss_winner[i] == "KXIP") {
    home_team_toss_winner[i] <- 1
  }
  else if(fmatches$city[i] == "Mumbai" && fmatches$toss_winner[i] == "MI") {
    home_team_toss_winner[i] <- 1
  }
  else if(fmatches$city[i] == "Visakhapatnam" && fmatches$toss_winner[i] == "MI") {
    home_team_toss_winner[i] <- 1
  }
  else if(fmatches$city[i] == "Jaipur" && fmatches$toss_winner[i] == "RR") {
    home_team_toss_winner[i] <- 1
  }
  else{
    home_team_toss_winner[i] <- 0
  }
}

# changing the result column to whether the home team won the toss
fmatches$result <- home_team_toss_winner
fmatches$home_team_toss_winner <- as.factor(fmatches$home_team_toss_winner)
colnames(fmatches)[6] <- "home_team_toss_winner"
colnames(fmatches)[8] <- "home_team_winner"
fmatches[,4] <- NULL
# EDA ideas
# extras per innings
# runs per innings per match 
# bar charts maybe
# deliveries bowled per innings per match
# first we should keep the data for the home team innings 
home_team_extras <- rep(0,636)
for(i in 1:nrow(fmatches)) {
  if(fmatches$city[i] == "Hyderabad" && fmatches$team1[i] == "SRH") {
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
  else if(fmatches$city[i] == "Hyderabad" && fmatches$team2[i] == "SRH") {
    home_team_extras[i] <- fmatches$extras_innings2[i]
  }
  else if(fmatches$city[i] == "Pune" && fmatches$team1[i] == "RPS") {
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
  else if(fmatches$city[i] == "Pune" && fmatches$team2[i] == "RPS") {
    home_team_extras[i] <- fmatches$extras_innings2[i]
  }
  else if(fmatches$city[i] == "Pune" && fmatches$team1[i] == "PW") {
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
  else if(fmatches$city[i] == "Pune" && fmatches$team2[i] == "PW") {
    home_team_extras[i] <- fmatches$extras_innings2[i]
  }
  else if(fmatches$city[i] == "Ahmedabad" && fmatches$team1[i] == "GL") {
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
  else if(fmatches$city[i] == "Ahmedabad" && fmatches$team2[i] == "GL") {
    home_team_extras[i] <- fmatches$extras_innings2[i]
  }
  else if(fmatches$city[i] == "Rajkot" && fmatches$team1[i] == "GL") {
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
  else if(fmatches$city[i] == "Rajkot" && fmatches$team2[i] == "GL") {
    home_team_extras[i] <- fmatches$extras_innings2[i]
  }
  else if(fmatches$city[i] == "Kanpur" && fmatches$team1[i] == "GL") {
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
  else if(fmatches$city[i] == "Kanpur" && fmatches$team2[i] == "GL") {
    home_team_extras[i] <- fmatches$extras_innings2[i]
  }
  else if(fmatches$city[i] == "Hyderabad" && fmatches$team1[i] == "DC") {
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
  else if(fmatches$city[i] == "Hyderabad" && fmatches$team2[i] == "DC") {
    home_team_extras[i] <- fmatches$extras_innings2[i]
  }
  else if(fmatches$city[i] == "Delhi" && fmatches$team1[i] == "DD") {
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
  else if(fmatches$city[i] == "Delhi" && fmatches$team2[i] == "DD") {
    home_team_extras[i] <- fmatches$extras_innings2[i]
  }
  else if(fmatches$city[i] == "Raipur" && fmatches$team1[i] == "DD") {
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
  else if(fmatches$city[i] == "Raipur" && fmatches$team2[i] == "DD") {
    home_team_extras[i] <- fmatches$extras_innings2[i]
  }
  else if(fmatches$city[i] == "Ranchi" && fmatches$team1[i] == "KKR") {
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
  else if(fmatches$city[i] == "Ranchi" && fmatches$team2[i] == "KKR") {
    home_team_extras[i] <- fmatches$extras_innings2[i]
  }
  else if(fmatches$city[i] == "Kolkata" && fmatches$team1[i] == "KKR") {
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
  else if(fmatches$city[i] == "Kolkata" && fmatches$team2[i] == "KKR") {
    home_team_extras[i] <- fmatches$extras_innings2[i]
  }
  else if(fmatches$city[i] == "Bangalore" && fmatches$team1[i] == "RCB") {
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
  else if(fmatches$city[i] == "Bangalore" && fmatches$team2[i] == "RCB") {
    home_team_extras[i] <- fmatches$extras_innings2[i]
  }
  else if(fmatches$city[i] == "Chennai" && fmatches$team1[i] == "CSK") {
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
  else if(fmatches$city[i] == "Chennai" && fmatches$team2[i] == "CSK") {
    home_team_extras[i] <- fmatches$extras_innings2[i]
  }
  else if(fmatches$city[i] == "Cuttack" && fmatches$team1[i] == "KTK") {
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
  else if(fmatches$city[i] == "Cuttack" && fmatches$team2[i] == "KTK") {
    home_team_extras[i] <- fmatches$extras_innings2[i]
  }
  else if(fmatches$city[i] == "Dharamsala" && fmatches$team1[i] == "KXIP") {
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
  else if(fmatches$city[i] == "Dharamsala" && fmatches$team2[i] == "KXIP") {
    home_team_extras[i] <- fmatches$extras_innings2[i]
  }
  else if(fmatches$city[i] == "Indore" && fmatches$team1[i] == "KXIP") {
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
  else if(fmatches$city[i] == "Indore" && fmatches$team2[i] == "KXIP") {
    home_team_extras[i] <- fmatches$extras_innings2[i]
  }
  else if(fmatches$city[i] == "Mumbai" && fmatches$team1[i] == "MI") {
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
  else if(fmatches$city[i] == "Mumbai" && fmatches$team2[i] == "MI") {
    home_team_extras[i] <- fmatches$extras_innings2[i]
  }
  else if(fmatches$city[i] == "Visakhapatnam" && fmatches$team1[i] == "MI") {
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
  else if(fmatches$city[i] == "Visakhapatnam" && fmatches$team2[i] == "MI") {
    home_team_extras[i] <- fmatches$extras_innings2[i]
  }
  else if(fmatches$city[i] == "Jaipur" && fmatches$team1[i] == "RR") {
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
  else if(fmatches$city[i] == "Jaipur" && fmatches$team2[i] == "RR") {
    home_team_extras[i] <- fmatches$extras_innings2[i]
  }
  else{
    home_team_extras[i] <- fmatches$extras_innings1[i]
  }
}

fmatches$extras <- home_team_extras

# doing the same thing for deliveries and for total runs
home_team_runs <- rep(0,636)
for(i in 1:nrow(fmatches)) {
  if(fmatches$city[i] == "Hyderabad" && fmatches$team1[i] == "SRH") {
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
  else if(fmatches$city[i] == "Hyderabad" && fmatches$team2[i] == "SRH") {
    home_team_runs[i] <- fmatches$total_runs_innings2[i]
  }
  else if(fmatches$city[i] == "Pune" && fmatches$team1[i] == "RPS") {
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
  else if(fmatches$city[i] == "Pune" && fmatches$team2[i] == "RPS") {
    home_team_runs[i] <- fmatches$total_runs_innings2[i]
  }
  else if(fmatches$city[i] == "Pune" && fmatches$team1[i] == "PW") {
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
  else if(fmatches$city[i] == "Pune" && fmatches$team2[i] == "PW") {
    home_team_runs[i] <- fmatches$total_runs_innings2[i]
  }
  else if(fmatches$city[i] == "Ahmedabad" && fmatches$team1[i] == "GL") {
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
  else if(fmatches$city[i] == "Ahmedabad" && fmatches$team2[i] == "GL") {
    home_team_runs[i] <- fmatches$total_runs_innings2[i]
  }
  else if(fmatches$city[i] == "Rajkot" && fmatches$team1[i] == "GL") {
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
  else if(fmatches$city[i] == "Rajkot" && fmatches$team2[i] == "GL") {
    home_team_runs[i] <- fmatches$total_runs_innings2[i]
  }
  else if(fmatches$city[i] == "Kanpur" && fmatches$team1[i] == "GL") {
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
  else if(fmatches$city[i] == "Kanpur" && fmatches$team2[i] == "GL") {
    home_team_runs[i] <- fmatches$total_runs_innings2[i]
  }
  else if(fmatches$city[i] == "Hyderabad" && fmatches$team1[i] == "DC") {
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
  else if(fmatches$city[i] == "Hyderabad" && fmatches$team2[i] == "DC") {
    home_team_runs[i] <- fmatches$total_runs_innings2[i]
  }
  else if(fmatches$city[i] == "Delhi" && fmatches$team1[i] == "DD") {
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
  else if(fmatches$city[i] == "Delhi" && fmatches$team2[i] == "DD") {
    home_team_runs[i] <- fmatches$total_runs_innings2[i]
  }
  else if(fmatches$city[i] == "Raipur" && fmatches$team1[i] == "DD") {
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
  else if(fmatches$city[i] == "Raipur" && fmatches$team2[i] == "DD") {
    home_team_runs[i] <- fmatches$total_runs_innings2[i]
  }
  else if(fmatches$city[i] == "Ranchi" && fmatches$team1[i] == "KKR") {
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
  else if(fmatches$city[i] == "Ranchi" && fmatches$team2[i] == "KKR") {
    home_team_runs[i] <- fmatches$total_runs_innings2[i]
  }
  else if(fmatches$city[i] == "Kolkata" && fmatches$team1[i] == "KKR") {
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
  else if(fmatches$city[i] == "Kolkata" && fmatches$team2[i] == "KKR") {
    home_team_runs[i] <- fmatches$total_runs_innings2[i]
  }
  else if(fmatches$city[i] == "Bangalore" && fmatches$team1[i] == "RCB") {
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
  else if(fmatches$city[i] == "Bangalore" && fmatches$team2[i] == "RCB") {
    home_team_runs[i] <- fmatches$total_runs_innings2[i]
  }
  else if(fmatches$city[i] == "Chennai" && fmatches$team1[i] == "CSK") {
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
  else if(fmatches$city[i] == "Chennai" && fmatches$team2[i] == "CSK") {
    home_team_runs[i] <- fmatches$total_runs_innings2[i]
  }
  else if(fmatches$city[i] == "Cuttack" && fmatches$team1[i] == "KTK") {
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
  else if(fmatches$city[i] == "Cuttack" && fmatches$team2[i] == "KTK") {
    home_team_runs[i] <- fmatches$total_runs_innings2[i]
  }
  else if(fmatches$city[i] == "Dharamsala" && fmatches$team1[i] == "KXIP") {
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
  else if(fmatches$city[i] == "Dharamsala" && fmatches$team2[i] == "KXIP") {
    home_team_runs[i] <- fmatches$total_runs_innings2[i]
  }
  else if(fmatches$city[i] == "Indore" && fmatches$team1[i] == "KXIP") {
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
  else if(fmatches$city[i] == "Indore" && fmatches$team2[i] == "KXIP") {
    home_team_runs[i] <- fmatches$total_runs_innings2[i]
  }
  else if(fmatches$city[i] == "Mumbai" && fmatches$team1[i] == "MI") {
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
  else if(fmatches$city[i] == "Mumbai" && fmatches$team2[i] == "MI") {
    home_team_runs[i] <- fmatches$total_runs_innings2[i]
  }
  else if(fmatches$city[i] == "Visakhapatnam" && fmatches$team1[i] == "MI") {
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
  else if(fmatches$city[i] == "Visakhapatnam" && fmatches$team2[i] == "MI") {
    home_team_runs[i] <- fmatches$total_runs_innings2[i]
  }
  else if(fmatches$city[i] == "Jaipur" && fmatches$team1[i] == "RR") {
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
  else if(fmatches$city[i] == "Jaipur" && fmatches$team2[i] == "RR") {
    home_team_runs[i] <- fmatches$total_runs_innings2[i]
  }
  else{
    home_team_runs[i] <- fmatches$total_runs_innings1[i]
  }
}

fmatches$runs <- home_team_runs

# for deliveries bowled
deliveries_bowled <- rep(0,636)
for(i in 1:nrow(fmatches)) {
  if(fmatches$city[i] == "Hyderabad" && fmatches$team1[i] == "SRH") {
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
  else if(fmatches$city[i] == "Hyderabad" && fmatches$team2[i] == "SRH") {
    deliveries_bowled[i] <- fmatches$deliveries_innings2[i]
  }
  else if(fmatches$city[i] == "Pune" && fmatches$team1[i] == "RPS") {
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
  else if(fmatches$city[i] == "Pune" && fmatches$team2[i] == "RPS") {
    deliveries_bowled[i] <- fmatches$deliveries_innings2[i]
  }
  else if(fmatches$city[i] == "Pune" && fmatches$team1[i] == "PW") {
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
  else if(fmatches$city[i] == "Pune" && fmatches$team2[i] == "PW") {
    deliveries_bowled[i] <- fmatches$deliveries_innings2[i]
  }
  else if(fmatches$city[i] == "Ahmedabad" && fmatches$team1[i] == "GL") {
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
  else if(fmatches$city[i] == "Ahmedabad" && fmatches$team2[i] == "GL") {
    deliveries_bowled[i] <- fmatches$deliveries_innings2[i]
  }
  else if(fmatches$city[i] == "Rajkot" && fmatches$team1[i] == "GL") {
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
  else if(fmatches$city[i] == "Rajkot" && fmatches$team2[i] == "GL") {
    deliveries_bowled[i] <- fmatches$deliveries_innings2[i]
  }
  else if(fmatches$city[i] == "Kanpur" && fmatches$team1[i] == "GL") {
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
  else if(fmatches$city[i] == "Kanpur" && fmatches$team2[i] == "GL") {
    deliveries_bowled[i] <- fmatches$deliveries_innings2[i]
  }
  else if(fmatches$city[i] == "Hyderabad" && fmatches$team1[i] == "DC") {
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
  else if(fmatches$city[i] == "Hyderabad" && fmatches$team2[i] == "DC") {
    deliveries_bowled[i] <- fmatches$deliveries_innings2[i]
  }
  else if(fmatches$city[i] == "Delhi" && fmatches$team1[i] == "DD") {
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
  else if(fmatches$city[i] == "Delhi" && fmatches$team2[i] == "DD") {
    deliveries_bowled[i] <- fmatches$deliveries_innings2[i]
  }
  else if(fmatches$city[i] == "Raipur" && fmatches$team1[i] == "DD") {
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
  else if(fmatches$city[i] == "Raipur" && fmatches$team2[i] == "DD") {
    deliveries_bowled[i] <- fmatches$deliveries_innings2[i]
  }
  else if(fmatches$city[i] == "Ranchi" && fmatches$team1[i] == "KKR") {
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
  else if(fmatches$city[i] == "Ranchi" && fmatches$team2[i] == "KKR") {
    deliveries_bowled[i] <- fmatches$deliveries_innings2[i]
  }
  else if(fmatches$city[i] == "Kolkata" && fmatches$team1[i] == "KKR") {
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
  else if(fmatches$city[i] == "Kolkata" && fmatches$team2[i] == "KKR") {
    deliveries_bowled[i] <- fmatches$deliveries_innings2[i]
  }
  else if(fmatches$city[i] == "Bangalore" && fmatches$team1[i] == "RCB") {
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
  else if(fmatches$city[i] == "Bangalore" && fmatches$team2[i] == "RCB") {
    deliveries_bowled[i] <- fmatches$deliveries_innings2[i]
  }
  else if(fmatches$city[i] == "Chennai" && fmatches$team1[i] == "CSK") {
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
  else if(fmatches$city[i] == "Chennai" && fmatches$team2[i] == "CSK") {
    deliveries_bowled[i] <- fmatches$deliveries_innings2[i]
  }
  else if(fmatches$city[i] == "Cuttack" && fmatches$team1[i] == "KTK") {
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
  else if(fmatches$city[i] == "Cuttack" && fmatches$team2[i] == "KTK") {
    deliveries_bowled[i] <- fmatches$deliveries_innings2[i]
  }
  else if(fmatches$city[i] == "Dharamsala" && fmatches$team1[i] == "KXIP") {
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
  else if(fmatches$city[i] == "Dharamsala" && fmatches$team2[i] == "KXIP") {
    deliveries_bowled[i] <- fmatches$deliveries_innings2[i]
  }
  else if(fmatches$city[i] == "Indore" && fmatches$team1[i] == "KXIP") {
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
  else if(fmatches$city[i] == "Indore" && fmatches$team2[i] == "KXIP") {
    deliveries_bowled[i] <- fmatches$deliveries_innings2[i]
  }
  else if(fmatches$city[i] == "Mumbai" && fmatches$team1[i] == "MI") {
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
  else if(fmatches$city[i] == "Mumbai" && fmatches$team2[i] == "MI") {
    deliveries_bowled[i] <- fmatches$deliveries_innings2[i]
  }
  else if(fmatches$city[i] == "Visakhapatnam" && fmatches$team1[i] == "MI") {
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
  else if(fmatches$city[i] == "Visakhapatnam" && fmatches$team2[i] == "MI") {
    deliveries_bowled[i] <- fmatches$deliveries_innings2[i]
  }
  else if(fmatches$city[i] == "Jaipur" && fmatches$team1[i] == "RR") {
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
  else if(fmatches$city[i] == "Jaipur" && fmatches$team2[i] == "RR") {
    deliveries_bowled[i] <- fmatches$deliveries_innings2[i]
  }
  else{
    deliveries_bowled[i] <- fmatches$deliveries_innings1[i]
  }
}

fmatches$deliveries_bowled <- deliveries_bowled

# dropping venue column
fmatches$venue <- NULL

# dropping extras_innings1 and 2 
# dropping total_runs1 and 2 
# dropping deliveries_innings1 and 2
fmatches$extras_innings1 <- NULL
fmatches$extras_innings2 <- NULL
fmatches$total_runs_innings1 <- NULL
fmatches$total_runs_innings2 <- NULL
fmatches$deliveries_innings1 <- NULL
fmatches$deliveries_innings2 <- NULL

# make toss winner into factor
# make dl_applied into factor
# make super_over into factor

fmatches$home_team_toss_winner <- as.factor(fmatches$home_team_toss_winner)
fmatches$dl_applied <- as.factor(fmatches$dl_applied)
fmatches$super_over <- as.factor(fmatches$super_over)

# how about we start modelling now
