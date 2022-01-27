# Akhil Anand 
# Applied Statistics
# This script will be used to do some graphing for the project
library(psych)

# Let's take a look at bar charts
# Idea, lets take home team winner percentages
# in the number of runs they score
boxplot(fmatches$runs)

# make 5 groups
# 0-52
# 53 - 105
# 106 - 159
# 160 - 212
# 213 - 265

group1 <- rep(0,636)
group2 <- rep(0,636)
group3 <- rep(0,636)
group4 <- rep(0,636)
group5 <- rep(0,636)

for(i in 1:636) {
  if(fmatches$runs[i] <= 52) {
    group1[i] <- 1
  }
  else if(fmatches$runs[i] > 52 && fmatches$runs[i] <= 105) {
    group2[i] <- 1
  }
  else if(fmatches$runs[i] > 105 && fmatches$runs[i] <= 159) {
    group3[i] <- 1
  }
  else if(fmatches$runs[i] > 159 && fmatches$runs[i] <= 212) {
    group4[i] <- 1
  }
  else {
    group5[i] <- 1
  }
}

groups <- c(1,32,301,287,15)
groups <- groups/636
groups <- groups*100

barplot(groups,angle=c(135,45,45,135,135),  
        density=c(24,-1,6,0,12),ylim=c(0,60), 
        names.arg=c("0-52","53-105","106-159", 
                    "160-212",">=213"),cex.axis=1.5,ylab=  
          "Percent",xlab="Run Group",cex.lab= 
          1.6,cex.names=1.5) 
axis(1,at=c(0,6),labels=c("","")) 


# Segmented Bar graph
# winner when in that group of runs
# winner when in that group of deliveries bowled

# Histogram
hist(fmatches$runs)
hist(fmatches$deliveries_bowled)

# Box plots 
boxplot(fmatches$runs~fmatches$home_team_winner)
boxplot(fmatches$deliveries_bowled~fmatches$home_team_winner)
boxplot(fmatches$win_by_runs~fmatches$home_team_winner)
boxplot(fmatches$win_by_wickets~fmatches$home_team_winner)
boxplot(fmatches$extras~fmatches$home_team_winner)

# Scatter PLots
attach(fmatches)
pairs.panels(data.frame(win_by_runs,win_by_wickets,runs,deliveries_bowled))

# percentage of home team toss winners
plot(fmatches$home_team_toss_winner~fmatches$home_team_winner, xlab="Home Team Winner", ylab="Toss Winner")
View(fmatches[fmatches$home_team_toss_winner == 1,])



