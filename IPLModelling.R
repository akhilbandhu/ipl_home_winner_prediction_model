# Akhil Anand
# MATH 2140
# Applied Statistics
# This will be the script for modelling and some graphs and all
# Both main projects will be in this script
# Even the multinomial with the other data set. 
# Also the project predicting scores will be here
install.packages("tree")
install.packages("tidyverse")
install.packages("glmnet")
install.packages("randomForest")
library(randomForest)
library(nnet) 
library(glmnet)
library(tidyverse)
library(tree)
library(ResourceSelection)
library(faraway)
library(asbio)
library(signmedian.test)
library(boot)
library(class)

# lets try running a full model and see what happens shall we
full_model <- glm(home_team_winner~.-city-team1-team2, data = fmatches, family = "binomial") 
summary(full_model)
logistic.plots(full_model)
# point 571 seems to be a problem

# now lets split the data into training and testing
# might also do some validation stuff
set.seed(1234)
num_obs <- nrow(fmatches)
train_obs <- sample(num_obs, 0.67*num_obs)
training_data <- fmatches[train_obs,]
testing_data <- fmatches[-train_obs,]

# the model I think should be good
log_fit <- glm(home_team_winner~.-team1-team2-toss_decision, data=training_data, family="binomial")

predictions_log_fit <- predict(log_fit,newdata=testing_data)
classifications_log_fit <- ifelse(predictions_log_fit>0.5,1,0)
acc_log_fit <- sum(testing_data$home_team_winner==classifications_log_fit)/nrow(testing_data)
hoslem.test(testing_data, fitted(log_fit), g=10)
logistic.plots(log_fit)
anova(log_fit,test="Chisq")
plot(residuals(log_fit)~fitted(log_fit))

# fitting a new model with the split
fit1 <- glm(home_team_winner~.-city-team1-team2, data = training_data, family = "binomial")
logistic.plots(fit1)
lin_preds_train <- predict(fit1,newdata=training_data)
lin_preds_test <- predict(fit1,newdata=testing_data)

class_winner <- ifelse(lin_preds_test>0.5,1,0)
acc_test <- sum(testing_data$home_team_winner==class_winner)/nrow(testing_data)

# log model final 
fit2 <- glm(home_team_winner ~ city + deliveries_bowled + runs + dl_applied + 
              win_by_wickets + extras, data = training_data, family = "binomial")
range(residuals(fit2, "pearson"))
?residuals.glm

# why is this not working
logistic.plots(fit2)
fit2_test <- predict(fit2,newdata=testing_data)

class_winner_fit2 <- ifelse(fit2_test>0.5,1,0)
acc_fit2_test <- sum(testing_data$home_team_winner==class_winner_fit2)/nrow(testing_data)

plot(fit2)
anova(fit2,test="Chisq")
hoslem.test(training_data$home_team_winner,fitted(fit2),g=100) 


# Multinomial regression
# lets get training and testing and try again
set.seed(1234)
num_obs <- nrow(match_data1)
train_obs <- sample(num_obs, 0.67*num_obs)
training_data_multinomial <- match_data1[train_obs,]
testing_data_multinomial  <- match_data1[-train_obs,]
multinomial_fit  =  multinom(winner ~ city + deliveries_bowled + runs + dl_applied + 
                               win_by_wickets + extras,data=training_data_multinomial) 
summary(multinomial_fit)
match_data1$runs <- fmatches$runs
match_data1$deliveries_bowled <- fmatches$deliveries_bowled
match_data1$extras <- fmatches$extras
# getting the p-values
2*(1-pnorm(abs(summary(multinomial_fit)$coefficients/summary(multinomial_fit)$standard)))

# getting predictions
predtest  =  predict(multinomial_fit, newdata = testing_data_multinomial)  #  Obtain  model  predictions 
table(testing_data_multinomial$winner,predtest)

sum(diag(table(testing_data_multinomial$winner,predtest)))/sum(table(testing_data_multinomial$winner,predtest))

#tree with the full model
tree_fit1 <- tree(home_team_winner~.-city-team1-team2, data = training_data)
summary(tree_fit1)
plot(tree_fit1)
text(tree_fit1)

# pruned tree
tree.pruned <- prune.tree(tree_fit1,best=5)
summary(tree.pruned)
plot(tree.pruned)
text(tree.pruned,pretty=1)

# predictions from the tree
tree_preds <- predict(tree.pruned,newdata=testing_data)

# tree probabilities 
# will use to make classifications
tree_probs <- tree_preds[,2]

# classifications
tree_class <- ifelse(tree_preds[,2]>0.5,1,0)
table(testing_data$home_team_winner,tree_class,dnn=c("Actual","Predicted"))
acc_tree <- sum(tree_class==testing_data$home_team_winner)/nrow(testing_data)
acc_tree


# tree fit 2
tree_fit2 <- tree(home_team_toss_winner~.-team1-team2-toss_decision-city, data=training_data)

summary(tree_fit2)
plot(tree_fit2)
text(tree_fit2)

tree_predictions <- predict(tree_fit2,newdata=testing_data)

# tree probabilities 
# will use to make classifications
tree_probabilities <- tree_predictions[,2]

# classifications
tree_classifications <- ifelse(tree_predictions[,2]>0.5,1,0)
table(testing_data$home_team_winner,tree_classifications,dnn=c("Actual","Predicted"))
accuracy_tree <- sum(tree_classifications==testing_data$home_team_winner)/nrow(testing_data)
accuracy_tree

# pruned tree
tree.pruned2 <- prune.tree(tree_fit2,best=5)
summary(tree.pruned2)
plot(tree.pruned2)
text(tree.pruned2,pretty=1)

# predictions from the tree
tree_preds2 <- predict(tree.pruned2,newdata=testing_data)

# tree probabilities 
# will use to make classifications
tree_probs2 <- tree_preds2[,2]

# classifications
tree_class2 <- ifelse(tree_preds2[,2]>0.5,1,0)
table(testing_data$home_team_winner,tree_class2,dnn=c("Actual","Predicted"))
acc_tree2 <- sum(tree_class2==testing_data$home_team_winner)/nrow(testing_data)
acc_tree2

# lets run some knns as well
# will have to split the data into testing and validation
test_instn <- sample(num_obs, 0.67*num_obs)
ipl_test <- fmatches[-test_instn,]
## Save the rest of the data as the data that isn't testing
ipl_rest <- fmatches[test_instn,]

## Partition 30% of the remaining data as validation data
## Then save the remainder as training data
valid_instn <- sample(nrow(ipl_rest), 0.67*nrow(ipl_rest))
ipl_valid <- ipl_rest[-valid_instn,]
ipl_train <- ipl_rest[valid_instn,]

# The tree variables were
train.X <- ipl_train[,c(4,5,9,12,13)]
valid.X <- ipl_valid[,c(4,5,9,12,13)]
test.X <- ipl_test[,c(4,5,9,12,13)]

# response variables
train.winner <- as.numeric(ipl_train$home_team_winner)
valid.winner <- as.numeric(ipl_valid$home_team_winner)
test.winner <- as.numeric(ipl_test$home_team_winner)

# knns
knn.pred <- knn(train.X,valid.X,train.winner,k=10)

# table for predictions
table1 <- table(valid.winner,knn.pred,dnn=c("Actual","Predicted"))

# accuracy 
acc1 <- sum(table1[1,1]+table1[2,2])/sum(table1)

# Step Procedures
# Both full and intercept models
ipl_full <- glm(home_team_winner~., data = training_data, family = "binomial")
ipl_null <- glm(home_team_winner~1, data = training_data, family = "binomial")

# lets run the backward model
backward_ipl1 <- step(ipl_full, direction = "backward")
model_from_backward <- glm(home_team_winner ~ city + dl_applied + win_by_wickets + extras + 
                             runs + deliveries_bowled, data = training_data, family = "binomial")
summary(model_from_backward)

backward_model_predictions <- predict(model_from_backward, newdata = testing_data)
backward_model_classifications <- ifelse(backward_model_predictions > 0.5,1,0)
acc_backward <- sum(testing_data$home_team_winner==backward_model_classifications)/nrow(testing_data)

# forward model
forward_ipl1 <- step(ipl_null, scope=list(upper=ipl_full), direction = "forward")
model_from_forward <- glm(home_team_winner ~ city + deliveries_bowled + runs + dl_applied + 
                            win_by_wickets + extras, data = training_data, family = "binomial")
summary(model_from_forward)

forward_model_predictions <- predict(model_from_forward, newdata = testing_data)
forward_model_classfications <- ifelse(forward_model_predictions>0.5,1,0)
acc_forward <- sum(testing_data$home_team_winner==forward_model_classfications)/nrow(testing_data)


# Both forward and backward landed on the same model
# lets do both directions
forward_model_both <- step(ipl_null, scope=list(upper=ipl_full), direction="both", trace=0)
both_model_predictions <- predict(forward_model_both, newdata = testing_data)
both_model_classfications <- ifelse(both_model_predictions>0.5,1,0)
acc_both <- sum(testing_data$home_team_winner==both_model_classfications)/nrow(testing_data)
# All three landed up on the same model


# Random forests
bag.trees <- randomForest(home_team_winner~.-team1-team2-city, data = training_data,ntree=100,mtry=7,importance=TRUE)
bag.trees
importance(bag.trees)
varImpPlot(bag.trees)
bag_preds <- predict(bag.trees,newdata=testing_data,type="prob")
bag_probs <- bag_preds[,2]
bag_class <- ifelse(bag_probs>0.5,1,0)
plot(bag.trees)
table(testing_data$home_team_winner,bag_class)
sum(ifelse(bag_class==testing_data$home_team_winner,1,0))/nrow(testing_data)

# this ran 100 trees but if we leave it out then it will run 500
rf.trees <- randomForest(home_team_winner~.-team1-team2,data=training_data,importance=TRUE)
rf.trees
importance(rf.trees)
varImpPlot(rf.trees)

# lets do some predictions
## Then we get back to something that looked similar to what we had for trees before (see above)

rf_preds <- predict(rf.trees,newdata=testing_data,type="prob")
rf_probs <- rf_preds[,2]
rf_class <- ifelse(rf_probs>0.5,1,0)

table(testing_data$home_team_winner,rf_class)
sum(ifelse(rf_class==testing_data$home_team_winner,1,0))/nrow(testing_data)
plot(rf.trees)


# project #3
# predicting the score of the match given everything else
training_instances <- sample(num_obs, 0.67*num_obs)
match_data_training <- match_data1[training_instances,]
match_data_testing <- match_data1[-training_instances,]
runs_model <- lm(runs~.-team1-team2-venue-super_over-city,data=match_data_training)
linear_predictions <- predict(runs_model,newdata=match_data_testing)
