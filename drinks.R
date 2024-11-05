library(tidyverse)
library(modelr)
drinks <- read_csv("C:/Users/NANA/Desktop/drinks.csv")
View(drinks)
str(drinks)
glimpse(drinks)
head(drinks)
#check for missing values
anyNA(drinks)
install.packages("maps")
library(maps)
install.packages("plotly")
library(plotly)
world_map <- map_data("world")
merged_data <- left_join(world_map,drinks,by=c("region"="country"))
gg_map <- ggplot(merged_data,aes(y=lat,x=long,group = group))+
  geom_polygon(aes(fill = total_litres_of_pure_alcohol),color="black")
interactive_map <- ggplotly(gg_map)
interactive_map
#average beer servings per country
ggplot(drinks,aes(x=country,y=beer_servings))+geom_bar(stat = "identity")+
  labs(title = "Beer servings per country",x="Country",y="Beer Servings")
#summary statistics
summary(drinks)
#relationship between beer_servings and total litres of alcohol
ggplot(drinks,aes(x=beer_servings,y=total_litres_of_pure_alcohol))+
  geom_point(color="green")+
  labs(title="relationship between beer servings and total litres of pure alcohol")
#linear model for the relationship
linear_model_beer_total_alcohol <- lm(total_litres_of_pure_alcohol~beer_servings,data = drinks)
summary(linear_model_beer_total_alcohol)
#getting the intercept
intercept <- coef(linear_model_beer_total_alcohol)[1]
intercept
#getting the slope
slope <- coef(linear_model_beer_total_alcohol)[2]
slope
#add predictions and plot
#new <- drinks %>% mutate(predicted=predict(linear_model_beer_total_alcohol))
#new
predicted_beer_vs_total <- add_predictions(drinks,linear_model_beer_total_alcohol)
ggplot(predicted_beer_vs_total,aes(x=pred,y=drinks$total_litres_of_pure_alcohol))+geom_point(color="blue")+
  geom_abline(intercept = 0,slope = 1,color="red",linetype="dashed")
#add residuals
residual_beer_vs_total <- add_residuals(drinks,linear_model_beer_total_alcohol)
#using freq to plot residuals
ggplot(residual_beer_vs_total,aes(x=resid))+geom_freqpoly(binwidth=0.5)
#using residual vs fitted vale plots
plot(linear_model_beer_total_alcohol,which = 1)
#using q-q plot
plot(linear_model_beer_total_alcohol,which = 2)
# TRAIN TEST SPLIT AND RUN ALL INDEPENDENT VARIABLES TO PREDICT TOTAL LITRES OF PURE ALCOHOL
install.packages("caret")
library(caret)
set.seed(123)
train_split <- createDataPartition(drinks$total_litres_of_pure_alcohol,p=0.8,list = FALSE)
train_data <- drinks[train_split, ]
test_data <-  drinks[-train_split, ]
colnames(drinks)
# for linear regression
lm_model <-  lm(total_litres_of_pure_alcohol~spirit_servings+beer_servings+wine_servings,data=train_data)
summary(lm_model)
lm_predictions <- predict(lm_model,newdata = test_data)
mse_lm <- mean((test_data$total_litres_of_pure_alcohol-lm_predictions)^2)
rmse_lm <- RMSE(lm_predictions,test_data$total_litres_of_pure_alcohol)
cat("The mean squared error for the linear regression model is", mse, "\n")
cat("The root mean squared error for the linear regression model is", rmse_lm, "\n")
#for decision tree 
install.packages("rpart")
library(rpart)
decision_tree_model <- rpart(total_litres_of_pure_alcohol~spirit_servings+beer_servings+wine_servings,data=train_data)
summary(decision_tree_model)
decision_tree_predictions <- predict(decision_tree_model,newdata = test_data)
mse_decision_tree <- mean((test_data$total_litres_of_pure_alcohol-decision_tree_predictions)^2)
rmse_decision_tree <- sqrt(mse_decision_tree)
cat("The mean squared error for the decision tree model is", mse_decision_tree, "\n")
cat("The root mean squared error for the decision tree model is", rmse_decision_tree, "\n")
# for random forest
install.packages("randomForest")
library(randomForest)
rf_model <- randomForest(total_litres_of_pure_alcohol~spirit_servings+beer_servings+wine_servings,data=train_data)
summary(rf_model)
rf_predictions <- predict(rf_model,newdata = test_data)
mse_random_forest <- mean((test_data$total_litres_of_pure_alcohol-rf_predictions)^2)
rmse_random_forest <- sqrt(mse_random_forest)
cat("The mean squared error for the random forest model is",mse_random_forest, "\n")
cat("The root mean squared error for the random forest model is", rmse_random_forest, "\n")
