# converting the raintomorrow and raintoday to numeric by using below code for getting
# better results
# to convert numeric into binary for raintomorrow and rain today
library(tidyverse)
# Recode as numeric
WeatherAustralia = WeatherAustralia %>%
  mutate(
    RainTomorrow = case_when(
      RainTomorrow == "Yes" ~ 1,
      RainTomorrow == "No" ~ 0
    )
  )

# Recode  raintoday from numeric to binary
WeatherAustralia = WeatherAustralia %>%
  mutate(
    RainToday = case_when(
      RainToday == "Yes" ~ 1,
      RainToday == "No" ~ 0
    )
  )

# here rounding  decimal the values for better  performances
# to round figure the values 

WeatherAustralia$MinTemp <- round(WeatherAustralia$MinTemp)
WeatherAustralia$MaxTemp <- round(WeatherAustralia$MaxTemp)
WeatherAustralia$Rainfall <- round(WeatherAustralia$Rainfall)
WeatherAustralia$Evaporation <- round(WeatherAustralia$Evaporation)
WeatherAustralia$Sunshine <- round(WeatherAustralia$Sunshine)
WeatherAustralia$Pressure3pm <- round(WeatherAustralia$Pressure3pm)
WeatherAustralia$Pressure9am <- round(WeatherAustralia$Pressure9am)
WeatherAustralia$Temp3pm <- round(WeatherAustralia$Temp3pm)
WeatherAustralia$Temp9am <- round(WeatherAustralia$Temp9am)
View(WeatherAustralia)

# checking the mean and standard  deviation 
str(WeatherAustralia)
summary(WeatherAustralia)
mean(WeatherAustralia$WindGustSpeed)
sd(WeatherAustralia$WindGustSpeed)
mean(WeatherAustralia$MinTemp)
sd(WeatherAustralia$MinTemp)
mean(WeatherAustralia$WindSpeed9am)
sd(WeatherAustralia$WindSpeed9am)
mean(WeatherAustralia$WindSpeed3pm)
sd(WeatherAustralia$WindSpeed3pm)
mean(WeatherAustralia$Humidity9am)
sd(WeatherAustralia$Humidity9am)
mean(WeatherAustralia$Humidity3pm)
sd(WeatherAustralia$Humidity3pm)
mean(WeatherAustralia$Cloud9am)
sd(WeatherAustralia$Cloud9am)
mean(WeatherAustralia$Cloud3pm)
sd(WeatherAustralia$Cloud3pm)
mean(WeatherAustralia$Rainfall)
sd(WeatherAustralia$Rainfall)
mean(WeatherAustralia$RainToday)
sd(WeatherAustralia$RainToday)
mean(WeatherAustralia$RainTomorrow)
sd(WeatherAustralia$RainTomorrow)


#1. Stationwise plot of specific variables
#1. MaxTemp
library(ggplot2)
library(dplyr)
library(viridis)
library(hrbrthemes)
WeatherAustralia %>%
  ggplot( aes(x=WindSpeed9am ,y=Cloud9am, group=Location, fill=Location)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Winspeed9am Vs Cloud9am") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  facet_wrap(~ Location, scale="free_y")


#Aggregated

library(ggplot2)
library(dplyr)

# Keep only 3 names
don <- WeatherAustralia %>% 
  filter(Location %in% c("Sydney","WaggaWagga","	
Melbourne","Portland","Brisbane","Perth","AliceSprings","Darwin"))
don %>%
  ggplot( aes(x=Temp9am, y=Humidity9am, group=Location, color=Location)) +
  geom_line()

library(ggplot2)
library(dplyr)

# Keep only 3 names
don <- WeatherAustralia %>% 
  filter(Location %in% c("Sydney","WaggaWagga","	
Melbourne","Portland","Brisbane","Perth","AliceSprings","Darwin"))
don %>%
  ggplot( aes(x=WindSpeed9am ,y=Cloud9am, group=Location, color=Location)) +
  geom_line()

# 2.Overtime

library(ggplot2)
library(dplyr)

# Keep only 3 names
don <- WeatherAustralia %>% 
  filter(Location %in% c("Sydney"))
don %>%
  ggplot( aes(x=Date, y=MaxTemp, group=Location, color=Location)) +
  geom_line()


#4. correlation
cordata <- WeatherAustralia[,c("WindGustSpeed","MinTemp","WindSpeed9am","WindSpeed3pm","Humidity9am","Humidity3pm","Cloud9am","Cloud3pm","Rainfall","RainToday","RainTomorrow")]
library("PerformanceAnalytics")
chart.Correlation(cordata, histogram=TRUE, pch=19)

# to know the missed values types this command
print(WeatherAustralia$MinTemp)
print(WeatherAustralia)

# checking the correlations between the variables
#install.packages("ggpubr")
#install.packages("zip")
library("ggpubr")
library(ggplot2)
attach(WeatherAustralia)
ggscatter(WeatherAustralia, x = "RainTomorrow", y = "WindGustSpeed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "RainTomorrow", ylab = "WindGustSpeed")

ggscatter(WeatherAustralia, x = "RainTomorrow", y = "MinTemp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "RainTomorrow", ylab = "MinTemp")

ggscatter(WeatherAustralia, x = "RainTomorrow", y = "WindSpeed9am", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "RainTomorrow", ylab = "WindSpeed9am")

ggscatter(WeatherAustralia, x = "RainTomorrow", y = "Rainfall", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "RainTomorrow", ylab = "Rainfall")

ggscatter(WeatherAustralia, x = "RainTomorrow", y = "RainToday", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "RainTomorrow", ylab = "RainToday")
WeatherAustralia$RainTomorrow <- as.numeric(WeatherAustralia$RainTomorrow)


res1 <- cor.test(WeatherAustralia$RainTomorrow, WeatherAustralia$Evaporation, 
                method = "pearson")
res1

res2 <- cor.test(WeatherAustralia$RainTomorrow, WeatherAustralia$MaxTemp, 
                method = "pearson")
res2

res3 <- cor.test(WeatherAustralia$RainTomorrow, WeatherAustralia$Temp9am, 
                method = "pearson")
res3

res4 <- cor.test(WeatherAustralia$RainTomorrow, WeatherAustralia$Cloud3pm, 
                method = "pearson")
res4

res5 <- cor.test(WeatherAustralia$RainTomorrow, WeatherAustralia$WindGustSpeed, 
                method = "pearson")
res5
 report(res5)
res6 <- cor.test(WeatherAustralia$RainTomorrow, WeatherAustralia$Evaporation, 
                method = "pearson")
res6

res7 <- cor.test(WeatherAustralia$RainTomorrow, WeatherAustralia$WindSpeed9am, 
                method = "pearson")
res7

res8 <- cor.test(WeatherAustralia$RainTomorrow, WeatherAustralia$WindSpeed3pm, 
                method = "pearson")
res8

res9 <- cor.test(WeatherAustralia$RainTomorrow, WeatherAustralia$Humidity9am, 
                method = "pearson")
res9

res10 <- cor.test(WeatherAustralia$RainTomorrow, WeatherAustralia$Humidity3pm, 
                method = "pearson")
res10

res11 <- cor.test(WeatherAustralia$RainTomorrow, WeatherAustralia$Pressure3pm, 
                method = "pearson")
res11

res12 <- cor.test(WeatherAustralia$RainTomorrow, WeatherAustralia$Pressure9am, 
                method = "pearson")
res12

res13 <- cor.test(WeatherAustralia$RainTomorrow, WeatherAustralia$Cloud9am, 
                method = "pearson")
res13
res14 <- cor.test(WeatherAustralia$RainTomorrow, WeatherAustralia$Cloud3pm, 
                method = "pearson")
res14

res15 <- cor.test(WeatherAustralia$RainTomorrow, WeatherAustralia$Temp9am, 
                method = "pearson")
res15

res16 <- cor.test(WeatherAustralia$RainTomorrow, WeatherAustralia$Temp3pm, 
                method = "pearson")
res16


res17 <- cor.test(WeatherAustralia$RainTomorrow, WeatherAustralia$RainToday, 
                method = "pearson")
res17

res18 <- cor.test(WeatherAustralia$RainTomorrow, WeatherAustralia$Humidity9am, 
                method = "pearson")
res18
 # After the getting the correlations using report we can get the results +ve/-ve/0
#install.packages("remotes")
#remotes::install_github("easystats/report")
library("report") # Load the package every time you start R
report(res18)



# Creating a subset for further process
set.seed(123)
df2 <- subset(WeatherAustralia, select = c(3,5,8,9,10,11,12,15,16,19,20))
View(df2)

# Stepwise Regression Essentials in R
library(tidyverse)
library(caret)
library(leaps)

# using stepwise function stepAIC
library(MASS)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(RainTomorrow ~., data = df2,
                    method = "leapForward", 
                    tuneGrid = data.frame(nvmax = 1:9),
                    trControl = train.control
)
step.model$results
# to know which value is the best one comparing to the others
step.model$bestTune
# summarize the output
summary(step.model$finalModel)
# to access the best coefficient number values
coef(step.model$finalModel, 8)



# Nested models using anova()
# model 1: 1 IV (only weight)
mod1 <- lm(RainTomorrow ~ MinTemp, data = df2)

# Model 2: 2 IVs (weight AND clarity)
mod2 <- lm(RainTomorrow ~ MinTemp + Rainfall, data = df2)

# Model 3: 3 IVs (weight AND clarity AND color)
mod3 <- lm( RainTomorrow ~ MinTemp + Rainfall+ WindGustSpeed + WindSpeed9am + 
              WindSpeed3pm + Humidity9am + Humidity3pm + Cloud3pm + RainToday, data = df2)

# Model 4: 4 IVs (weight AND clarity AND color)
mod4 <- lm( RainTomorrow ~ WindSpeed9am + 
              WindSpeed3pm + Humidity9am + Humidity3pm + Cloud3pm + RainToday, data = df2)

# Model 5: 5 IVs (weight AND clarity AND color)
mod5 <- lm( RainTomorrow ~ WindSpeed9am + WindGustSpeed+ 
              WindSpeed3pm +  Humidity3pm + Cloud3pm + RainToday, data = df2)


# Compare model 2 to model 3
anova(mod2, mod3)
# Compare model 1 to model 3
anova(mod1,mod3)

# Compare model 1 to model 4
anova(mod1, mod4)
# Compare model 2 to model 4
anova(mod2, mod4)
# Compare model 3 to model 4
anova(mod3, mod4)


# splitting the data into 70 as train set and 30 as test set
library(caret)
intrain<-createDataPartition(y=df2$RainTomorrow,p=0.7,list=FALSE)
train_set<-df2[intrain,]
test_set<-df2[-intrain,]
view(train_set)
view(test_set)



# Logistic Regression Essentials in R
library(tidyverse)
library(caret)
# Fit the model multi logistic regression
model <- glm( RainTomorrow ~., data = train_set, family = binomial)
# Summarize the model
summary(model)
# Make predictions
# after performing the multi logistic we know the good predictors
# so we are creating new model with good predictors
model1 <- glm(RainTomorrow~ MinTemp + WindGustSpeed + WindSpeed9am +WindSpeed3pm
              + Humidity3pm+ Cloud3pm +RainToday, data = train_set, family = binomial)
probabilities <- predict(model1,test_set, type = "response")
plot(probabilities, type = "h")
# Predict the class of individuals
predicted.classes <- ifelse(probabilities > 0.5, "1", "0")
plot(as.factor(predicted.classes), type = "h")
# confusion matrix
cm_glm <- table(predicted.classes, test_set$RainTomorrow)
cm_glm
# Model accuracy tell about the percentage of results our result is 85%
acc_glm<-mean(predicted.classes == test_set$RainTomorrow)
acc_glm
plot(acc_glm, type = "h")



# KNN MODEL
##store it as data frame
knn_dataset <- data.frame(df2)
##create a random number equal 90% of total number of rows
random_number <- sample(1:nrow(knn_dataset),0.7 * nrow(knn_dataset))
##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }
##normalization function is created
knn_nor <- as.data.frame(lapply(knn_dataset[,c(1,2,3,4,5,6,7,8,9,10)], nor))
##training dataset extracted
knn_train <- knn_nor[random_number,]
##test dataset extracted
knn_test <- knn_nor[-random_number,]
##the 2nd column of training dataset because that is what we need to predict about testing dataset
##also convert ordered factor to normal factor
knn_target <- knn_dataset[random_number,11]
##the actual values of 2nd couln of testing dataset to compare it with values that will be predicted
##also convert ordered factor to normal factor
knn_test_target <- knn_dataset[-random_number,11]
#Find the number of observation
NROW(knn_target) # output for this is 6831
# square root of 6831 we get 82.65 so we will create two models(82 and 83) in knn 
##run knn function
library(class)
pr_83 <- knn(knn_train,knn_test,cl=knn_target,k=83)
pr_84 <- knn(knn_train,knn_test,cl=knn_target,k=84)
view(pr_84)
plot(as.factor(pr_84), type = "h")
##create the confusion matrix
tb_84 <- table(pr_84,knn_test_target)
tb_83 <- table(pr_83,knn_test_target)
tb_84
# error rate 
mean(pr_84 == knn_test_target)
##check the accuracy and it tells the accuracy is 84.76776 => 85
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tb_84) # 84.35792
accuracy(tb_83) # 84.35792
#Accuracy plot
i=1
k.optm=1
for(i in 1:84) 
{
  knn.mod<-knn(train = knn_train,test = knn_test,cl=knn_target,k=i)
   k.optm[i]<- 100*sum(knn_test_target==knn.mod)/NROW(knn_test_target)
  k=i
  cat(k,"=",k.optm[i],'\n')#to print accuracy
  
 }
plot(k.optm,type="b",xlab = "K-value",ylab = "Accuracy level")
#according the plot k value for 84 gives more accurate value of K 86.57787



# Discriminant anlaysis
# Split the data into training (80%) and test set (20%)
aw <- df2
set.seed(123)
training.samples <- aw$RainTomorrow %>%
  createDataPartition(p = 0.7, list = FALSE)
da_train <- aw[training.samples, ]
da_test <- aw[-training.samples, ]
# NOrmalizing the data
# Estimate preprocessing parameters
preproc.param <- da_train %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
da_train.transformed <- preproc.param %>% predict(da_train)
da_test.transformed <- preproc.param %>% predict(da_test)
# Regularized discriminant analysis
library(klaR)
# Fit the model
rda_model <- rda(RainTomorrow~., data = da_train.transformed)
rda_model
# Make predictions
rda_predictions <- predict(rda_model,da_test.transformed)
plot(rda_predictions$class)
# Model accuracy
acc_rda <-mean(rda_predictions$class == da_test.transformed$RainTomorrow) # 85%
acc_rda
plot(acc_rda, type = "h")



# Classification trees
#Loading libraries
library(rpart)
library(caret)
library(rpart.plot,quietly = TRUE)
library(rattle)
# analyzing the each variable
 table(df2$RainTomorrow,df2$MinTemp)
# Here we are analayzing which one is better to split variable to predict the output
 # so Rainfall is the best one to split 
 number.perfect.splits <- apply(X=df2[-1], MARGIN = 2, FUN = function(col){
   t <- table(df2$RainTomorrow,col)
   sum(t == 0)
 })

# Descending order of perfect splits
 order <- order(number.perfect.splits,decreasing = TRUE)
 number.perfect.splits <- number.perfect.splits[order]
# Plot graph
 par(mar=c(10,2,2,2))
 barplot(number.perfect.splits,
         main="Number of perfect splits vs feature",
         xlab="",ylab="Feature",las=2,col="wheat")
 
# taining and testing
#data splicing
set.seed(12345)
train <- sample(1:nrow(df2),size = ceiling(0.70*nrow(df2)),replace = FALSE)
# training set
tree_train <- df2[train,]
# test set
tree_test <- df2[-train,]

# building a classification model
# building the classification tree with rpart
tree <- rpart(RainTomorrow~.,
               data=tree_train,method = "class")
library("rpart.plot")
# Visualize the decision tree with rpart.plot
rpart.plot(tree, nn=TRUE)
 
#Testing the model
pred <- predict(object=tree,tree_test,type="class")
pred
plot(pred)
#Calculating accuracy
t <- table(tree_test$RainTomorrow,pred) 
t
# Accuracy  rate
acc_tree<- mean(tree_test$RainTomorrow == pred)
acc_tree
plot(acc_tree,type = "h")




# Random Forest
library(randomForest)
#data splicing
set.seed(12345)
master <- df2
forest_train <- sample(1:nrow(master),size = ceiling(0.70*nrow(master)),replace = FALSE)
# training set
rf_train <- master[forest_train,]
# test set
rf_test <- master[-forest_train,]
# here im converting into factor of my response variable
rf_train$RainTomorrow <- as.factor(rf_train$RainTomorrow)
rf_train
rf_test$RainTomorrow <- as.factor(rf_test$RainTomorrow)
# creating model using random forest
rf <- randomForest(RainTomorrow ~ .,data=rf_train, mtry =4, ntree=1000, importance= TRUE)
rf # as you can see while executing the this line we will get error rate 14.14% so the accuracy is 85.86
plot(rf)
# predicting the model
pred1 = predict(rf, newdata=rf_test, type = "class")
pred1
plot(pred1)
# confusion matrix
cm = table(rf_test[,11], pred1)
cm
# error rate
acc_rf <-mean(rf_test$RainTomorrow == pred1)
acc_rf
plot(acc_rf, type= "h")

# # Neural Network
# # install package
# 
# # load library
library(neuralnet)
 index <- df2
# # normalizing the data
 normalize1 <- function(x) {
   return ((x - min(x)) / (max(x) - min(x)))
 }
 maxmindf <- as.data.frame(lapply(index, normalize1))
 Neural <- sample(1:nrow(maxmindf),size = ceiling(0.70*nrow(maxmindf)),replace = FALSE)
# # training set
 nn_train <- maxmindf[Neural,]
# # test set
 nn_test <- maxmindf[-Neural,]
# 
# #Neural Network
 library(neuralnet)
# # creating model
 nn <- neuralnet(RainTomorrow ~., data=nn_train, hidden=c(2,1), linear.output=FALSE, threshold=0.01,stepmax = 1000000)
# #summary(nn)
# # generating errors, hidden layers values ....
 nn$result.matrix
 plot(nn)
# # testing accuracy of the model
 nn.results =compute(nn, nn_test)
 results <- data.frame(actual = nn_test$RainTomorrow, prediction = nn.results$net.result)
 results
 plot(results, type= "h")
# # confusion matrix
 roundedresults<-sapply(results,round,digits=0)
 roudedresultsdf=data.frame(roundedresults)
 attach(roundedresultsdf)
 table(actual,prediction)
# # error rate
 mean(actual ==  prediction)




# ROC curves
#install.packages("pROC")
library(pROC)
ROC_rda <- roc(da_test.transformed$RainTomorrow, (as.numeric(rda_predictions$class)))
ROC_rf <- roc(rf_test$RainTomorrow, (as.numeric(pred1)))
ROC_knn <- roc(knn_test_target, pr_84)
ROC_lr <- roc(test_set$RainTomorrow, (as.numeric(predicted.classes)))
ROC_nn <- roc(actual,prediction)
# Area Under Curve (AUC) for each ROC curve (higher -> better)
ROC_rf_auc <- auc(ROC_rf)
ROC_lr_auc <- auc(ROC_lr)
ROC_knn_auc <- auc(ROC_knn)
ROC_rda_auc <- auc(ROC_rda)
ROC_NN_AUC <- auc(ROC_nn)
# plotting the ROC
par(pty = "s")
plot(ROC_rf, col = "green", main = "ROC For Curves")
lines(ROC_lr, col = "red")
lines(ROC_rda, col ="black")
lines(ROC_knn, col= "orange")
lines(ROC_nn,col= "blue")

# print the confusion matrix of each model
paste("Confusion Martix % of random forest: ", table(rf_test$RainTomorrow,pred1))
paste("Confusion Matrix % of logistic regression: ", table(test_set$RainTomorrow,predicted.classes))
paste("Confusion Matrix % of KNN: ", table(pr_84, knn_test_target))
paste("Confusion Matrix % of RDA: ", table(da_test.transformed$RainTomorrow ,rda_predictions$class))
paste("Confusion Matrix % of Neural Network: ",table(actual,prediction))

# print the Accuracy of each model
paste("Accuracy % of random forest: ", mean(rf_test$RainTomorrow == pred1))
paste("Accuracy % of logistic regression: ", mean(test_set$RainTomorrow == predicted.classes))
paste("Accuracy % of KNN: ", mean(pr_84== knn_test_target))
paste("Accuracy % of RDA: ", mean(da_test.transformed$RainTomorrow == rda_predictions$class))
paste("Accuracy % of Neural Network: ",mean(actual==prediction))

# print the error rate of each model
paste("error rate % of random forest: ", mean(rf_test$RainTomorrow != pred1))
paste("error rate % of logistic regression: ", mean(test_set$RainTomorrow != predicted.classes))
paste("error rate % of KNN: ", mean(pr_84!= knn_test_target))
paste("error rate % of RDA: ", mean(da_test.transformed$RainTomorrow != rda_predictions$class))
paste("error rate % of Neural Network: ",mean(actual!=prediction))

# print the Brier Score of each model 
paste("Brier Score % of random forest: ", mean(((as.numeric(rf_test$RainTomorrow))-(as.numeric(pred1)))^2))
paste("Brier Score % of logistic regression: ", mean((test_set$RainTomorrow-(as.numeric(predicted.classes)))^2))
paste("Brier Score% of KNN: ", mean((knn_test_target-(as.numeric(pr_84)))^2))
paste("Brier Score% of RDA: ", mean((da_test.transformed$RainTomorrow-(as.numeric(rda_predictions$class)))^2))
paste("Accuracy % of Neural Network: ",mean((actual==prediction)^2))

paste("Area under curve of random forest: ", ROC_rf_auc)
paste("Area under curve of logistic regression: ", ROC_lr_auc)
paste("Area under curve of random forest: ", ROC_knn_auc)
paste("Area under curve of random forest: ", ROC_rda_auc)


# calculating AIC using MOdels
AIC(mod1) #10327.28
AIC(mod2)#9566.971
AIC(mod3)#6117.865
AIC(mod4)#6886.212
AIC(mod5)#6173.56

# hurrry their is no Rain Tomorrow #######

