rm(list=ls())

#Load all csv files
#Many features from the imdb dataset was converted into categorical binary columns
Date<-read.csv("Date.csv",sep = ",",header = T)
Genre<-read.csv("Genre.csv",sep = ",",header = T)
Type<-read.csv("Type.csv",sep = ",",header = T)
Certificate<-read.csv("Certificate.csv",sep = ",",header = T)
Nudity<-read.csv("Nudity.csv",sep = ",",header = T)
Violence<-read.csv("Violence.csv",sep = ",",header = T)
Profanity<-read.csv("Profanity.csv",sep = ",",header = T)
Alcohol<-read.csv("Alcohol.csv",sep = ",",header = T)
Frightening<-read.csv("Frightening.csv",sep = ",",header = T)
imdb<-read.csv("imdb.csv",sep = ",",header = T)

#Convert the first column in all files to ID
Date$ï..Row.Labels
imdb$ï..ID
names(Date)[names(Date) == "ï..Row.Labels"]<-"ID"
names(Genre)[names(Genre) == "ï..Row.Labels"]<-"ID"
names(Type)[names(Type) == "ï..Row.Labels"]<-"ID"
names(Certificate)[names(Certificate) == "ï..Row.Labels"]<-"ID"
names(Nudity)[names(Nudity) == "ï..Row.Labels"]<-"ID"
names(Violence)[names(Violence) == "ï..Row.Labels"]<-"ID"
names(Profanity)[names(Profanity) == "ï..Row.Labels"]<-"ID"
names(Alcohol)[names(Alcohol) == "ï..Row.Labels"]<-"ID"
names(Frightening)[names(Frightening) == "ï..Row.Labels"]<-"ID"
names(imdb)[names(imdb) == "ï..ID"]<-"ID"


#Null Columns that are not being used for modeling that are in the imdb file
imdb2<-imdb
imdb2$Date<-NULL
imdb2$Name<-NULL
imdb2$Genre<-NULL
imdb2$Type<-NULL
imdb2$Certificate<-NULL
imdb2$Nudity<-NULL
imdb2$Violence<-NULL
imdb2$Profanity<-NULL
imdb2$Alcohol<-NULL
imdb2$Frightening<-NULL
imdb2$Episodes<-NULL


#Get rid of the rows that don't have a value
imdb2<-imdb2[!(imdb2$Rate=="No Rate" | imdb2$Votes=="No Votes"),]
imdb3<-imdb2[!(imdb2$Duration=="None"),]




#Merged dataframes together
library(dplyr)
model_df<-imdb3%>% left_join(Alcohol, by="ID")
model_df<-model_df%>% left_join(Certificate, by="ID")
model_df<-model_df%>% left_join(Frightening, by="ID")
model_df<-model_df%>% left_join(Nudity, by="ID")
model_df<-model_df%>% left_join(Profanity, by="ID")
model_df<-model_df%>% left_join(Type, by="ID")
model_df<-model_df%>% left_join(Violence, by="ID")

#Nulled the ID Column
model_df$ID<-NULL

#Converted all NA to 0
model_df[is.na(model_df)]<-0

#Made sure all columns were either numeric or categorical features
cols<-c(4:53)
model_df[cols]<-lapply(model_df[cols], factor)
model_df$Rate<-as.numeric(model_df$Rate)
model_df$Votes<-as.numeric(gsub(",","",model_df$Votes))
model_df$Duration<-as.numeric(model_df$Duration)
str(model_df)

#Linear Regression Model
#Training Set, Testing Set, Partition 70/30
library(caret)
inTrain_model_df<- createDataPartition(y = model_df$Rate, p = .70, list = FALSE)
training_model_df <- model_df[inTrain_model_df,]
testing_model_df<- model_df[-inTrain_model_df,]
logistic_rate<-lm(Rate ~ ., data = training_model_df, family = "binomial")
summary(logistic_rate)

#Backwards Elimination
#Made sure all the features that had a p-value above 0.05 were cut out
#since they are not statistically significant
model_df2<-model_df
model_df2<-model_df2[, -c(5,7,9,11:18,21,22,24:26,28,30,32,35,38,40:45,47,50,52)]


#Linear Regression Model
#Training Set, Testing Set, Partition 70/30
library(caret)
inTrain_model_df2<- createDataPartition(y = model_df2$Rate, p = .70, list = FALSE)
training_model_df2 <- model_df2[inTrain_model_df2,]
testing_model_df2<- model_df2[-inTrain_model_df2,]
logistic_rate2<-lm(Rate ~ ., data = training_model_df2, family = "binomial")
summary(logistic_rate2)

#Backwards Elimination
#Made sure all the features that had a p-value above 0.05 were cut out
#since they are not statistically significant
model_df3<-model_df2[, -c(4,5,10:14,16,18,19,22,23)]
library(caret)
inTrain_model_df3<- createDataPartition(y = model_df3$Rate, p = .70, list = FALSE)
training_model_df3 <- model_df3[inTrain_model_df3,]
testing_model_df3<- model_df3[-inTrain_model_df3,]
logistic_rate3<-lm(Rate ~ ., data = training_model_df3, family = "binomial")
summary(logistic_rate3)

#Predict rate based on testing model
Pred1 <- predict(logistic_rate3, testing_model_df3)
summary(logistic_rate3)

#Calculate prediction accuracy
actuals_preds <- data.frame(cbind(actuals=testing_model_df3$Rate, predicteds=Pred1))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)

#Calculate min-max accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  

#Calculate mean absolute percentage error
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  

#Save testing set to an Excel CSV file
#write.csv(testing_model_df3,"C:/Users/theRa/OneDrive/Documents/IMDB/Predicted.csv")

#Calculated the predicted rates in Excel and loaded the file back in R
Predicted<-read.csv("Predicted.csv",sep = ",",header = T)

#Calculate Mean Absolute Error
library(Metrics)
mae(actual = testing_model_df3$Rate, predicted = Predicted$Predicted)

#Calculate RMSE
sqrt(mean((testing_model_df3$Rate - Predicted$Predicted)^2))

#Calculate MSE
mean((actuals_preds$actuals - actuals_preds$predicteds)^2)



#Linear Regression Model
#Training Set, Testing Set, Partition 80/20
model_df4<-model_df3
library(caret)
inTrain_model_df4<- createDataPartition(y = model_df4$Rate, p = .80, list = FALSE)
training_model_df4 <- model_df4[inTrain_model_df4,]
testing_model_df4<- model_df4[-inTrain_model_df4,]
logistic_rate4<-lm(Rate ~ ., data = training_model_df4, family = "binomial")
summary(logistic_rate4)


#Predict rate based on testing model
Pred2 <- predict(logistic_rate4, testing_model_df4)
summary(logistic_rate4)

#Calculate prediction accuracy
actuals_preds2 <- data.frame(cbind(actuals=testing_model_df4$Rate, predicteds=Pred2))  # make actuals_predicteds dataframe.
correlation_accuracy2 <- cor(actuals_preds2)
head(actuals_preds2)

#Calculate min-max accuracy
min_max_accuracy2 <- mean(apply(actuals_preds2, 1, min) / apply(actuals_preds2, 1, max))  

#Calculate mean absolute percentage error
mape2 <- mean(abs((actuals_preds2$predicteds - actuals_preds2$actuals))/actuals_preds2$actuals)  

#Save testing set to an Excel CSV file
#write.csv(testing_model_df4,"C:/Users/theRa/OneDrive/Documents/IMDB/Predicted2.csv")

#Calculated the predicted rates in Excel and loaded the file back in R
Predicted2<-read.csv("Predicted2.csv",sep = ",",header = T)

#Calculate Mean Absolute Error
library(Metrics)
mae(actual = testing_model_df4$Rate, predicted = Predicted2$Predicted)

#Calculate RMSE
sqrt(mean((testing_model_df4$Rate - Predicted2$Predicted)^2))

#Calculate MSE
mean((actuals_preds2$actuals - actuals_preds2$predicteds)^2)




#Linear Regression Model
#Training Set, Testing Set, Partition 90/10
model_df5<-model_df4
library(caret)
inTrain_model_df5<- createDataPartition(y = model_df5$Rate, p = .90, list = FALSE)
training_model_df5 <- model_df5[inTrain_model_df5,]
testing_model_df5<- model_df5[-inTrain_model_df5,]
logistic_rate5<-lm(Rate ~ ., data = training_model_df5, family = "binomial")
summary(logistic_rate5)

#Predict rate based on testing model
Pred3 <- predict(logistic_rate5, testing_model_df5)
summary(logistic_rate5)

#Calculate prediction accuracy
actuals_preds3 <- data.frame(cbind(actuals=testing_model_df5$Rate, predicteds=Pred3))  # make actuals_predicteds dataframe.
correlation_accuracy3 <- cor(actuals_preds3)
head(actuals_preds3)

#Calculate min-max accuracy
min_max_accuracy3 <- mean(apply(actuals_preds3, 1, min) / apply(actuals_preds3, 1, max))  

#Calculate mean absolute percentage error
mape3 <- mean(abs((actuals_preds3$predicteds - actuals_preds3$actuals))/actuals_preds3$actuals)  

#Save testing set to an Excel CSV file
#write.csv(testing_model_df5,"C:/Users/theRa/OneDrive/Documents/IMDB/Predicted3.csv")

#Calculated the predicted rates in Excel and loaded the file back in R
Predicted3<-read.csv("Predicted3.csv",sep = ",",header = T)

#Calculate Mean Absolute Error
library(Metrics)
mae(actual = testing_model_df5$Rate, predicted = Predicted3$Predicted)

#Calculate RMSE
sqrt(mean((testing_model_df5$Rate - Predicted3$Predicted)^2))

#Calculate MSE
mean((actuals_preds3$actuals - actuals_preds3$predicteds)^2)