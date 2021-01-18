# uploading data in R
df<-read.csv('D:\\Data Mining\\Accidents_categorical.csv',header = TRUE)
names(df)
head(df)
#Creating a dataframe for London cases
library(dplyr)
Accident_London<-filter(df,Region== 'London')
dim(Accident_London)
head(Accident_London)

#checking Null Values
sapply(Accident_London,function(x) sum(is.na(x)))

#checking each variables datatype
str(Accident_London)
#dropping variable not needed for analysis
Accident_London$Accident_Index<-NULL
Accident_London$Latitude<-NULL
Accident_London$Longitude<-NULL
Accident_London$Datetime<-NULL
#again checking dataframe
dim(Accident_London) #variables are removed from the dataframe
str(Accident_London)

#DataPartition
set.seed(111)
library(caret)
Train<-createDataPartition(Accident_London$Accident_Severity,p=0.70,list = FALSE)
#Train<-as.numeric(Train)
Accident_training<-(Accident_London[Train,])
Accident_testing<-(Accident_London[-Train,])
#
prop.table(table(Accident_training$Accident_Severity))

#Decision_Tree Model
library(rpart.plot)
#start_time <- Sys.time() #time needed for 

#Growing Tree fully
unpruned_DT<-rpart(Accident_Severity~., data=Accident_training, method="class",
                   minbucket=1, minsplit=1, control=rpart.control(cp=0.0001))
#unpruned_DT

#plotting the tree
plot(unpruned_DT)

#Confusion Matrix
confusionMatrix(data=Accident_testing$pred, reference=Accident_testing$Accident_Severity)

printcp(unpruned_DT)
plotcp(unpruned_DT)

######
#Postpruning


pruned_DT <- prune(unpruned_DT, cp=.00069843)

rpart.plot(pruned_DT)
# , main="Pruned - Road accident severity in UK",
#            extra=104, branch.lty=3, split.cex=1.2, type=3)

png("pruned DecisionTree.png", width=1920, height=1080, res=400)

#a<-rpart.plot(pruned_DT,box.palette = "RdBu",shadow.col = "grey",nn=TRUE)

Accident_testing$pred_prune <- predict(pruned_DT, Accident_testing, type="class")

confusionMatrix(data=Accident_testing$pred_prune, reference=Accident_testing$Accident_Severity)





#####################
#Random Forest

#installing library for random forest
library(randomForest)
model1 <- randomForest(Accident_Severity~., data = Accident_training, importance = TRUE)
model1




# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 6:11) {
  model3 <- randomForest(Accident_Severity~., data = Accident_training, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, Accident_testing, type = "class")
  a[i-2] = mean(predValid == Accident_testing$Accident_Severity)
}

a

plot(6:11,a)

# Fine tuning parameters of Random Forest model
model2 <- randomForest(Accident_Severity~., data = Accident_training, ntree = 500, mtry = 13, importance = TRUE)
model2

##predicting for testing data
Accident_testing$pred_rf <- predict(model1,Accident_testing, type="class")
confusionMatrix(data=Accident_testing$pred_rf, reference=Accident_testing$Accident_Severity)


