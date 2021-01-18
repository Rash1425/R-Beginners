#Library needed for the analysis
library(caret)
library(class)
library(e1071)

#Uploading the data
Intention<-read.csv('D:\\Data Mining\\online_shoppers_intention.csv')

#checking the shape
dim(Intention)
#understanding the data
head(Intention)
names(Intention)
str(Intention)
summary(Intention)

#checking for null value in each column
sapply(Intention,function(x) sum(is.na(x)))

#converting variables into factors
Intention$SpecialDay<-as.factor(Intention$SpecialDay)
Intention$OperatingSystems<-as.factor(Intention$OperatingSystems)
Intention$Browser<-as.factor(Intention$Browser)
Intention$Region<-as.factor(Intention$Region)
Intention$TrafficType<-as.factor(Intention$TrafficType)

#hotencoding
a<-dummyVars("~.",data=Intention)
Intention_encoded <- data.frame(predict(a, newdata = Intention))

#removing one of the hot encoded column of response variable
Intention_encoded$RevenueFALSE<-NULL

dim(Intention_encoded)
sapply(Intention_encoded,function(x) sum(is.na(x)))

#Dividing data into training and testing
set.seed(222)
Intention_Train<-createDataPartition(Intention_encoded$RevenueTRUE,p=0.70,list = FALSE)
#Intention_Train<-as.numeric(Train)
Intention_training<-(Intention_encoded[Intention_Train,])
Intention_testing<-(Intention_encoded[-Intention_Train,])
dim(Intention_training)
dim(Intention_testing)



#KNN classifier

KNNprediction_1 <- knn(train = Intention_training, test=Intention_testing, cl=Intention_training$RevenueTRUE ,k=1)
summary(KNNprediction_1)

confusionMatrix(as.factor(Intention_testing$RevenueTRUE),KNNprediction_1)

KNNprediction_2 <- knn(train = Intention_training, test=Intention_testing, cl=Intention_training$RevenueTRUE ,k=11)
summary(KNNprediction_2)
confusionMatrix(as.factor(Intention_testing$RevenueTRUE),KNNprediction_2)



# set.seed(400)
# ctrl <- trainControl(method="repeatedcv",repeats = 3)
# knnFit <- train(RevenueTRUE ~ ., data = Intention_training, method = "knn", trControl = ctrl, preProcess = c("center","scale"),tuneLength = 20)
# knnFit


##############
#SVM Model

SVM_model <- svm(formula = RevenueTRUE ~ ., data = Intention_training, type = 'C-classification', kernel = 'linear') 
SVM_model


Y_pred<- predict(SVM_model, Intention_testing)
Y_pred

summary(Y_pred)

#Confusion Matrix
confusionMatrix(as.factor(Y_pred),as.factor(Intention_testing$RevenueTRUE))

