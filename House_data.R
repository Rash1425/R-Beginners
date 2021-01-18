#uploding Data
case1<-read.csv("D:\\Data Mining\\kc_house_data.csv")

#exploring the data set
str(case1)
names(case1)
head(case1)


#Removing the column which are not required for analysis
case1$id<-NULL
case1$date<-NULL
case1$ zipcode <-NULL
case1$ long <-NULL
case1$lat<-NULL
case1$ zipcode <-NULL
names(case1)





# Creating a subset from data which contains all numeric and few categorical variable with two levels only,
# as linear regression tends to underperform with categorical variable with more than two level
House<-case1[c(1:5,10:12,15,16)]
#attach(case1)

str(House)
names(House)
#converting datatype to numeric

House $ bedrooms<-as.numeric(case1 $ bedrooms)
House $ sqft_living<-as.numeric(case1 $ sqft_living)
House $ grade<-as.numeric(case1 $ grade)
House $ sqft_above<-as.numeric(case1$sqft_above)
House $ sqft_basement<-as.numeric(case1 $ sqft_basement)
House $ sqft_living15<-as.numeric(House $ sqft_living15)
House $ sqft_lot15<-as.numeric(House $ sqft_lot15)

#Checking correlation between selected variables
cor(House)
House $ sqft_living15<- NULL
House $ sqft_lot15<- NULL

#Boxplots
boxplot(House $ price)
hist(log(House $ price))
hist(House $ sqft_living)
hist(House $ sqft_above)
hist(House $ sqft_basement)
#Data Partition
library(caret)
set.seed(333)
TrainHouse<-createDataPartition(House$price ,p=0.70,list = FALSE)
TrainHouse<-as.numeric(TrainHouse)
trainingHouse<-House[TrainHouse,]
testingHouse<-House[-TrainHouse,]

#checking the formation of training and testing data
dim(trainingHouse)
dim(testingHouse)
#table(trainingHouse$price)

#linear Regression Model

model_1<-(lm(log(price)~.-sqft_basement,data=trainingHouse))
model_1
summary(model_1)
library(car)
par(mfrow=c(2,2))
plot(model_1)


#step variable elimination

# model_2<-step(lm(log(price~.),data=trainingHouse),direction = "both")
# summary(model_2)
# par(mfrow=c(2,2))
# plot(model2)


#Prediction of  'Y'
testingHouse <- predict(model_1,testingHouse)
testingHouse$Original <- exp(testingHouse) # reverse log transformation
head(testingHouse$Original)
