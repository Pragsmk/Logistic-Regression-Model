setwd("F:\\Datasets")
dm <- read.csv("DirectMarketing.csv")
install.packages("gains")
install.packages("caret")
install.packages("irr")
install.packages("e1071")
library(dplyr)
library(irr)
library(caret)
library(gains)
library(e1071)

#---------------------------Data Preparation-------------------------------

## Create a categorical variable Target for amount spent with 1 and 0 1 for high value and 0 for low value

dm%>%mutate(Target=ifelse(AmountSpent>mean(AmountSpent),1,0))->dm
dm$Target
names(dm)

# Remove Amountt spent

dm <- dm[,-10]

summary(dm)
dim(dm)

# Create a value called missing and replace na with missing
dm$History1<-ifelse(is.na(dm$History),"Missing",as.character(dm$History))

#converting it into a factor variable
dm$History1 <- as.factor(dm$History1)

summary(dm$History1)
class(dm$History1)


#convert children and catalog variables to factor variables
dm$Children <- as.factor(dm$Children)
dm$Catalogs <- as.factor(dm$Catalogs)

#remove history as it is a repetition
head(dm)

dm <- dm[,-8]
#----------------------------------------------------------------------

#Splitting into test and training samples

set.seed(200)
index<-sample(nrow(dm),0.70*nrow(dm),replace=F)
train<-dm[index,]
test<-dm[-index,]


head(train)
head(test)
dim(train)

#-----------------------------------------------------------------------
#---------------------------------MODELING------------------------------

#Building a Logistic regression model

#------------Build the first model using all the variables--------------

mod<-glm(train$Target~.,data=train[,-9],family="binomial")

summary(mod)


dm$Age
dm$Gender
names(dm)

#----------------------------------------------------------------------

#Build the second model using significant variables and dummy variables
#Creating dummy variables for both train and test dataset

train$AgeYoung_d<-ifelse(train$Age=="Young",1,0)

train$Hist.Mid_d<-ifelse(train$History1=="Medium",1,0)

train$Children2_d<-ifelse(train$Children=="2",1,0)

train$Children3_d<-ifelse(train$Children=="3",1,0)

test$AgeYoung_d <- ifelse(test$Age=="Young",1,0)

test$Hist.Mid_d <- ifelse(test$History1=="Medium",1,0)

test$Children2_d <- ifelse(test$Children=="2",1,0)

test$Children3_d <- ifelse(test$Children=="3",1,0)

#----------------Build the second model-------------------------------------

mod2 <- glm(Target~AgeYoung_d+Location+Salary+Children3_d+Children2_d+Catalogs+Hist.Mid_d,data=train,family="binomial")

summary(mod2)


#---------------------------Test data -------------------------------
#apply mod2 that is apply second model on test data


pred <- predict(mod2,type="response",newdata=test)

head(pred)
nrow(train)
nrow(test)

View(pred)

table(dm$Target)/nrow(dm)

pred_binary <- ifelse(pred>=0.399,1,0)

pred_binary <- as.factor(pred_binary)
test$Target<- as.factor(test$Target)

#------------Generate a confusionn matrix which will tell us 
#the actual no of 1's predicted as 1 and no of 0's predicted as 0's.

confusionMatrix(pred_binary,test$Target,positive = "1")

kappa2(data.frame(test$Target,pred_binary))

