library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(e1071)
library(class)





setwd("F:/unversity/level4_subject/seecond_term/Distributed_computing/Assignment1/Titanic Disaster")
train_dataset<-read.table("train.csv",header = TRUE,sep = ",")
Test_dataset<-read.table("test.csv",header = TRUE,sep = ",")


#Preprocesing on Train_Data

#Removing NA Values and replace it with the average of the Age on Train dataset
train_age <- na.omit(train_dataset$Age)
train_age_avg <- mean(train_age)
train_dataset$Age[is.na(train_dataset$Age)] <- train_age_avg

# Change Age values to 0 = Adult(>=18), 1 = Child(<18) (label encoder)
train_dataset$Age <- ifelse(train_dataset$Age<18, 1, 0)
#train_dataset$Age = as.numeric(format(round(train_dataset$Age, 0)))

#Function to normalize the values
normalize <- function(x) {
  numerator <- x - min(x)
  denomrator <- max(x) - min(x)
  return (numerator/denomrator)
}

#Making normalization for Pclass
train_dataset$Pclass = normalize(train_dataset$Pclass)


#Removing NA Values and replace it with the average of the Fare on Test dataset
Test_Fare <- na.omit(Test_dataset$Fare)
Test_Fare_avg <- mean(Test_Fare)
Test_dataset$Fare[is.na(Test_dataset$Fare)] <- Test_Fare_avg



#Making normalization for Fare
test_length <- length(Test_dataset$Fare)
fare <- normalize(c(train_dataset$Fare, Test_dataset$Fare))
train_dataset$Fare <- fare[1:(length(fare)-test_length)]
Test_dataset$Fare <- fare[(length(fare)-test_length + 1): length(fare)]


# Change Sex to 0 = male, 1 = female (Making label encoder)
train_dataset$Sex  = factor(train_dataset$Sex , 
                            levels = c('male','female'), 
                            labels = c(0,1))

#Handling missing values in Take_Off column
train_dataset$take.off[train_dataset$take.off == ''] <- 'S'

#make label encoding for column Take_off

train_dataset$take.off  = factor(train_dataset$take.off , 
                            levels = c('C','Q','S'), 
                            labels = c(0,1,2))


#Remove coulmn Cabin from training_dataset
Train_Data <- subset(train_dataset, select = -c(Cabin,Ticket))
#-------------------------------------------------------------------------------------------------

#Preprocessing for Test_Data

#Removing NA Values and replace it with the average of the Age on Test dataset
Test_age <- na.omit(Test_dataset$Age)
test_average_age <- mean(Test_age)
Test_dataset$Age[is.na(Test_dataset$Age)] <- test_average_age



# Change Age values to 0 = Adult(>=18), 1 = Child(<18) (label encoder)

Test_dataset$Age <- ifelse(Test_dataset$Age<18, 1, 0)

#Test_data$Age = as.numeric(format(round(Test_data$Age, 0)))

#Make normalization for Pclass
Test_dataset$Pclass = normalize(Test_dataset$Pclass)


# Change Sex to 0 = male, 1 = female (Making label encoder)
Test_dataset$Sex  = factor(Test_dataset$Sex , 
                            levels = c('male','female'), 
                            labels = c(0,1))

#make label encoding for column Take_off

Test_dataset$take.off  = factor(Test_dataset$take.off , 
                                 levels = c('C','Q','S'), 
                                 labels = c(0,1,2))


#Remove coulmn Cabin from testing_dataset
Test_dataset <- subset(Test_dataset, select = -c(Cabin,Ticket))

#---------------------------------------------------------------------------------------------------



#Model (Decision Tree model)

#fit model on features

Dtree_model <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Age + Fare + take.off,
             method="class", data=Train_Data)
#Decision tree model plotting(splited feature is Sex) 
fancyRpartPlot(Dtree_model)

# making prediction
Prediction_of_model <- predict(Dtree_model, Test_dataset, type = "class")
#make Csv file with passenger_id and its assign(which passenger is survive or not)
DTree_csv <- data.frame(PassengerId = Test_dataset$PassengerId, Survived = Prediction_of_model)

#prediction result of decision tree
write.csv(DTree_csv, file = "Model_Result/Model_dtree_predict3.csv", row.names = FALSE)

#Accuracy of Decision tree =77.511%

#Model2(KNN)

survived <- Train_Data$Survived
passengers <- Test_dataset$PassengerId
train <- Train_Data[,-c(1,3,8,9,10)]
test <- Test_dataset[,-c(1,3,8,9)]
#Train and test have the same features and same dimensions
knn_titanic <- knn(train, test, survived, k = 5,  l = 0, prob = FALSE, use.all = TRUE)

KNN_CSV <- data.frame(PassengerId=passengers,Survived = knn_titanic)

write.csv(KNN_CSV,file = 'Model_Result/KnnResults2.csv', row.names = FALSE)

#ACcuraccy of Knn = 77.511%

#--------------------------------------------------------------------------------------------------

#Model3(Naive_bayes)

#Fitting Naive bayes on training data
Bayes_model<-naiveBayes(as.factor(Survived)~., Train_Data)

#Make prediction on testdata
BayesPrediction<-predict(Bayes_model, Test_dataset)

summary(BayesPrediction)
Naive_csv<-data.frame(Test_dataset$PassengerId, BayesPrediction)
#Rename Bayesprediction column to survived
colnames(Naive_csv)<-cbind("PassengerId","Survived")

write.csv(output, file = 'Model_Result/NaiveBayesRes2.csv', row.names = F)
#Naive bayes accuracy =74.641%
#---------------------------------------------------------------------------------------------------
#Model4(Svm)

install.packages('e1071')
library(e1071)

survived<- factor(survived,levels = c(0,1))



#Fitting SVM
SVM_classifier = svm(formula = Survived ~Pclass + Sex + SibSp + Parch + Age + Fare + take.off,
                 data = Train_Data,
                 type = 'C-classification',
                 kernel = 'linear')

Svm_prediction_model<-predict(SVM_classifier,Test_dataset )
Svm_dtframe<-data.frame(Test_dataset$PassengerId, Svm_prediction_model)
colnames(Svm_dtframe)<-cbind("PassengerId","Survived")
write.csv(Svm_dframe, file = 'Model_Result/SVMRes2.csv', row.names = F)

#Svm accuracy = 76.555%

