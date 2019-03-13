library(dplyr)
library(GGally)
library(caret)
library(ggplot2)
#install.packages('randomForest')
library(randomForest)



#read the data into a new file (houseprice_train and houseprice_test)
houseprice_train <- read.csv('C:/Users/Jahanzaib Talpur/Desktop/GMU/Semesters/2018_Fall/OR_568/Project_Data/train.csv')
houseprice_test <- read.csv('C:/Users/Jahanzaib Talpur/Desktop/GMU/Semesters/2018_Fall/OR_568/Project_Data/test.csv')
houseprice_test$SalePrice <- 0
houseprice_test$SalePrice <- as.integer(houseprice_test$SalePrice)
is.integer(houseprice_test$SalePrice)


#view the data
View(houseprice_train)

#summary of the dataset. Includes everything such as mean, median, class etc.
summary(houseprice_train)
summary(houseprice_test)
ncol(houseprice_train)

#create a new dataframe with only numeric variables
housepriceNumeric_train <- select_if(houseprice_train, is.numeric)
housepriceString_train <- select_if(houseprice_train, is.character)

#let's see if we have any NA values?
sum(is.na(housepriceNumeric_train))
sum(is.na(housepriceString_train))

#which columns have NA values?
columns_Numeric <- colnames(housepriceNumeric_train)[apply(is.na(housepriceNumeric_train), 2, any)]
columns_Numeric
columns_String <- colnames(housepriceString_train)[apply(is.na(housepriceString_train), 2, any)]
columns_String

#confirm that these are the columns? Then remove them
#259
sum(is.na(housepriceNumeric_train$LotFrontage))
unique(housepriceNumeric_train$LotFrontage)
housepriceNumeric_train$LotFrontage <- NULL
houseprice_train$LotFrontage <- NULL
#8
sum(is.na(housepriceNumeric_train$MasVnrArea))
unique(housepriceNumeric_train$MasVnrArea)
#81
sum(is.na(housepriceNumeric_train$GarageYrBlt))
unique(housepriceNumeric_train$GarageYrBlt)

#For String data
#1369
sum(is.na(housepriceString_train$Alley))
unique(housepriceString_train$Alley)
housepriceString_train$Alley <- NULL
houseprice_train$Alley <- NULL
#8
sum(is.na(housepriceString_train$MasVnrType))
unique(housepriceString_train$MasVnrType)

#37
sum(is.na(housepriceString_train$BsmtQual))
unique(housepriceString_train$BsmtQual)

#37
sum(is.na(housepriceString_train$BsmtCond))
unique(housepriceString_train$BsmtCond)

#38
sum(is.na(housepriceString_train$BsmtExposure))
unique(housepriceString_train$BsmtExposure)

#37
sum(is.na(housepriceString_train$BsmtFinType1))
unique(housepriceString_train$BsmtFinType1)

#38
sum(is.na(housepriceString_train$BsmtFinType2))
unique(housepriceString_train$BsmtFinType2)

#1
sum(is.na(housepriceString_train$Electrical))
unique(housepriceString_train$Electrical)


#690
sum(is.na(housepriceString_train$FireplaceQu))
unique(housepriceString_train$FireplaceQu)
housepriceString_train$FireplaceQu <- NULL
houseprice_train$FireplaceQu <- NULL
#81
sum(is.na(housepriceString_train$GarageType))
unique(housepriceString_train$GarageType)

#81
sum(is.na(housepriceString_train$GarageFinish))
unique(housepriceString_train$GarageFinish)

#81
sum(is.na(housepriceString_train$GarageQual))
unique(housepriceString_train$GarageQual)

#81
sum(is.na(housepriceString_train$GarageCond))
unique(housepriceString_train$GarageCond)

#1453
sum(is.na(housepriceString_train$PoolQC))
unique(housepriceString_train$PoolQC)
housepriceString_train$PoolQC <- NULL
houseprice_train$PoolQC <- NULL
#1179
sum(is.na(housepriceString_train$Fence))
unique(housepriceString_train$Fence)
housepriceString_train$Fence <- NULL
houseprice_train$Fence <- NULL
#1406
sum(is.na(housepriceString_train$MiscFeature))
unique(housepriceString_train$MiscFeature)
housepriceString_train$MiscFeature <- NULL
houseprice_train$MiscFeature <- NULL
#37
sum(is.na(housepriceString_train$BsmtQual))
unique(housepriceString_train$BsmtQual)



#Clean the data for numeric test dataset
houseprice_test$LotFrontage <- NULL
housepriceNumeric_test$LotFrontage <- NULL

sum(is.na(housepriceNumeric_test$MasVnrArea))
unique(housepriceNumeric_test$MasVnrArea)

sum(is.na(housepriceNumeric_test$BsmtFinSF1))
unique(housepriceNumeric_test$BsmtFinSF1)

sum(is.na(housepriceNumeric_test$BsmtFinSF2))
unique(housepriceNumeric_test$BsmtFinSF2)

sum(is.na(housepriceNumeric_test$BsmtUnfSF))
unique(housepriceNumeric_test$BsmtUnfSF)

sum(is.na(housepriceNumeric_test$TotalBsmtSF))
unique(housepriceNumeric_test$TotalBsmtSF)

sum(is.na(housepriceNumeric_test$BsmtFullBath))
unique(housepriceNumeric_test$BsmtFullBath)

sum(is.na(housepriceNumeric_test$BsmtHalfBath))
unique(housepriceNumeric_test$BsmtHalfBath)

sum(is.na(housepriceNumeric_test$GarageYrBlt))
unique(housepriceNumeric_test$GarageYrBlt)

sum(is.na(housepriceNumeric_test$GarageCars))
unique(housepriceNumeric_test$GarageCars)

sum(is.na(housepriceNumeric_test$GarageArea))
unique(housepriceNumeric_test$GarageArea)




#Clear the data for string test dataset
housepriceString_test$Alley <- NULL
houseprice_test$Alley <- NULL
housepriceString_test$FireplaceQu <- NULL
houseprice_test$FireplaceQu <- NULL
housepriceString_test$PoolQC <- NULL
houseprice_test$PoolQC <- NULL
housepriceString_test$Fence <- NULL
houseprice_test$Fence <- NULL
housepriceString_test$MiscFeature <- NULL
houseprice_test$MiscFeature <- NULL

sum(is.na(housepriceString_test$MSZoning))
unique(housepriceString_test$MSZoning)

sum(is.na(housepriceString_test$Utilities))
unique(housepriceString_test$Utilities)

sum(is.na(housepriceString_test$Exterior1st))
unique(housepriceString_test$Exterior1st)

sum(is.na(housepriceString_test$Exterior2nd))
unique(housepriceString_test$Exterior2nd)

sum(is.na(housepriceString_test$MasVnrType))
unique(housepriceString_test$MasVnrType)

sum(is.na(housepriceString_test$BsmtQual))
unique(housepriceString_test$BsmtQual)

sum(is.na(housepriceString_test$BsmtCond))
unique(housepriceString_test$BsmtCond)

sum(is.na(housepriceString_test$BsmtExposure))
unique(housepriceString_test$BsmtExposure)

sum(is.na(housepriceString_test$BsmtFinType1))
unique(housepriceString_test$BsmtFinType1)

sum(is.na(housepriceString_test$BsmtFinType2))
unique(housepriceString_test$BsmtFinType2)

sum(is.na(housepriceString_test$KitchenQual))
unique(housepriceString_test$KitchenQual)

sum(is.na(housepriceString_test$Functional))
unique(housepriceString_test$Functional)

sum(is.na(housepriceString_test$GarageType))
unique(housepriceString_test$GarageType)

sum(is.na(housepriceString_test$GarageFinish))
unique(housepriceString_test$GarageFinish)

sum(is.na(housepriceString_test$GarageQual))
unique(housepriceString_test$GarageQual)

sum(is.na(housepriceString_test$GarageCond))
unique(housepriceString_test$GarageCond)

sum(is.na(housepriceString_test$SaleType))
unique(housepriceString_test$SaleType)



#Remove the rows with less missing data
houseprice_train <- na.omit(houseprice_train)
houseprice_test <- na.omit(houseprice_test)


housepriceString_train <- as.data.frame(lapply(housepriceString_train, factor)) 



houseprice_train1 <- cbind(housepriceString_train, housepriceNumeric_train)




#Prediction
#Merge the train and test
houseprice <- rbind(houseprice_train, houseprice_test)
set.seed(555)

#remove NA values for feature selection
#housepriceNumeric_train <- na.omit(housepriceNumeric_train)
#sum(is.na(housepriceNumeric_train))
#housepriceString_train <- na.omit(housepriceString_train)
#sum(is.na(housepriceString_train))
#houseprice_train1 <- cbind(housepriceString_train, housepriceNumeric_train)



#Model building
model1 <- randomForest(SalePrice ~ ., data = houseprice[1:1338,], importance = TRUE)
model1

model2 <- randomForest(SalePrice ~ ., data = houseprice[1:1338,], ntree = 500, mtry = 6, importance = TRUE)
model2





#do the prediction
predTrain1 <- predict(model1, houseprice[1:1338,], type = "response")
plot(predTrain1)


predTest1 <- predict(model1, houseprice[1339:2657,], type = 'response')
plot(predTest1)

predTrain2 <- predict(model2, houseprice[1:1338,], type = "response")
table(predTrain2, houseprice_train1$SalePrice)
plot(predTrain2)

predTest2 <- predict(model2, houseprice[1339:2657,], type = 'response')
plot(predTest2)



#To Check important variables
importance(model1)
varImpPlot(model1)


importance(model2)       
varImpPlot(model2) 





#Model3
houseprice3 <- houseprice

houseprice3$Id <- NULL
houseprice3$PoolArea <- NULL
houseprice3$GarageQual <- NULL
houseprice3$GarageCond <- NULL
houseprice3$LowQualFinSF <- NULL
houseprice3$MiscVal <- NULL
houseprice3$YrSold <- NULL


#___
#Mean of squared residuals: 765604493
#% Var explained: 87.7
#_____
houseprice3$RoofMatl <-NULL
houseprice3$Electrical <- NULL
houseprice3$Heating <- NULL
houseprice3$Condition2 <- NULL
houseprice3$Street <- NULL
houseprice3$MoSold <- NULL
houseprice3$ExterCond <- NULL
#____
#Mean of squared residuals: 761999040
#% Var explained: 87.75





set.seed(0)
model3 <- randomForest(SalePrice ~ ., data = houseprice3[1:1338,], importance = TRUE)
model3

predTrain3 <- predict(model3, houseprice3[1:1338,], type = "response")
plot(predTrain3)

predTest3 <- predict(model3, houseprice3[1339:2657,], type = 'response')
plot(predTest3)

importance(model3)
varImpPlot(model3)
