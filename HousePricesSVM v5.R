library(corrplot)
library(grid)
library(AppliedPredictiveModeling)
library(caret)
library(e1071)
library(lattice)

setwd("E:/Grad/OR 568 - Predictive Analytics/R Code/Data/House Prices/")

train = read.csv("train.csv", stringsAsFactors = FALSE)
test = read.csv("test.csv", stringsAsFactors = FALSE)


##Recode quality factors as numeric
train$ExterCond <- as.numeric(recode(train$ExterCond, `Po` = "1", `Fa` = "2",`TA` = "3",`Gd` = "4",`Ex`="5"))
train$ExterQual <- as.numeric(recode(train$ExterQual, `Po` = "1", `Fa` = "2",`TA` = "3",`Gd` = "4",`Ex`="5"))
train$BsmtQual <- as.numeric(recode(train$BsmtQual, `Po` = "1", `Fa` = "2",`TA` = "3",`Gd` = "4",`Ex`="5"))
train$BsmtCond <- as.numeric(recode(train$BsmtCond, `Po` = "1", `Fa` = "2",`TA` = "3",`Gd` = "4",`Ex`="5"))
train$KitchenQual <- as.numeric(recode(train$KitchenQual, `Po` = "1", `Fa` = "2",`TA` = "3",`Gd` = "4",`Ex`="5"))
train$FireplaceQu <- as.numeric(recode(train$FireplaceQu, `Po` = "1", `Fa` = "2",`TA` = "3",`Gd` = "4",`Ex`="5"))
train$GarageQual <- as.numeric(recode(train$GarageQual, `Po` = "1", `Fa` = "2",`TA` = "3",`Gd` = "4",`Ex`="5"))
train$GarageCond <- as.numeric(recode(train$GarageCond, `Po` = "1", `Fa` = "2",`TA` = "3",`Gd` = "4",`Ex`="5"))
train$HeatingQC <- as.numeric(recode(train$HeatingQC, `Po` = "1", `Fa` = "2",`TA` = "3",`Gd` = "4",`Ex`="5"))

test$ExterCond <- as.numeric(recode(test$ExterCond, `Po` = "1", `Fa` = "2",`TA` = "3",`Gd` = "4",`Ex`="5"))
test$ExterQual <- as.numeric(recode(test$ExterQual, `Po` = "1", `Fa` = "2",`TA` = "3",`Gd` = "4",`Ex`="5"))
test$BsmtQual <- as.numeric(recode(test$BsmtQual, `Po` = "1", `Fa` = "2",`TA` = "3",`Gd` = "4",`Ex`="5"))
test$BsmtCond <- as.numeric(recode(test$BsmtCond, `Po` = "1", `Fa` = "2",`TA` = "3",`Gd` = "4",`Ex`="5"))
test$KitchenQual <- as.numeric(recode(test$KitchenQual, `Po` = "1", `Fa` = "2",`TA` = "3",`Gd` = "4",`Ex`="5"))
test$FireplaceQu <- as.numeric(recode(test$FireplaceQu, `Po` = "1", `Fa` = "2",`TA` = "3",`Gd` = "4",`Ex`="5"))
test$GarageQual <- as.numeric(recode(test$GarageQual, `Po` = "1", `Fa` = "2",`TA` = "3",`Gd` = "4",`Ex`="5"))
test$GarageCond <- as.numeric(recode(test$GarageCond, `Po` = "1", `Fa` = "2",`TA` = "3",`Gd` = "4",`Ex`="5"))
test$HeatingQC <- as.numeric(recode(test$HeatingQC, `Po` = "1", `Fa` = "2",`TA` = "3",`Gd` = "4",`Ex`="5"))

train$Alley[is.na(train$Alley)] = "None"
test$Alley[is.na(test$Alley)] = "None"

train$FireplaceQu[is.na(train$FireplaceQu)] = 0
test$FireplaceQu[is.na(test$FireplaceQu)] = 0

train$GarageType[is.na(train$GarageType)] = "None"
train$GarageFinish[is.na(train$GarageFinish)] = "None"
train$GarageQual[is.na(train$GarageQual)] = 0
train$GarageCond[is.na(train$GarageCond)] = 0

test$GarageCond[is.na(test$GarageCond)] = 0
test$GarageType[is.na(test$GarageType)] = "None"
test$GarageFinish[is.na(test$GarageFinish)] = "None"
test$GarageQual[is.na(test$GarageQual)] = 0

train$BsmtQual[is.na(train$BsmtQual)] = 0
train$BsmtCond[is.na(train$BsmtCond)] = 0
train$BsmtExposure[is.na(train$BsmtExposure)] = "None"
train$BsmtFinType1[is.na(train$BsmtFinType1)] = "None"
train$BsmtFinType2[is.na(train$BsmtFinType2)] = "None"

test$BsmtFinType2[is.na(test$BsmtFinType2)] = "None"
test$BsmtQual[is.na(test$BsmtQual)] = 0
test$BsmtCond[is.na(test$BsmtCond)] = 0
test$BsmtExposure[is.na(test$BsmtExposure)] = "None"
test$BsmtFinType1[is.na(test$BsmtFinType1)] = "None"

train$PoolQC[is.na(train$PoolQC)] = "No Pool"
test$PoolQC[is.na(test$PoolQC)] = "No Pool"

train$HeatingQC[is.na(train$HeatingQC)] = 0
test$HeatingQC[is.na(test$HeatingQC)] = 0

train$Fence[is.na(train$Fence)] = "No Fence"
test$Fence[is.na(test$Fence)] = "No Fence"

train$MiscFeature[is.na(train$MiscFeature)] = "No Feature"
test$MiscFeature[is.na(test$MiscFeature)] = "No Feature"

########################################################
#Replace NA values with the most common value from each of the following predictors

train$MasVnrType[is.na(train$MasVnrType)] = "None"
test$MasVnrType[is.na(test$MasVnrType)] = "None"

#replace missing values in Electrical column in training set with _
#"SBrkr, which is most frequently occuring electrical set-up

train$Electrical[is.na(train$Electrical)] = "SBrkr"
test$Electrical[is.na(test$Electrical)] = "SBrkr"

#replace missing values in MSZoning column in test set with RL (most frequently occuring)
train$MSZoning[is.na(train$MSZoning)] = "RL"
test$MSZoning[is.na(test$MSZoning)] = "RL"

#replace missing values in Utilities column in test set with AllPub (most frequently occuring)
train$Utilities[is.na(train$Utilities)] = "AllPub"
test$Utilities[is.na(test$Utilities)] = "AllPub"

#replace missing values in Exterior1st column in test set with VinylSd (most frequently occuring)
train$Exterior1st[is.na(train$Exterior1st)] = "VinylSd"
test$Exterior1st[is.na(test$Exterior1st)] = "VinylSd"

#replace missing values in Exterior2nd column in test set with VinylSd (most frequently occuring)
train$Exterior2nd[is.na(train$Exterior2nd)] = "VinylSd"
test$Exterior2nd[is.na(test$Exterior2nd)] = "VinylSd"

#replace missing value in KitchenQual column in test set with TA (most common)
train$KitchenQual[is.na(train$KitchenQual)] = 0
test$KitchenQual[is.na(test$KitchenQual)] = 0

#replace missing values in Functional column in test set with Min2 (most common)
train$Functional[is.na(train$Functional)] = "Min2"
test$Functional[is.na(test$Functional)] = "Min2"

#replace missing value in SaleType column in test set with WD (most common)
train$SaleType[is.na(train$SaleType)] = "WD"
test$SaleType[is.na(test$SaleType)] = "WD"

############################################################################

#replace missing value in BsmtFinSF1 column in test set with 0 (has no basement)
train$BsmtFinSF1[is.na(train$BsmtFinSF1)] = 0
test$BsmtFinSF1[is.na(test$BsmtFinSF1)] = 0

#replace missing value in BsmtFinSF2 column in test set with 0 (has no basement)
train$BsmtFinSF2[is.na(train$BsmtFinSF2)] = 0
test$BsmtFinSF2[is.na(test$BsmtFinSF2)] = 0

#replace missing value in BsmtUnfSF column in test set with 0 (has no basement)
train$BsmtUnfSF[is.na(train$BsmtUnfSF)] = 0
test$BsmtUnfSF[is.na(test$BsmtUnfSF)] = 0

#replace missing value in TotalBsmtSF column in test set with 0 (has no basement)
train$TotalBsmtSF[is.na(train$TotalBsmtSF)] = 0
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] = 0

#replace missing values in BsmtFullBath column in test set with 0 (has no basement)
train$BsmtFullBath[is.na(train$BsmtFullBath)] = 0
test$BsmtFullBath[is.na(test$BsmtFullBath)] = 0

#replace missing values in BsmtHalfBath column in test set with 0 (has no basement)
train$BsmtHalfBath[is.na(train$BsmtHalfBath)] = 0
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] = 0



###################################################################################

#replace missing value in GarageCars column in test set with 0 (no garage)
train$GarageCars[is.na(train$GarageCars)] = 0
test$GarageCars[is.na(test$GarageCars)] = 0

#replace missing value in GarageArea column in test set with 0 (no garage)
train$GarageArea[is.na(train$GarageArea)] = 0
test$GarageArea[is.na(test$GarageArea)] = 0

##################################################################################


#replace missing values in LotFrontage column in training and test sets with median
train$LotFrontage[is.na(train$LotFrontage)] = median(train$LotFrontage, na.rm = TRUE)
test$LotFrontage[is.na(test$LotFrontage)] = median(test$LotFrontage, na.rm = TRUE)

#replace missing values in GarageYrBlt column in training and test sets with 0
train$GarageYrBlt[is.na(train$GarageYrBlt)] = 0
test$GarageYrBlt[is.na(test$GarageYrBlt)] = 0

#replace missing values in MasVnrArea column in training and test sets with 0
train$MasVnrArea[is.na(train$MasVnrArea)] = 0
test$MasVnrArea[is.na(test$MasVnrArea)] = 0

#Elimnate zero variance predictors
VarsNearZero = nearZeroVar(train)
colnames(train[VarsNearZero])
train1 = train[,-VarsNearZero]
test1 = test[,-VarsNearZero]


train2<-train1
train2$SalePrice=log(train2$SalePrice)


##################################################################################

train2$LotArea = log(train2$LotArea)
test1$LotArea = log(test1$LotArea)

#Shift data for log transformation
train2$TotalBsmtSF = train2$TotalBsmtSF + 1
test1$TotalBsmtSF = test1$TotalBsmtSF + 1

train2$TotalBsmtSF = log(train2$TotalBsmtSF)
test1$TotalBsmtSF = log(test1$TotalBsmtSF)

#Shift data for log transformation
train2$BsmtFinSF1 = train2$BsmtFinSF1 + 1
test1$BsmtFinSF1 = test1$BsmtFinSF1 + 1

#Shift data for log transformation
train2$BsmtUnfSF = train2$BsmtUnfSF + 1
test1$BsmtUnfSF = test1$BsmtUnfSF + 1

train2$BsmtFinSF1 = log(train2$BsmtFinSF1)
test1$BsmtFinSF1 = log(test1$BsmtFinSF1)

train2$BsmtUnfSF = log(train2$BsmtUnfSF)
test1$BsmtUnfSF = log(test1$BsmtUnfSF)


##################################################################################


train2<-train2[,-match("Id",names(train2))]
test1<-test1[,-match("Id",names(test1))]

#Combining Variables which are likely correlated
# Exterior = train2$ExterQual+train2$ExterCond
# Interior = train2$BsmtQual+train2$KitchenQual
# Garage = train2$GarageQual+train2$GarageCond
# Bathrooms= train2$FullBath+0.5*train2$HalfBath
# 
# #Removing the single variables
# train2<-train2[,-match("ExterQual",names(train2))]
# train2<-train2[,-match("ExterCond",names(train2))]
# 
# train2<-train2[,-match("BsmtQual",names(train2))]
# train2<-train2[,-match("KitchenQual",names(train2))]
# train2<-train2[,-match("GarageQual",names(train2))]
# train2<-train2[,-match("GarageCond",names(train2))]
# 
# train2<-train2[,-match("FullBath",names(train2))]
# train2<-train2[,-match("HalfBath",names(train2))]
##################################################################################

control <- trainControl(method='cv', number=10)

set.seed(101)
grid <- expand.grid(C = c(0.001))
model_svm1 = train(SalePrice ~ .,
                  data = train2,
                  method = "svmLinear",
                  Metric = 'RMSE',
                  tuneGrid = grid,
                  trControl = control)

model_svm1

prediction = predict(model_svm1, test1)
#make a submission file
submit <- data.frame(Id = test$Id, SalePrice = exp(prediction))
write.csv(submit, file = "Submit1.csv", row.names = FALSE)


set.seed(101)
grid <- expand.grid(C = 0.005)
model_svm2 = train(SalePrice ~ ., 
                   data = train2,
                   preProc="scale",
                   method = "svmLinear",
                   Metric = 'RMSE',
                   tuneGrid = grid,
                   trControl = control)



model_svm2

prediction = predict(model_svm2, test1)
#make a submission file
submit <- data.frame(Id = test$Id, SalePrice = exp(prediction))
write.csv(submit, file = "Submit2.csv", row.names = FALSE)


set.seed(101)
grid <- expand.grid(C = 0.010)
model_svm3 = train(SalePrice ~ ., 
                   data = train2,
                   preProc="scale",
                   method = "svmLinear",
                   Metric = 'RMSE',
                   tuneGrid = grid,
                   trControl = control)

model_svm3

prediction = predict(model_svm3, test1)
#make a submission file
submit <- data.frame(Id = test$Id, SalePrice = exp(prediction))
write.csv(submit, file = "Submit3.csv", row.names = FALSE)


set.seed(101)
grid <- expand.grid(C = 0.015)
model_svm4 = train(SalePrice ~ ., 
                   data = train2,
                   preProc="scale",
                   method = "svmLinear",
                   Metric = 'RMSE',
                   tuneGrid = grid,
                   trControl = control)

model_svm4

prediction = predict(model_svm4, test1)
#make a submission file
submit <- data.frame(Id = test$Id, SalePrice = exp(prediction))
write.csv(submit, file = "Submit4.csv", row.names = FALSE)

set.seed(101)
grid <- expand.grid(C = 0.5)
model_svm5 = train(SalePrice ~ ., 
                   data = train2,
                   preProc="scale",
                   method = "svmLinear",
                   Metric = 'RMSE',
                   tuneGrid = grid,
                   trControl = control)

model_svm5

prediction = predict(model_svm5, test1)
#make a submission file
submit <- data.frame(Id = test$Id, SalePrice = exp(prediction))
write.csv(submit, file = "Submit5.csv", row.names = FALSE)


set.seed(101)
grid <- expand.grid(C = 0.10)
model_svm4 = train(SalePrice ~ ., 
                   data = train2,
                   preProc="scale",
                   method = "svmLinear",
                   Metric = 'RMSE',
                   tuneGrid = grid,
                   trControl = control)

model_svm4

prediction = predict(model_svm4, test1)
#make a submission file
submit <- data.frame(Id = test$Id, SalePrice = exp(prediction))
write.csv(submit, file = "Submit4.csv", row.names = FALSE)






