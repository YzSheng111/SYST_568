install.packages("rsq")
install.packages("Amelia")
install.packages("tidyverse")
install.packages("caret")
install.packages("ggplot2")
install.packages("magrittr")
install.packages("earth")
install.packages("pls")
install.packages("gridExtra")
install.packages("e1071")

library(rsq)
library(plyr)
library(tidyverse)
library(caret)
library(ggplot2)
library(earth)
library(pls)
library(gridExtra)
library(e1071)

#Load Train and Test dataset
Train=read.csv("~/Desktop/SYST 568/project/housing_train.csv", stringsAsFactors = F)
Train
dim(Train)
str(Train)
Test=read.csv("~/Desktop/SYST 568/project/housing_test.csv", stringsAsFactors = F)
dim(Test)
str(Test)

# Combine two datasets and using NearZeroVar to preprocess missing values
Test$SalePrice <- NA
all <- rbind(Train, Test)
dim(all)
colnames(all)
summary(all$SalePrice)
nearVars=nearZeroVar(all)
all1=all[-nearVars,]
sapply(all1[,1:81], function(x) sum(is.na(x)))
missing = sort(apply(all1, 2, function(x) sum(is.na(x))),decreasing = T)
missing[missing>0]

#There are over 90% NAs in PoolQC, MiscFeature, Alley and Fence, just delete them
all2<- all1[,-c(7,73,74,75)]
dim(all2)

#data type in all2
str(all2)
colnames(all2)

#For categorical missing values, imputing "NA" or blank to "None"
#
x<-all2[c(3,9,23,24,25,30,31,32,33,35,42,53,55,57,58,60,63,64,75)]
colnames(x)
x[is.na(x)]<-"None"
x
all2[c(3,9,23,24,25,30,31,32,33,35,42,53,55,57,58,60,63,64,75)]<-x
all2

#For numerical data, imputing "NA"s or blanks as 0
y<-all2[c(4)]
colnames(y)
y[is.na(y)]<-0
y
all2[c(4)]<-y
all2

# few of missing values, just remove them
dim(all2)
navals=list()
for(i in 1:2898){
  navals[i]=sum(is.na(all2[i,]))
}
remove_navals=which(navals > 0)
all2=all2[-remove_navals,]
colSums(is.na(all2))

sum(is.na(all2))

### find numerical data, prepare for linear regression
class<-sapply(names(all2),function(x){class(all2[[x]])})
class
nclass = names(class[class != "character"])
numclass= all2[nclass]
numclass
str(numclass)

#linear model
lm1= lm(SalePrice~., data=numclass)
summary(lm1)
plot(lm1)


#devide into test and train data set
set.seed(2318)
train_test= sample(seq_len(nrow(numclass)),size=floor(0.8*nrow(all2)))
train = numclass[train_test, ]
test = numclass[-train_test, ]
ctrl <- trainControl(method = "repeatedcv", repeats = 5)
train = as.data.frame(lapply(train, function(x) if(class(x)=="character") {as.factor(x)} else {x}))
test = as.data.frame(lapply(test, function(x) if(class(x)=="character") {as.factor(x)} else {x}))

# choose some variavles which have strong correlation (10 variables)
#FullBath, YearBuilt, YearRemodAdd, TotalBsmtSF, X1stFlrSF, GarageCars, GarageArea, OverallQual, GrLivArea, TotRmsAbvGrd with SalePrice
predVars = train[c("FullBath","YearBuilt","YearRemodAdd","TotalBsmtSF","X1stFlrSF","GarageCars","GarageArea","OverallQual","GrLivArea","TotRmsAbvGrd","SalePrice")]
plot(predVars)
lm2 = lm(SalePrice~FullBath+YearBuilt+YearRemodAdd+TotalBsmtSF+X1stFlrSF+GarageCars+GarageArea+OverallQual+GrLivArea+TotRmsAbvGrd, data = train,method ="qr")
summary(lm2)
plot(lm2)

#test errors and accuracy of all variables 
predAll = predict(lm1,test)
test_predAll = cbind(test,round(predAll))
test_predAll
RMSEAll= RMSE(predAll, test$SalePrice)
RMSEAll
RsqureAll = rsq::rsq(lm1)
RsqureAll
MAEAll = MAE(predAll,test$SalePrice)
MAEAll

#all variables' RMSE, R2, MAE and SDs
ctrl1 <- trainControl(method = "cv", number = 15)
lmFit1 <- train(SalePrice ~ ., data = numclass, method = "lm", trControl = ctrl1)
lmFit1$results

#test errors and accuracy of 10 selected variables 
pred_10vars = predict(lm2,test,type = "response")
test_pred_10vars = cbind(test,round(pred_10vars))
test_pred_10vars
RMSE_10vars = RMSE(pred_10vars, test$SalePrice)
RMSE_10vars
Rsqure_10vars = rsq::rsq(lm2)
Rsqure_10vars
MAE_10vars = MAE(pred_10vars,test$SalePrice)
MAE_10vars

#10 varables' RMSE,R2, MAE and SDs
ten_vars<-train[c("FullBath","YearBuilt","YearRemodAdd","TotalBsmtSF","X1stFlrSF","GarageCars","GarageArea","OverallQual","GrLivArea","TotRmsAbvGrd","SalePrice")]
ctrl2 <- trainControl(method = "cv", number = 15)
lmFit2 <- train(SalePrice ~ ., data = ten_vars, method = "lm", trControl = ctrl2)
lmFit2$results

# PLS model
#x is SalePrice, y are other numerical variavles
y<-numclass[,38]
x<-numclass[,-38]

View(sapply(x, class))
x_train <- x[train_test,] 
x_test <- x[-train_test,]
y_train <- numclass[train_test,38] 
y_test <- numclass[-train_test,38]

options(scipen=999) 
PLSFit = plsr(y_train ~ ., data = x_train, scale = FALSE)
plspred = predict(PLSFit, x_test, ncomp = 2) 
plsvalue = data.frame(obs = y_test, pred = plspred[,,1])
test1 <- defaultSummary(plsvalue)
test1

options(scipen=999) 
PLSFit = plsr(y_train ~ ., data = x_train, scale = FALSE)
plspred = predict(PLSFit, x_test, ncomp = 12) 
plsvalue = data.frame(obs = y_test, pred = plspred[,,1])
test2 <- defaultSummary(plsvalue)
test2

PLSFit <- train(x=x_train, y=y_train, method = "pls", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 15)
PLSFit
PLSFit$results

PLSresamples<-PLSFit$results
PLSresamples$Model <- "PLS"


xyplot(RMSE ~ ncomp, 
       data = PLSresamples,
       aspect = 1.2,
       main="PLS",
       xlab = "# Components",
       ylab = "RMSE (Cross-Validation)",       
       col = c("blue ","red"),
       groups = Model,
       type = c("o", "g"))

plsImp <- varImp(PLSFit, scale = FALSE)
plot(plsImp, top = 25, scales = list(y = list(cex = .95)))

#PCR
options(scipen=999) 
PCRFit = pcr(y_train ~ ., data = x_train, scale = FALSE)
pcrpred = predict(PCRFit, x_test, ncomp = 2) 
pcrvalue = data.frame(obs = y_test, pred = pcrpred[,,1])
test3 <- defaultSummary(pcrvalue)
test3

options(scipen=999) 
PCRFit = pcr(y_train ~ ., data = x_train, scale = FALSE)
pcrpred = predict(PCRFit, x_test, ncomp = 7) 
pcrvalue = data.frame(obs = y_test, pred = pcrpred[,,1])
test4 <- defaultSummary(pcrvalue)
test4

PCRFit <- train(x=x_train, y=y_train, method = "pcr", trControl = ctrl, tuneLength = 15)
PCRFit
PCRFit$results

PCRresamples<-PCRFit$results
PCRresamples$Model <- "PCR"

xyplot(RMSE ~ ncomp, # Plot PLS component results on a chart
       data = PCRresamples,
       aspect = 1.2,
       main="PCR",
       xlab = "# Components",
       ylab = "RMSE (Cross-Validation)",       
       col = c("blue ","red"),
       groups = Model,
       type = c("o", "g"))

pcrImp <- varImp(PCRFit, scale = FALSE)
plot(pcrImp, top = 25, scales = list(y = list(cex = .95)))






           