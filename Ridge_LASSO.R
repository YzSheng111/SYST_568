library("elasticnet")
library("pls")
library(mlbench) 
library( caret )
library(corrplot)
library(AppliedPredictiveModeling)
library(e1071) # misc library including skewness function



data = read.csv("train.csv", header = TRUE)




#Do NearZero Variance to remove unwanted predictors 

nzv <- nearZeroVar(data, saveMetrics= TRUE)
nzv_predictors <- subset(nzv, nzv == "TRUE")
pnames <- colnames(t(nzv_predictors[0]),do.NULL = FALSE)
pnames_bol <- names(data) %in% pnames
dataXXX <- data[!pnames_bol]

xxx <- dataXXX[,1:40]
yyy <- dataXXX[41]




# transofrm data to remove skewness using box cox 

dataXXXPP <- preProcess(xxx, method = "BoxCox")
dataXXXBC <- predict(dataXXXPP, xxx)
SkewValuesBC <- apply(dataXXXBC, 2, skewness )

#split data for all variables 
sample_size= floor(.70*nrow(dataXXXBC))
sample_size

set.seed(50)
ind= sample(seq_len(nrow(dataXXXBC)), size=sample_size)

trainXXXBC <- dataXXXBC[ind,]
trainYYYBC <- yyy[ind,]
testXXXBC <- dataXXXBC[-ind,]
testYYYBC <- yyy[-ind,]



# look for any multicolinearity - using the correlation matrix 
correlations <- cor(dataXXXBC)
highCorr <- findCorrelation(correlations, cutoff = .75) 
length(highCorr)
head(highCorr)
filteredCorData <- dataXXXBC[, -highCorr]


#split the model for filtered corr data 
filter_x <- filteredCorData[,1:35]

sample_size= floor(.70*nrow(filter_x))
sample_size

set.seed(50)
ind= sample(seq_len(nrow(filter_x)), size=sample_size)

trainXXX <- filter_x[ind,]
trainYYY <- yyy[ind,]
testXXX <- filter_x[-ind,]
testYYY <- yyy[-ind,]


# apply a linear model to the data with highly correlated predictors (all predictors)

ctrl <- trainControl(method = "cv", number = 10)
#Linear Regression Model 

set.seed(10)
lm_modelRaw = train(trainXXXBC, trainYYYBC, method="lm", preProcess=c("center","scale"), trControl = ctrl )
lm_modelRaw

lmPredRaw = predict(lm_modelRaw, testXXXBC)

# Evaluate the test performance using a caret function
lmValuesRaw = data.frame(obs = testYYYBC, pred = lmPredRaw)
defaultSummary(lmValuesRaw) # RMSE: 48619.851491 R2: 0.645442

lmImp <- varImp(lm_modelRaw, scale = FALSE)
plot(lmImp, top = 25, scales = list(y = list(cex = .95)))

# apply Ridge Regression to the data with highly correlated predictors (All predictors)
set.seed(10)
# lambda is the ridge regression penalty, enet works with matrix, converts the 
# data frame solTrainXtrans to a matrix. lambda = 0 performs lasso fit
lmRidgeRaw <- enet(x = as.matrix(trainXXXBC), y = trainYYYBC, lambda = 0.001)
RidgePred2 = predict(lmRidgeRaw , newx = as.matrix(testXXXBC), s=1, mode="fraction", type = "fit")

## test data 
RidgeValue2 = data.frame(obs = testYYYBC, pred = RidgePred2$fit)

defaultSummary(RidgeValue2)  # RMSE: 4.852184e+04 R2: 6.468478e-01


#resamp = resamples(list(lm=lm_modelRaw ,pls=pls_modelRaw,ridge=ridge_modelRaw))
#print( summary(resamp) )

#dotplot( resamp, metric="RMSE" )



# Apply Lasso Regression the the data with highly correlated predictors (All preidctors)

lmLassoRaw <- enet(x = as.matrix(trainXXXBC), y = trainYYYBC, lambda = 0)


# specify the lasso parameter in terms of the fraction of full solution, with s = 1, this is ridge regression
LassoPredRaw = predict(lmLassoRaw, newx = as.matrix(testXXXBC), s=1, mode="fraction", type = "fit")

## performance, try on test data 
LassoValueRaw = data.frame(obs = testYYYBC, pred = LassoPredRaw$fit)

defaultSummary(LassoValueRaw) # 48619.851972  R2: 0.645442
# From tuning: 0.000   0.70     RMSE:  30370.99 R2:  0.8631789  20984.83
LassoImp <- varImp(lmLassoRaw , scale = FALSE)
plot(LassoImp, top = 25, scales = list(y = list(cex = .95)))


# apply linear model after removing highly correlated variables 

set.seed(10)
lm_modelFilter = train(trainXXX, trainYYY, method="lm", preProcess=c("center","scale"), trControl = ctrl )
lm_modelFilter

lmPredFilter = predict(lm_modelFilter, testXXX)

# Evaluate the test performance using a caret function
lmValuesFilter= data.frame(obs = testYYY, pred = lmPredFilter)
defaultSummary(lmValuesFilter) # RMSE: 4.829949e+04 R2: 6.509745e-01

lmImp <- varImp(lm_modelFilter, scale = FALSE)
plot(lmImp, top = 25, scales = list(y = list(cex = .95)))




# apply Ridge Regression after removing highly correlated variables 
set.seed(10)
# lambda is the ridge regression penalty, enet works with matrix, converts the 
# data frame solTrainXtrans to a matrix. lambda = 0 performs lasso fit
lmRidgeFilter <- enet(x = as.matrix(trainXXX), y = trainYYY, lambda = 0.001)


# specify the lasso parameter in terms of the fraction of full solution, with s = 1, this is ridge regression
RidgePredFilter = predict(lmRidgeFilter , newx = as.matrix(testXXX), s=1, mode="fraction", type = "fit")

## performance, try on test data 
RidgeValueFilter = data.frame(obs = testYYY, pred = RidgePredFilter$fit)

defaultSummary(RidgeValueFilter)  # RMSE: 48228.85403  R2: 0.65193
RidgeImp <- varImp(lmRidgeFilter, scale = FALSE)
plot(RidgeImp, top = 25, scales = list(y = list(cex = .95)))


# apply Lasso regression after removing highly correlated variables

lmLassoFilter<- enet(x = as.matrix(trainXXX), y = trainYYY, lambda = 0)


# specify the lasso parameter in terms of the fraction of full solution, with s = 1, this is ridge regression
LassoFilterPredict = predict(lmLassoFilter, newx = as.matrix(testXXX), s=1, mode="fraction", type = "fit")

## performance, try on test data 
LassoValueFilter = data.frame(obs = testYYY, pred =LassoFilterPredict$fit)

defaultSummary(LassoValueFilter) # RMSE:4.829949e+04 R2: 6.509745e-01
# From tuning lambda = 0 : 0.000   0.90    RMSE:   30768.21 R2:  0.8589812  




#use tuning to find the best lambda for the ridge regression 




### Section 6.4 Penalized Models
install.packages("lars")
install.packages("elasticnet")
library("elasticnet")

# use enet
set.seed(10)
# lambda is the ridge regression penalty, enet works with matrix, converts the 
# data frame solTrainXtrans to a matrix. lambda = 0 performs lasso fit
lmRidge <- enet(x = as.matrix(solTrainXtrans), y = solTrainY, lambda = 0.001)

# Compute the solubility for the new test samples 
# specify the lasso parameter in terms of the fraction of full solution, with s = 1, this is ridge regression
RidgePred = predict(lmRidge, newx = as.matrix(solTestXtrans), s=1, mode="fraction", type = "fit")

## performance, try on test data (316)
RidgeValue = data.frame(obs = solTestY, pred = RidgePred$fit)

defaultSummary(RidgeValue) 

## Experiment with different penalty lambda
enetGrid <- expand.grid(lambda = c(0,0.001, 0.01, .1), fraction = seq(.05, 1, length = 20))
set.seed(100)
enetTune <- train(x = trainXXXBC, y = trainYYYBC,
                  method = "enet",
                  tuneGrid = enetGrid,
                  trControl = ctrl,
                  preProc = c("center", "scale"))
enetTune

## Figure 6.18
plot(enetTune)
