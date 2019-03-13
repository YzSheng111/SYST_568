rm(list = ls())
#load the data 
cat("\014")  

data = read.csv("Processed.csv", header = TRUE)
#Check if Transformations to Resolve Outliers with spatial sign will improve the performance 
transformed <- spatialSign(data)
transformedData <- as.data.frame.matrix(transformed)
set.seed(502)

#Run for different Corr Value, used 0.9 and 0.8 
corrval = 0.9
filename = sprintf("Results-%.*f.csv", 1,corrval)
#Check for near zero vairance
#Remove predictors with near zero variance 
#170 Vars to Start
#Ended with 76
nzv <- nearZeroVar(data, saveMetrics= TRUE)
nzv_predictors <- subset(nzv, nzv == "TRUE")
pnames <- colnames(t(nzv_predictors[0]),do.NULL = FALSE)
pnames_bol <- names(data) %in% pnames
data <- data[!pnames_bol]
data <- droplevels(data)



#Remove the responce from the data set
responce <- data$SalePrice
responce_name <- c("SalePrice")
responce_name_bol <- names(data) %in% responce_name
data <- data[!responce_name_bol]
data <- droplevels(data)

#Sample Size
sample_size <- createDataPartition(responce, p = 0.7, list= FALSE)

predTrain <- data[ sample_size,]
predTest  <- data[-sample_size,]
respTrain <- responce[ sample_size]
respTest  <- responce[-sample_size]


#Center and Scale Training
segPPTrain <- preProcess(predTrain, c("center", "scale"))
segTrainTrans <- predict(segPPTrain, predTrain)

#Center and Scale Testing
segPPTest <- preProcess(predTest, c("center", "scale"))
segTestTrans <- predict(segPPTest, predTest)



#Check for Correlation
#Use different cut offs to see the impact on performance 
#75 predictors down to 66 for 0.9
tooHigh <- findCorrelation(cor(segTrainTrans), corrval)
trainXfiltered <- segTrainTrans[, -tooHigh]
testXfiltered  <-  segTestTrans[, -tooHigh]

set.seed(300)

#Preprocess
ctrl <- trainControl(method = "cv", number = 10)

#robust linear regresson
rlm <- train(x = trainXfiltered, y = respTrain, method = "rlm",trControl = ctrl,preProc = "pca")
summary(rlm)
testResultsrlm <- data.frame(obs = respTest, pred = predict(rlm, testXfiltered))
rlmSummary <- defaultSummary(testResultsrlm)

#robust linear regresson
lm <- train(x = trainXfiltered, y = respTrain, method = "lm",trControl = ctrl,preProc = "pca")
summary(lm)
testResultslm <- data.frame(obs = respTest, pred = predict(lm, testXfiltered))
lmSummary <- defaultSummary(testResultslm)



############
#Checking if Spatial sign will help
#Repeat above but using transformed data

set.seed(200)


#Check for near zero vairance
#Remove predictors with near zero variance 
#170 Vars to Start
#Ended with 76
nzv <- nearZeroVar(transformedData, saveMetrics= TRUE)
nzv_predictors <- subset(nzv, nzv == "TRUE")
pnames <- colnames(t(nzv_predictors[0]),do.NULL = FALSE)
pnames_bol <- names(transformedData) %in% pnames
transformedData <- transformedData[!pnames_bol]
transformedData <- droplevels(transformedData)



#Remove the responce from the transformed set
responce <- transformedData$SalePrice
responce_name <- c("SalePrice")
responce_name_bol <- names(transformedData) %in% responce_name
transformedData <- transformedData[!responce_name_bol]
transformedData <- droplevels(transformedData)

#Sample Size
sample_size <- createDataPartition(responce, p = 0.7, list= FALSE)

predTrain <- transformedData[ sample_size,]
predTest  <- transformedData[-sample_size,]
respTrain <- responce[ sample_size]
respTest  <- responce[-sample_size]


#Center and Scale Training
segPPTrain <- preProcess(predTrain, c("center", "scale"))
segTrainTrans <- predict(segPPTrain, predTrain)

#Center and Scale Testing
segPPTest <- preProcess(predTest, c("center", "scale"))
segTestTrans <- predict(segPPTest, predTest)



#Check for Correlation
#75 down to 73
tooHigh <- findCorrelation(cor(segTrainTrans), corrval)
trainXfilteredSpatial <- segTrainTrans[, -tooHigh]
testXfilteredSpatial  <-  segTestTrans[, -tooHigh]

set.seed(300)

#Preprocess
ctrl <- trainControl(method = "cv", number = 10)

#robust linear regresson
rlmSpatial <- train(x = trainXfilteredSpatial, y = respTrain, method = "rlm",trControl = ctrl,preProc = "pca")
summary(rlmSpatial)
testResultsrlmSpatial <- data.frame(obs = respTest, pred = predict(rlmSpatial, testXfilteredSpatial))

rlmSummarySpatial <- defaultSummary(testResultsrlmSpatial)

lmSpatial <- train(x = trainXfilteredSpatial, y = respTrain, method = "lm",trControl = ctrl,preProc = "pca")
summary(lmSpatial)
testResultslmSpatial <- data.frame(obs = respTest, pred = predict(lmSpatial, testXfilteredSpatial))

lmSummarySpatial <- defaultSummary(testResultslmSpatial)


Method <- c("lm","lm Trans","RLM", "RLM Trans")
RMSE <- c(lmSummary[[1]],lmSummarySpatial[[1]],rlmSummary[[1]],rlmSummarySpatial[[1]])
Rsquared <- c(lmSummary[[2]],lmSummarySpatial[[2]],rlmSummary[[2]],rlmSummarySpatial[[2]])
RemovedPreds <- c((dim(data)[2]-dim(trainXfiltered)[2]),dim(transformedData)[2]-dim(trainXfilteredSpatial)[2])
Results <- data.frame(Method,RMSE,Rsquared,RemovedPreds)
write.csv(Results,filename,row.names=FALSE)
