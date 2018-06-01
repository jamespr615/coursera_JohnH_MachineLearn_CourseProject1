
list.of.packages <- c('caret','C50','gbm','glmnet')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")

suppressPackageStartupMessages( library(caret) )
suppressPackageStartupMessages( library(C50) )
suppressPackageStartupMessages( library(gbm) )
suppressPackageStartupMessages( library(glmnet) )

print('A')

Master_start_time <- Sys.time()
xtrainURL <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
xtrainfile<-'pml-training.csv'

if (!file.exists(xtrainfile)) {
    download.file(xtrainURL, destfile = xtrainfile)    
}

xtestURL<- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
xtestfile<-'pml-testing.csv'

if (!file.exists(xtestfile)) {
    download.file(xtestURL, destfile = xtestfile)    
}

print('B')


xtrainIn <- read.csv(xtrainfile, na.strings=c("NA","#DIV/0!", "") )
dim(xtrainIn)

xtestIn <- read.csv(xtestfile, na.strings=c("NA","#DIV/0!", "") )
dim(xtestIn)

nacols <- apply(xtrainIn, 2, function(x) any(is.na(x)))
dfcols <- as.data.frame(nacols)
dfcols$rn <- 1:length(dfcols$nacols)
dfcols <- dfcols[dfcols$nacols==FALSE,]
dfcols <- dfcols[-c(1:7),]
dfcols

all <- dfcols$rn
totalaccel <- c(11, 49, 102, 140, 160)
CORECORE <- c(8:10, 46:48, 84:86, 122:124, 160)
corewtaccel <- c(8:11, 46:49, 84:86,102, 122:124, 140, 160)
corewGyros <- c(8:10, 38,39, 46:48, 61,62, 84:86, 114,115, 122:124, 153,154, 160)

modelcols <- CORECORE

X <- xtrainIn[,modelcols]
Xtest <- xtestIn[,modelcols]

set.seed(9)
Train <- createDataPartition(X$classe, p=0.67, list=FALSE)

xt <- X[ Train, ]
dim(xt)
xtrain <- xt[,-ncol(xt)]
ytrain <- xt[,ncol(xt)]

xt <- X[ -Train, ]
dim(xt)
xcvalidation <- xt[,-ncol(xt)]
ycvalidation <- xt[,ncol(xt)]

# remove last column because it is project_id, not used
xtest <- Xtest[,-ncol(Xtest)]


Xall <- xtrainIn[,dfcols$rn]
xt <- Xall[ Train, ]
xtrainall <- xt[,-ncol(xt)]
ytrainall <- xt[,ncol(xt)]


xt <- Xall[ -Train, ]
xcvalidationall <- xt[,-ncol(xt)]
ycvalidationall <- xt[,ncol(xt)]

xtestall <- xtestIn[,dfcols$rn]
xtestall <- xtestall[,-ncol(xtestall)]

print('C')

MN <- vector(mode="character", length=10)
ISE <- vector(mode="integer", length=10)
ELT <- vector(mode="character", length=10)

print('C50')

start_time <- Sys.time()
set.seed(1)
grid <- expand.grid(.model = "tree", .trials = c(1:100), .winnow = FALSE)

tcc5 <- trainControl(method = "repeatedcv", number = 3, savePredictions = TRUE)

start_time <- Sys.time()
fitc5 <- train(x=xtrain,  y=ytrain, 
               method="C5.0", 
               tunegrid=grid,
               preProc = c("center", "scale"),
               trControl = tcc5)

predc5 <- predict(fitc5, newdata=xcvalidation)
cmc5 <- confusionMatrix(data=predc5, ycvalidation)
cmc5

accuracy <- table(predc5, ycvalidation)
ISE[1]<-sum(diag(accuracy))/sum(accuracy)

predc5test <- predict(fitc5, newdata=xtest)
dfresult <- as.data.frame(predc5test)

MN[1] <-'C5.0'
ELT[1] <- format(Sys.time() - start_time)

print('gbm')

start_time <- Sys.time()
set.seed(1)
tcgbm <- trainControl(method = "repeatedcv", number = 5, savePredictions = TRUE)

g<- capture.output( 
    fitgbm <- train(x=xtrain,  y=ytrain, 
                method='gbm',
                preProc = c("center", "scale"),
                trControl = tcgbm)
    )

predgbm <- predict(fitgbm, newdata=xcvalidation)
cmgbm <- confusionMatrix(data=predgbm, ycvalidation)
cmgbm

accuracy <- table(predgbm, ycvalidation)
ISE[2]<-sum(diag(accuracy))/sum(accuracy)

predgbmtest <- predict(fitgbm, newdata=xtest)
dfresult$predgbmtest <- predgbmtest

MN[2] <-'GBM'
ELT[2] <- format(Sys.time() - start_time)

print('knn')

start_time <- Sys.time()
set.seed(0)
tcknn <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

fitknn <- train(x=xtrain,  y=ytrain, 
                method="knn", 
                preProc = c("center", "scale"),
                trControl = tcknn, tuneLength = 1)

predknn <- predict(fitknn, newdata = xcvalidation)
cmknn <- confusionMatrix(data = predknn, ycvalidation)
cmknn

accuracy <- table(predknn, ycvalidation)
ISE[3]<-sum(diag(accuracy))/sum(accuracy)

predknntest <- predict(fitknn, newdata=xtest)
dfresult$predknntest <- predknntest

MN [3] <-'KNN'
ELT[3] <- format(Sys.time() - start_time)

print('knn.all')

start_time <- Sys.time()
set.seed(0)
tcknnall <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

fitknnall <- train(x=xtrainall,  y=ytrainall, 
                   method="knn", 
                   preProc = c("center", "scale"),
                   trControl = tcknnall, tuneLength = 1)

predknnall <- predict(fitknnall, newdata = xcvalidationall)
cmknnall <- confusionMatrix(data = predknnall, ycvalidationall)
cmknnall

accuracy <- table(predknnall, ycvalidationall)
ISE[4]<-sum(diag(accuracy))/sum(accuracy)

predknntestall <- predict(fitknnall, newdata=xtestall)
dfresult$predknntestall <- predknntestall

MN [4] <-'KNN.ALL'
ELT[4] <- format(Sys.time() - start_time)

print('lasso')

start_time <- Sys.time()

rctrl1 <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

set.seed(1)
fitlasso <- train(x=xtrain,  y=ytrain, 
                  method = "glmnet", 
                  preProc = c("center", "scale"),
                  trControl = rctrl1)

predlasso <- predict(fitlasso, newdata=xcvalidation)
cmlasso <- confusionMatrix(data=predlasso, ycvalidation)
cmlasso

accuracy <- table(predlasso, ycvalidation)
ISE[5]<-sum(diag(accuracy))/sum(accuracy)

predlassotest <- predict(fitlasso, newdata=xtest)
dfresult$predlassotest <- predlassotest

MN [5] <-'LASSO'
ELT[5] <- format(Sys.time() - start_time)

print('lda')

start_time <- Sys.time()
set.seed(0)
tclda <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

fitlda <- train(x=xtrain,  y=ytrain, 
                method="lda", 
                preProc = c("center", "scale"),
                trControl = tclda, 
                tuneLength = 5)

predlda <- predict(fitlda, newdata = xcvalidation )
cmlda <- confusionMatrix(data=predlda, ycvalidation)
cmlda

accuracy <- table(predlda, ycvalidation)
ISE[6]<-sum(diag(accuracy))/sum(accuracy)

predldatest <- predict(fitlda, newdata=xtest)
dfresult$predldatest <- predldatest

MN [6] <-'LDA'
ELT[6] <- format(Sys.time() - start_time)

print('lda.all')

start_time <- Sys.time()
set.seed(0)
tcldaall <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

fitldaall <- train(x=xtrain,  y=ytrain, 
                   method="lda", 
                   preProc = c("center", "scale"),
                   trControl = tcldaall, 
                   tuneLength = 10)

predldaall <- predict(fitldaall, newdata = xcvalidation )
cmldaall <- confusionMatrix(data=predldaall, ycvalidation)
cmldaall

accuracy <- table(predldaall, ycvalidation)
ISE[7]<-sum(diag(accuracy))/sum(accuracy)

predldaalltest <- predict(fitldaall, newdata=xtest)
dfresult$predldaalltest <- predldaalltest

MN [7] <-'LDA.ALL'
ELT[7] <- format(Sys.time() - start_time)


print('logitB')


start_time <- Sys.time()
set.seed(0)
tclb <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

fitlb <- train(x=xtrain,  y=ytrain,
               method="LogitBoost", 
               preProc = c("center", "scale"),
               trControl = tclb, 
               tuneLength = 9)

predlb <- predict(fitlb, newdata = xcvalidation )
cmlb <- confusionMatrix(data=predlb, ycvalidation)
cmlb

accuracy <- table(predlb, ycvalidation)
ISE[8]<-sum(diag(accuracy))/sum(accuracy)

predlbtest <- predict(fitlb, newdata=xtest)
dfresult$predlbtest <- predlbtest

MN [8] <-'LogitBoost'
ELT[8] <- format(Sys.time() - start_time)

print('RF')

start_time <- Sys.time()
set.seed(0)
tcrf <- trainControl(method = "repeatedcv", number = 3, savePredictions = TRUE)

fitrf <- train(x=xtrain,  y=ytrain, 
               method='rf',
               preProc = c("center", "scale"),
               tuneLength = 3,
               trControl = tcrf)

predrf <- predict(fitrf, newdata=xcvalidation)
cmrf <- confusionMatrix(data=predrf, ycvalidation)
cmrf

accuracy <- table(predrf, ycvalidation)
ISE[9]<-sum(diag(accuracy))/sum(accuracy)

predrftest <- predict(fitrf, newdata=xtest)
dfresult$predrftest <- predrftest

MN [9] <-'Random Forest'
ELT[9] <- format(Sys.time() - start_time)

print('p part')

start_time <- Sys.time()
set.seed(0)
tcrp <- trainControl(method = "repeatedcv", 
                     number = 5, 
                     savePredictions = TRUE)

fitrp <- train(x=xtrain,  y=ytrain,  
               method='rpart', 
               preProc = c("center", "scale"),
               tuneLength = 5,
               trControl = tcrp)


predrp <- predict(fitrp, newdata=xcvalidation)
cmrp <- confusionMatrix(data=predrp, ycvalidation)
cmrp

accuracy <- table(predrp, ycvalidation)
ISE[10]<-sum(diag(accuracy))/sum(accuracy)

predrptest <- predict(fitrp, newdata=xtest)
dfresult$predrptest <- predrptest

MN [10] <-'R Part'
ELT[10] <- format(Sys.time() - start_time)

print('5')


dfmodelsummary <-data.frame(
    'name' = MN,
    'accuracy' = round(ISE,3),
    'o.o.s.err' = round(1-ISE,3),
    'elapsed.time'= ELT)
dfmodelsummary<-dfmodelsummary[order(dfmodelsummary$accuracy, decreasing=T),]
dfmodelsummary

names(dfresult)<-MN
dfresult <- dfresult[, as.character(dfmodelsummary$name)] 
dfresult

dfmodelsummary$prediction <- apply(dfresult,2, function(x) paste(x,collapse=' '))
dfmodelsummary

total_time <- format(Sys.time() - Master_start_time )

## END

