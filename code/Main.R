options(warn = -1)
#Libraries
library(corrplot)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(plyr)
library(e1071)
library(neuralnet)
library(ipred)
library(kernlab)
library(klaR)
library(ada)
library(randomForest)
library(gbm)

LoadNprocess <- function() {
  #Loading data from data set URL - blood-transfusion dataset
  transfusion <-
    read.csv(
      url(
        "http://archive.ics.uci.edu/ml/machine-learning-databases/blood-transfusion/transfusion.data"
      )
    )
  
  names(transfusion) <-
    c(
      "Recency (Months)",
      "Frequency (times)",
      "Blood Donated (cc)",
      "Time (months)",
      "Donated blood"
    )
  Donated <- transfusion$`Donated blood`
  
  
  summary(transfusion)
  
  Correlation <- cor(transfusion)
  
  corrplot(cor(transfusion))
  #corrplot(Correlation, method="pie")
  
  #Pre-Processing and Scaling
  clean = !anyNA(transfusion)   #No NA values if Clean flag is True
  
  preProcParam <-
    preProcess(transfusion[1:4], method = c("center", "scale"))
  prtrain <- predict(preProcParam, transfusion[1:4])
  prtrain <- cbind(prtrain, Donated)
  
  
  summary(prtrain)
  
  #Sampling Train and Test Data
  set.seed(44)
  trainsamples <- sample(nrow(prtrain), 0.7 * nrow(prtrain))
  trainx <- data.frame(prtrain[trainsamples,])
  trainx
  testx <- data.frame(prtrain[-trainsamples,])
  
  train <- data.frame(trainx[1:2], trainx[4:5])
  test <- data.frame(testx[1:2], testx[4:5])
  
  totaldata = prtrain[-3]
  corrplot(cor(train))
  
  #Seperate X and Y
  x <- train[1:3]
  y <- factor(train$Donated)
  
  #Control for Cross Validation parameter is 10 i.e. K-10
  
  ctrl = trainControl(method = 'cv', number = 10)
  
  #Writing Formula as default formula doesnt work with neuralnet package
  n <- names(train)
  f <-
    as.formula(paste("Donated ~", paste(n[!n %in% "Donated"], collapse = " + ")))
  assign("f", f, envir = .GlobalEnv)
  assign("x", x, envir = .GlobalEnv)
  assign("y", y, envir = .GlobalEnv)
  assign("train", train, envir = .GlobalEnv)
  assign("test", test, envir = .GlobalEnv)
  assign("totaldata", totaldata, envir = .GlobalEnv)
  assign("ctrl", ctrl, envir = .GlobalEnv)
  
  
}


AllClassifiers <- function(train, test) {
  source("Classifiers.R")
  #Decision Tree Model
  decisionTree(train, test)
  # Perceptron
  
  #Neural Net
  neuralNetwork(train, test)
  #Deep Network
  DeepNetwork(train, test)
  #SVM
  SVM(train, test)
  # Logistic Regression
  logisticRegression(train, test)
  # Bagging
  baggingT(train, test)
  
  # Janu
  
  assign("prtrain", train, envir = .GlobalEnv)
  assign("prtest", test, envir = .GlobalEnv)
  assign("Donated", prtrain[4], envir = .GlobalEnv)
  
  
  accuracyperceptron <- perceptronfun(prtrain, prtest)
  accuracynaive <- naivefun(prtrain, prtest)
  accuracyknn <- knnfun(prtrain, prtest)
  accuracyrf <- rffun(prtrain, prtest)
  accuracyada <- adafun(prtrain, prtest)
  accuracygb <- gbfun(prtrain, prtest)
  
}

LoadNprocess()

AllClassifiers(train, test)

#All

accuracytree = data.frame(0, 0)
accuracyperceptron = data.frame(0, 0)
accuracyNeural = data.frame(0, 0)
accuracyDeep = data.frame(0, 0)
accuracySVM = data.frame(0, 0)
accuracynaive = data.frame(0, 0)
accuracylreg = data.frame(0, 0)
accuracyknn = data.frame(0, 0)
accuracyBag = data.frame(0, 0)
accuracyrf = data.frame(0, 0)
accuracyada = data.frame(0, 0)
accuracygb = data.frame(0, 0)
name <- c("Accuracy", "AUC")
colnames(accuracytree) <- name
colnames(accuracyperceptron) <- name
colnames(accuracyNeural) <- name
colnames(accuracyDeep) <- name
colnames(accuracySVM) <- name
colnames(accuracynaive) <- name
colnames(accuracylreg) <- name
colnames(accuracyknn) <- name
colnames(accuracyBag) <- name
colnames(accuracyrf) <- name
colnames(accuracyada) <- name
colnames(accuracygb) <- name

numofFolds <- 10
folds <-
  createFolds(
    totaldata$Donated,
    k = numofFolds,
    list = TRUE,
    returnTrain = FALSE
  )
for (i in 1:numofFolds) {
  trainingset <- totaldata[-folds[[i]], ]
  assign("train", trainingset, envir = .GlobalEnv)
  
  testset <- totaldata[folds[[i]], ]
  assign("test", testset, envir = .GlobalEnv)
  
  df <- data.frame(decisionTree(trainingset, testset))
  colnames(df) <- name
  accuracytree <- rbind(accuracytree, df)
  
  df <- data.frame(perceptronfun(trainingset, testset))
  colnames(df) <- name
  
  accuracyperceptron <- rbind(accuracyperceptron  , df)
  
  df <- data.frame(neuralNetwork(trainingset, testset))
  colnames(df) <- name
  
  accuracyNeural <- rbind(accuracyNeural      , df)
  
  df <- data.frame(DeepNetwork(trainingset, testset))
  colnames(df) <- name
  
  accuracyDeep <- rbind(accuracyDeep  , df)
  
  df <- data.frame(SVM(trainingset, testset))
  colnames(df) <- name
  
  accuracySVM <- rbind(accuracySVM   , df)
  
  df <- data.frame(naivefun(trainingset, testset))
  colnames(df) <- name
  
  accuracynaive <- rbind(accuracynaive       , df)
  
  df <- data.frame(logisticRegression(trainingset, testset))
  colnames(df) <- name
  
  accuracylreg <- rbind(accuracylreg  , df)
  
  df <- data.frame(knnfun(trainingset, testset))
  colnames(df) <- name
  
  accuracyknn <- rbind(accuracyknn   , df)
  
  df <- data.frame(baggingT(trainingset, testset))
  colnames(df) <- name
  
  accuracyBag <- rbind(accuracyBag   , df)
  
  df <- data.frame(rffun(trainingset, testset))
  colnames(df) <- name
  
  accuracyrf <- rbind(accuracyrf    , df)
  
  df <- data.frame(adafun(trainingset, testset))
  colnames(df) <- name
  
  accuracyada <- rbind(accuracyada   , df)
  
  df <- data.frame(gbfun(trainingset, testset))
  colnames(df) <- name
  
  accuracygb <- rbind(accuracygb , df)
  
}



#Average Means

sum(accuracytree$Accuracy) / numofFolds
sum(accuracyperceptron$Accuracy) / numofFolds
sum(accuracyNeural$Accuracy) / numofFolds
sum(accuracyDeep$Accuracy) / numofFolds
sum(accuracySVM$Accuracy) / numofFolds
sum(accuracynaive$Accuracy) / numofFolds
sum(accuracylreg$Accuracy) / numofFolds
sum(accuracyknn$Accuracy) / numofFolds
sum(accuracyBag$Accuracy) / numofFolds
sum(accuracyrf$Accuracy) / numofFolds
sum(accuracyada$Accuracy) / numofFolds
sum(accuracygb$Accuracy) / numofFolds


sum(accuracytree$AUC) / numofFolds
sum(accuracyperceptron$AUC) / numofFolds
sum(accuracyNeural$AUC) / numofFolds
sum(accuracyDeep$AUC) / numofFolds
sum(accuracySVM$AUC) / numofFolds
sum(accuracynaive$AUC) / numofFolds
sum(accuracylreg$AUC) / numofFolds
sum(accuracyknn$AUC) / numofFolds
sum(accuracyBag$AUC) / numofFolds
sum(accuracyrf$AUC) / numofFolds
sum(accuracyada$AUC) / numofFolds
sum(accuracygb$AUC) / numofFolds