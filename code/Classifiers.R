decisionTree <- function(train, test) {
  mytree <- rpart(train$Donated ~ ., data = train, method = "class")
  ptree <- prune(mytree, cp = 0.02)
  
  #Display Tree
  fancyRpartPlot(mytree)
  fancyRpartPlot(ptree)
  
  # Make predictions on the test set
  predict.dtree <- predict(mytree, newdata = test, type = "class")
  predict.dtree.prune <-
    predict(ptree, newdata = test, type = "class")
  
  #Use caret's confusion matrix
  conTree <- confusionMatrix(predict.dtree, test$Donated)
  conTreePrune <- confusionMatrix(predict.dtree.prune, test$Donated)
  
  
  #Accuracy and Confusion Matrix
  conTree
  
  #Area under ROC
  
  ROC.dtree <- roc(test$Donated, as.numeric(predict.dtree))
  plot(ROC.dtree,
       col = "blue",
       print.auc = TRUE,
       main = "Tree")
  auc.dtree <- auc(ROC.dtree)
  auc.dtree
  
  #Accuracy and Confusion Matrix
  conTreePrune
  
  #Area under ROC
  
  ROC.dtree.prune <-
    roc(test$Donated, as.numeric(predict.dtree.prune))
  plot(
    ROC.dtree.prune,
    col = "blue",
    print.auc = TRUE,
    main = "Pruned Tree"
  )
  auc.dtree.prune <- auc(ROC.dtree.prune)
  auc.dtree.prune
  print("1")
  accuracy <-
    (sum(diag(conTreePrune$table)) / sum(conTreePrune$table)) * 100.0
  
return(list(accuracy,as.numeric(auc.dtree.prune)))
}

neuralNetwork <- function(a,b) {
  set.seed(42)
  # nn <- train(as.factor(Don)~., data = train, method = "nnet",trControl=trainControl(method='cv',number=10))
  # nn2 <- neuralnet(Don~R+Fa+T, data=x, linear.output=FALSE)
  # nn3<-train(as.factor(Don)~R+Fa+B+T, data=train, method="nnet",linear.output=FALSE)
  # predict<- compute(nn, test)
  colnames(a)<-c("R","Fa","T","Don")
  n <- names(a)
  assign("n", n, envir = .GlobalEnv)
  
  f <-
    as.formula(paste("Don ~", paste(n[!n %in% "Don"], collapse = " + ")))
  assign("f", f, envir = .GlobalEnv)
  
  
  net <- neuralnet(f,data=a,
                   hidden = 6,
                   linear.output = FALSE)
  
  predict.nnet <- neuralnet::compute(net, b[1:3])
  
  #extracting 1's and zeros
  
  vals <- predict.nnet$net.result > 0.2
  predict.nnet.vals = ifelse(vals == "TRUE", 1, 0)
  
  #Use caret's confusion matrix
  
  conNeural <- confusionMatrix(predict.nnet.vals, b$Don)
  #Accuracy and Confusion Matrix
  
  conNeural
  
  #Area under ROC
  
  ROC.nnet <- roc(b$Don, as.numeric(predict.nnet.vals))
  plot(ROC.nnet,
       col = "red",
       print.auc = TRUE,
       main = "NeuralNet")
  auc.nnet <- auc(ROC.nnet)
  auc.nnet
  plot(net)
  print(2)
  accuracy <-
    (sum(diag(conNeural$table)) / sum(conNeural$table)) * 100.0
  return(list(accuracy,as.numeric(auc.nnet)))
  
  
}

DeepNetwork <- function(a, b) {
  #Writing Formula as default formula doesnt work with neuralnet package
  #Pass list for Deep network
  colnames(a)<-c("R","Fa","T","Don")
  n <- names(a)
  assign("n", n, envir = .GlobalEnv)
  
  f <-
    as.formula(paste("Don ~", paste(n[!n %in% "Don"], collapse = " + ")))
  assign("f", f, envir = .GlobalEnv)
  deep <-
    neuralnet(
      f,
      data = a,
      hidden = c(2, 2, 2),
      linear.output = FALSE
    )
  
  predict.deep <- compute(deep, b[1:3])
  
  #extracting 1's and zeros
  
  vals <- predict.deep$net.result > 0.2
  predict.deep.vals = ifelse(vals == "TRUE", 1, 0)
  
  #Use caret's confusion matrix
  
  con.deep <- confusionMatrix(predict.deep.vals, b$Donated)
  con.deep
  
  #Area under ROC
  
  ROC.deep <- roc(b$Donated, as.numeric(predict.deep.vals))
  plot(ROC.deep,
       col = "red",
       print.auc = TRUE,
       main = "Deep")
  auc.deep <- auc(ROC.deep)
  auc.deep
  plot(deep)
  print(3)
  accuracy <-
    (sum(diag(con.deep$table)) / sum(con.deep$table)) * 100.0
  return(list(accuracy,as.numeric(auc.deep)))
  
}


SVM <- function(train, test) {
  l.svm <- train(x, y, method = "svmLinear", trControl = ctrl)
  p.svm <- train(x, y, method = "svmRadial", trControl = ctrl)
  
  l.svm
  p.svm
  
  predict.svm <- predict(l.svm, newdata = test[1:3])
  predict.psvm <- predict(p.svm, newdata = test[1:3])
  
  #Use caret's confusion matrix
  
  conSVM <- confusionMatrix(predict.svm, test$Donated)
  conSVM
  conpSVM <- confusionMatrix(predict.psvm, test$Donated)
  conpSVM
  #Area under ROC
  
  ROC.svm <- roc(test$Donated, as.numeric(predict.svm))
  plot(ROC.svm,
       col = "blue",
       print.auc = TRUE,
       main = "Linear SVM")
  auc.svm <- auc(ROC.svm)
  auc.svm
  ROC.psvm <- roc(test$Donated, as.numeric(predict.psvm))
  plot(ROC.psvm,
       col = "red",
       print.auc = TRUE,
       main = "Radial SVM")
  auc.psvm <- auc(ROC.psvm)
  auc.psvm
  print(4)
  accuracy <-
    (sum(diag(conpSVM$table)) / sum(conpSVM$table)) * 100.0
  return(list(accuracy,as.numeric(auc.psvm)))
  
}

logisticRegression <- function(a, b) {
  
  x <- a[1:3]
  y <- factor(a$Donated)
  
  glm <- train(x, y, method = "glm", trControl = ctrl)
  
  predict.glm <- predict(glm, b[1:3])
  
  #Use caret's confusion matrix
  
  conGLM <- confusionMatrix(predict.glm, b$Donated)
  #Accuracy and Confusion Matrix
  
  conGLM
  
  #Area under ROC
  
  ROC.glm <- roc(b$Donated, as.numeric(predict.glm))
  plot(ROC.glm,
       col = "red",
       print.auc = TRUE,
       main = "Logistic Regression")
  auc.glm <- auc(ROC.glm)
  auc.glm
  print(5)
  accuracy <- (sum(diag(conGLM$table)) / sum(conGLM$table)) * 100.0
  return(list(accuracy,as.numeric(auc.glm)))
  
}

baggingT <- function(a, b) {
  x <- a[1:3]
  y <- factor(a$Donated)
  bag <-
    train(
      x,
      y,
      method = "treebag",
      trControl = ctrl,
      control = rpart.control(
        maxdepth = 15,
        cp = 0.0010000,
        minsplit = 30,
        xval = 5
      ),
      iter = 200
    )
  
  predict.bag <- predict(bag, b[1:3])
  
  #Use caret's confusion matrix
  
  conBAG <- confusionMatrix(predict.bag, b$Donated)
  #Accuracy and Confusion Matrix
  
  conBAG
  
  #Area under ROC
  
  ROC.bag <- roc(b$Donated, as.numeric(predict.bag))
  plot(ROC.bag,
       col = "red",
       print.auc = TRUE,
       main = "Bagging")
  auc.bag <- auc(ROC.bag)
  auc.bag
  print(6)
  accuracy <- (sum(diag(conBAG$table)) / sum(conBAG$table)) * 100.0
  return(list(accuracy,as.numeric(auc.bag)))
  
}

perceptronfun <- function(prtrain, prtest) {
  perceptron <- lm(Donated ~ ., data = prtrain)
  perceptron
  predicted_perceptron = ifelse(predict(perceptron, prtest) > 0.35, 1, 0)
  con <- confusionMatrix(predicted_perceptron, prtest$Donated)
  ROCp <- roc(prtest$Donated, predicted_perceptron)
  plot(ROCp, print.auc = TRUE, main = "Perceptron")
  aucp <- auc(ROCp)
  aucp
  plot(ROCp)
  accuracy <- (sum(diag(con$table)) / sum(con$table)) * 100.0
  return(list(accuracy,as.numeric(aucp)))
  
}

# naivemodel
naivefun <- function(prtrain, prtest) {
  naive = train(
    as.factor(Donated) ~ .,
    data = prtrain,
    'nb',
    trControl = trainControl(method = 'repeatedcv', number = 3),
    tuneGrid = data.frame(
      fL = 1,
      usekernel = TRUE,
      adjust = TRUE
    )
  )
  print(naive)
  predicted_naive = predict(naive, prtest)
  con <- confusionMatrix(predicted_naive, as.factor(prtest$Donated))
  ROCn <- roc(prtest$Donated, as.numeric(predicted_naive))
  plot(ROCn, print.auc = TRUE, main = "Naive Bayes")
  aucn <- auc(ROCn)
  aucn
  plot(ROCn)
  accuracy <- (sum(diag(con$table)) / sum(con$table)) * 100.0
  return(list(accuracy,as.numeric(aucn)))
  
}

#KNN
knnfun <- function(prtrain, prtest) {
  knn = train(
    as.factor(Donated) ~ .,
    data = prtrain,
    method = 'knn',
    trControl = trainControl(method = 'repeatedcv', number = 3),
    tuneGrid = data.frame(k = 10)
  )
  print(knn)
  predicted_knn = predict(knn, prtest)
  con <- confusionMatrix(predicted_knn, as.factor(prtest$Donated))
  ROCknn <- roc(prtest$Donated, as.numeric(predicted_knn))
  plot(ROCknn, print.auc = TRUE, main = "KNN")
  aucknn <- auc(ROCknn)
  aucknn
  accuracy <- (sum(diag(con$table)) / sum(con$table)) * 100.0
  return(list(accuracy,as.numeric(aucknn)))
  
}

#RF
rffun <- function(prtrain, prtest) {
  control <-
    trainControl(
      method = "repeatedcv",
      number = 10,
      repeats = 3,
      search = "random"
    )
  randomforest <-
    train(
      as.factor(Donated) ~ .,
      data = prtrain,
      method = "rf",
      trControl = control,
      ntree = 20
    )
  print(randomforest)
  predicted_randomforest = (predict(randomforest, prtest))
  con <-
    confusionMatrix(predicted_randomforest, as.factor(prtest$Donated))
  ROCrf <- roc(prtest$Donated, as.numeric(predicted_randomforest))
  aucrf <- auc(ROCrf)
  aucrf
  plot(ROCrf, print.auc = TRUE, main = "Random Forest")
  accuracy <- (sum(diag(con$table)) / sum(con$table)) * 100.0
  return(list(accuracy,as.numeric(aucrf)))
  
}

#Adaboost
adafun <- function(a, b) {
  colnames(a)<-c("R","Fa","T","Don")
  colnames(b)<-c("R","Fa","T","Don")
  x <- a[1:3]
  y <- factor(a$Don)
  adaModel <-
    ada(x,y,
        control = rpart.control(
          maxdepth = 30,
          cp = 0.0010000,
          minsplit = 20,
          xval = 10
        ),
        iter = 100
    )
  print(adaModel)
  predicted_ada = (predict(adaModel, b))
  con <- confusionMatrix(predicted_ada, b$Don)
  ROCada <- roc(b$Don, as.numeric(predicted_ada))
  aucada <- auc(ROCada)
  aucada
  plot(ROCada, print.auc = TRUE, main = "Ada Boosting")
  accuracy <- (sum(diag(con$table)) / sum(con$table)) * 100.0
  return(list(accuracy,as.numeric(aucada)))
  
}

#Gradient Boost
gbfun <- function(prtrain, prtest) {
  control <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 33)
  gradientboost <-
    train(
      as.factor(Donated) ~ .,
      data = prtrain,
      method = "gbm",
      trControl = control,
      verbose = FALSE,
      tuneGrid = data.frame(
        n.trees = 300,
        interaction.depth = 1,
        shrinkage = 0.1 ,
        n.minobsinnode = 10
      )
    )
  print(gradientboost)
  predicted_gb = predict(gradientboost, prtest)
  con <- confusionMatrix(predicted_gb, prtest$Donated)
  ROCgb <- roc(prtest$Donated, as.numeric(predicted_gb))
  aucgb <- auc(ROCgb)
  aucgb
  plot(ROCgb, print.auc = TRUE, main = "Gradient Boost")
  accuracy <- (sum(diag(con$table)) / sum(con$table)) * 100.0
  return(list(accuracy,as.numeric(aucgb)))
  
}
