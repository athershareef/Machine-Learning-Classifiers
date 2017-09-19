Language Used: R


R version 3.3.2 (2016-10-31) -- "Sincere Pumpkin Patch"
Copyright (C) 2016 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)
Tool Used: Eclipse Neon

Libraries required

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

Code is available in code folder

How to run the Code?
1.Set Working directory where the Main.R is available for ex: ..\Assignment 5\code\, also make sure to have all libraries listed above. Note: Main.R requires Classifiers.R as a source.
2.Navigate to Main.R and run the methods and library inclusion i.e from line 1 to 126
3.Run LoadNprocess() method later to load the data
4.Now run AllClassifiers(train, test) method to get the best result classifier.
5. Now Run the remaining code to get the 10 fold cross validation on all the classifiers.
 
Please find the detailed output results.txt

Summary:
	The best classifiers observed for our data set are K NN(Tuning parameter 'k' = 10, 3-fold, repeated 1 times) and Deep Network (Layers - 6)