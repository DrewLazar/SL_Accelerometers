#install.packages("tree")
library(randomForest)
library(tree)
library(tidyverse)
library(mosaic)
rm(list=ls())

#Function for computing tree errors
AL_trees = function(train, test){
  # Grow  tree for four class response on training dataset
  tree.AL.train=tree(COS.Intensity~.,train, model = T)
  # ALt the right sized tree from the the training data by pruning and cross valivation
  AL.cv.tree =cv.tree(tree.AL.train,FUN=prune.misclass)
  # The right size of the tree in terms of lowest misclassification error 
  AL.prune=prune.misclass(tree.AL.train, best = max(AL.cv.tree$size))
  # Class prediction from test data with predict()
  tree.pred.AL=predict(AL.prune,test,type="class")
  # Estimating misclassification error
  1-mean(tree.pred.AL == test$COS.Intensity)
} 

#Function for computing bagging errors
AL_bagging = function(train, test, b){
  # Bagging classifier with all available features (number of features are 52)
  AL.bag=randomForest(COS.Intensity~.,data=train,mtry=b,importance=TRUE)
  AL.predict.bag = predict(AL.bag,newdata=test)
  #error
  1-mean(AL.predict.bag ==test$COS.Intensity)
}

#Function for computing random forest errors
AL_rf = function(train, test, b){
  # Bagging classifier with all available features (number of features are 52)
  sb=sqrt(b)
  AL.bag=randomForest(COS.Intensity~.,data=train,mtry=sb,importance=TRUE)
  AL.predict.bag = predict(AL.bag,newdata=test)
  #error
  1-mean(AL.predict.bag ==test$COS.Intensity)
}

#Read visit 1 (AL.file.2A.txt) as training and visit 2 (AL.file.2B.txt) as test data for all locations. Correct data error for age in training set. 
train_all <- read.table("AL.file.2A.txt",header=TRUE)
index <- train_all$Wt == 94.8
train_all$Age[index] <- 29 
test_all <- read.table("AL.file.2B.txt",header=TRUE)
#Read data with just ank_hip locations
train_ank_hip <- train_all[,c(1:24,25:48,97:101)]
test_ank_hip <- test_all[,c(1:24,25:48,97:101)]

#loops here with four prediction classes - SED, LPA, MPA, VPA
#compute error for ank_hip using tree method, then boost method, then rf method and take differences
set.seed(98815)
a<-AL_trees(train_ank_hip,test_ank_hip)
c<-AL_bagging(na.omit(train_ank_hip),na.omit(test_ank_hip),52)
d<-AL_rf(na.omit(train_ank_hip),na.omit(test_ank_hip),52)
xt_a<-c(a)
xt_dc<-c(a-c)
xt_dd<-c(a-d)
#start loop. take random sample from training sets with same index. Compute tree errors and boost errors store differences.
for(i in 2:1000){
  index <- sample(1:nrow(train_all), replace=TRUE) 
  train2a<-train_ank_hip[index,] 
  a<-AL_trees(train2a,test_ank_hip)
  c<-AL_bagging(na.omit(train2a),na.omit(test_ank_hip),52)
  d<-AL_rf(na.omit(train2a),na.omit(test_ank_hip),52)
  xt_a[i]<-c(a)
  xt_dc[i]<-c(a-c)
  xt_dd[i]<-c(a-d)
}
#end of loops for four classes 

#Create binary data - with classes active = {MPA,VPA} and nonactive = {SED,LPA}
#Binary for training
train2_all = mutate(train_all, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_all <- select(train2_all,-COS.Intensity) 
colnames(train2_all)[colnames(train2_all)=="bin.int"] <- "COS.Intensity"
#Binary for test 
test2_all = mutate(test_all, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_all <- select(test2_all,-COS.Intensity)
colnames(test2_all)[colnames(test2_all)=="bin.int"] <- "COS.Intensity"
#Binary for train ank_hip
train2_ank_hip = mutate(train_ank_hip, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_ank_hip <- select(train2_ank_hip,-COS.Intensity) 
colnames(train2_ank_hip)[colnames(train2_ank_hip)=="bin.int"] <- "COS.Intensity"
#Binary for test ank_hip
test2_ank_hip = mutate(test_ank_hip, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_ank_hip <- select(test2_ank_hip,-COS.Intensity)
colnames(test2_ank_hip)[colnames(test2_ank_hip)=="bin.int"] <- "COS.Intensity"
#End of create binary data 

#loops here with two prediction classes - nonactive, active 
#compute error for ank_hip using tree method, then boost method, then rf method and take differences
set.seed(98815)
a<-AL_trees(train2_ank_hip,test2_ank_hip)
c<-AL_bagging(na.omit(train2_ank_hip),na.omit(test2_ank_hip),52)
d<-AL_rf(na.omit(train2_ank_hip),na.omit(test2_ank_hip),52)
xt2_a<-c(a)
xt2_dc<-c(a-c)
xt2_dd<-c(a-d)
#start loop. take random sample from training sets with same index. Compute tree errors, boost errors, rf errors and store differences.
for(i in 2:1000){
  index <- sample(1:nrow(train2_all), replace=TRUE) 
  train2a<-train2_ank_hip[index,] 
  a<-AL_trees(train2a,test2_ank_hip)
  c<-AL_bagging(na.omit(train2a),na.omit(test2_ank_hip),52)
  d<-AL_rf(na.omit(train2a),na.omit(test2_ank_hip),52)
  xt2_a[i]<-c(a)
  xt2_dc[i]<-c(a-c)
  xt2_dd[i]<-c(a-d)
}
#end of loops for two classes

#xt_a errors for tree method for ank_hip locations, #xt_dc differences for tree method against bagging method for ank_hip locations, 
#xt_dd differences for tree method against rf method for ank_hip location (binary with a 2)
save(xt_a,xt_dc,xt_dd,xt2_a,xt2_dc,xt2_dd,file = "ALank_hipdiff1a.RData")