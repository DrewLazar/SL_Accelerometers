library(C50)
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

#Function for computing boosting errors  
boost_tree = function(train, test, res_col, t){
  tree.train<-C5.0(train[,-res_col],train[,res_col], trials =t)
  pred<-predict(tree.train,test[,-res_col])
  1-mean(pred == test$COS.Intensity)
}

#Read visit 1 (AL.file.2A.txt) as training and visit 2 (AL.file.2B.txt) as test data for all locations.  
train_all <- read.table("AL.file.2A.txt",header=TRUE)
test_all <- read.table("AL.file.2B.txt",header=TRUE)
#Read data with just ank location. 
train_ank <- train_all[,c(1:24,97:101)]
test_ank <- test_all[,c(1:24,97:101)]

#loops here with four prediction classes - SED, LPA, MPA, VPA
#compute error for ank using tree method and then boost method and take difference
set.seed(98815)
a<-AL_trees(train_ank,test_ank)
b<-boost_tree(na.omit(train_ank),na.omit(test_ank),29,80)
xt_a<-c(a)
xt_db<-c(a-b)
#start loop. take random sample from training sets with same index. Compute tree errors and boost errors store differences.
for(i in 2:3){
  index <- sample(1:nrow(train_all), replace=TRUE) 
  train2a<-train_ank[index,] 
  a<-AL_trees(train2a,test_ank)
  b<-boost_tree(na.omit(train2a),na.omit(test_ank),29,80)
  xt_a[i]<-c(a)
  xt_db[i]<-c(a-b)
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
#Binary for train ank
train2_ank = mutate(train_ank, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_ank <- select(train2_ank,-COS.Intensity) 
colnames(train2_ank)[colnames(train2_ank)=="bin.int"] <- "COS.Intensity"
#Binary for test ank 
test2_ank = mutate(test_ank, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_ank <- select(test2_ank,-COS.Intensity)
colnames(test2_ank)[colnames(test2_ank)=="bin.int"] <- "COS.Intensity"
#End of create binary data 

#loops here with two prediction classes - nonactive, active 
#compute error for ank using tree method and then boost method and take difference
set.seed(98815)
a<-AL_trees(train2_ank,test2_ank)
b<-boost_tree(na.omit(train2_ank),na.omit(test2_ank),29,80)
xt2_a<-c(a)
xt2_db<-c(a-b)
#start loop. take random sample from training sets with same index. Compute tree errors and boost errors store differences.
for(i in 2:3){
  index <- sample(1:nrow(train2_all), replace=TRUE) 
  train2a<-train2_ank[index,] 
  a<-AL_trees(train2a,test2_ank)
  b<-boost_tree(na.omit(train2a),na.omit(test2_ank),29,80)
  xt2_a[i]<-c(a)
  xt2_db[i]<-c(a-b)
}
#end of loops for two classes

#xt_a errors for tree method for ank location, #xt_db differences for tree method against boost method for ank location (binary with a 2)
save(xt_a,xt_db,xt2_a,xt2_db,file = "ALankdiff1.RData")