#install.packages("tree")
library(tree)
library(tidyverse)
library(mosaic)
rm(list=ls())

#Read visit 1 (AL.file.2A.txt) as training and visit 2 (AL.file.2B.txt) as test data for all locations. 
train_all <- read.table("AL.file.2A.txt",header=TRUE)
test_all <- read.table("AL.file.2B.txt",header=TRUE)
#Read data with just lw_rw_hip and lw_rw_ank locations
train_ank <-train_all[,-c(1:24)]
test_ank <-test_all[,-c(1:24)]
train_hip<- train_all[,-c(25:48)]
test_hip <- test_all[,-c(25:48)]

#Function for computing tree errors
GE_trees = function(train, test){
  # Grow  tree for four class response on training dataset
  tree.GE.train=tree(COS.Intensity~.,train, model = T)
  # Get the right sized tree from the the training data by pruning and cross valivation
  GE.cv.tree =cv.tree(tree.GE.train,FUN=prune.misclass)
  # The right size of the tree in terms of lowest misclassification error 
  GE.prune=prune.misclass(tree.GE.train, best = max(GE.cv.tree$size))
  # Class prediction from test data with predict()
  tree.pred.GE=predict(GE.prune,test,type="class")
  # Estimating misclassification error
  1-mean(tree.pred.GE == test$COS.Intensity)
} 

#tree loops here with four prediction classes - SED, LPA, MPA, VPA
#compute error for all locations and just lw_rw_hip and lw_rw_ank and take differences
a<-GE_trees(train_all,test_all)
b<-GE_trees(train_ank,test_ank)
c<-GE_trees(train_hip,test_hip)
xt_a<-c(a)
xt_db<-c(a-b)
xt_dc<-c(a-c)
#start loop. take random sample from training sets with same index. Compute bagging errors and store differences. 
for(i in 2:1200){
  index <- sample(1:nrow(train_all), replace=TRUE) 
  train2a<-train_all[index,] 
  train2b<-train_ank[index,] 
  train2c<-train_hip[index,]
  a<-GE_trees(train2a,test_all)
  b<-GE_trees(train2b,test_ank)
  c<-GE_trees(train2c,test_hip)
  xt_a[i]<-a
  xt_db[i]<-c(a-b)
  xt_dc[i]<-c(a-c)
}
#end of tree loops for four classes

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
#Binary for train lw_rw_hip
train2_ank = mutate(train_ank, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_ank <- select(train2_ank,-COS.Intensity) 
colnames(train2_ank)[colnames(train2_ank)=="bin.int"] <- "COS.Intensity"
#Binary for test lw_rw_hip
test2_ank = mutate(test_ank, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_ank <- select(test2_ank,-COS.Intensity)
colnames(test2_ank)[colnames(test2_ank)=="bin.int"] <- "COS.Intensity"
#Binary for train lw_rw_ank
train2_hip = mutate(train_hip, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_hip <- select(train2_hip,-COS.Intensity) 
colnames(train2_hip)[colnames(train2_hip)=="bin.int"] <- "COS.Intensity"
#Binary for test lw_rw_ank
test2_hip = mutate(test_hip, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_hip <- select(test2_hip,-COS.Intensity)
colnames(test2_hip)[colnames(test2_hip)=="bin.int"] <- "COS.Intensity"
#End of create binary data 

#tree loops here with two prediction classes - nonactive, active 
#compute error for all locations and just lw_rw_hip and lw_rw_ank and take differences 
a<-GE_trees(train2_all,test2_all)
b<-GE_trees(train2_ank,test2_ank)
c<-GE_trees(train2_hip,test2_hip)
xt2_a<-c(a)
xt2_db<-c(a-b)
xt2_dc<-c(a-c)
#start loop. take random sample from training sets with same index. Compute tree errors and store differences.
for(i in 2:1200){
  index <- sample(1:nrow(train2_all), replace=TRUE) 
  train2a<-train2_all[index,] 
  train2b<-train2_ank[index,] 
  train2c<-train2_hip[index,]
  a<-GE_trees(train2a,test2_all)
  b<-GE_trees(train2b,test2_ank)
  c<-GE_trees(train2c,test2_hip)
  xt2_a[i]<-a
  xt2_db[i]<-c(a-b)
  xt2_dc[i]<-c(a-c)
}
#end of tree loops for two classes

#xt_a errors for all locations, #xt_db differences for lw_rw_hip,  #xt_dc differences for lw_rw_ank (binary with a 2)
save(xt_a,xt_db,xt_dc,xt2_a,xt2_db,xt2_dc,file = "ALtreediff3.RData")
