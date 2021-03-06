#install.packages("tree")
library(tree)
library(tidyverse)
library(mosaic)
rm(list=ls())

#Read visit 1 (.2A.txt) as training and visit 2 (.2B.txt) as test data for all locations. 
train_all <- read.table("AL.file.2A.txt",header=TRUE)
test_all <- read.table("AL.file.2B.txt",header=TRUE)
#Read data with just lw_ank and rw_ank locations
train_lw_ank <- train_all[,c(1:24,49:72,97:101)]
test_lw_ank <- test_all[,c(1:24,49:72,97:101)]
train_rw_ank <- train_all[,c(1:24,73:96,97:101)]
test_rw_ank <- test_all[,c(1:24,73:96,97:101)]
train_hip_ank <- train_all[,c(1:24,25:48,97:101)]
test_hip_ank <- test_all[,c(1:24,25:48,97:101)]


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
#compute error for all locations and just lw_ank, rw_ank, hip_ank and take differences
a<-GE_trees(train_all,test_all)
b<-GE_trees(train_lw_ank,test_lw_ank)
c<-GE_trees(train_rw_ank,test_rw_ank)
d<-GE_trees(train_hip_ank,test_hip_ank)
xt_a<-c(a)
xt_db<-c(a-b)
xt_dc<-c(a-c)
xt_dd<-c(a-d)
#start loop. take random sample from training sets with same index. Compute bagging errors and store differences. 
for(i in 2:1200){
  index <- sample(1:nrow(train_all), replace=TRUE) 
  train2a<-train_all[index,] 
  train2b<-train_lw_ank[index,] 
  train2c<-train_rw_ank[index,]
  train2d<-train_hip_ank[index,]
  a<-GE_trees(train2a,test_all)
  b<-GE_trees(train2b,test_lw_ank)
  c<-GE_trees(train2c,test_rw_ank)
  d<-GE_trees(train2d,test_hip_ank)
  xt_a[i]<-a
  xt_db[i]<-c(a-b)
  xt_dc[i]<-c(a-c)
  xt_dd[i]<-c(a-d)
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
#Binary for all 
test2_all = mutate(test_all, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_all <- select(test2_all,-COS.Intensity)
colnames(test2_all)[colnames(test2_all)=="bin.int"] <- "COS.Intensity"
#Binary for train lw_ank 
train2_lw_ank = mutate(train_lw_ank, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_lw_ank <- select(train2_lw_ank,-COS.Intensity) 
colnames(train2_lw_ank)[colnames(train2_lw_ank)=="bin.int"] <- "COS.Intensity"
#Binary for test lw_ank 
test2_lw_ank = mutate(test_lw_ank, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_lw_ank <- select(test2_lw_ank,-COS.Intensity) 
colnames(test2_lw_ank)[colnames(test2_lw_ank)=="bin.int"] <- "COS.Intensity"
#Binary for train rw_ank 
train2_rw_ank = mutate(train_rw_ank, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_rw_ank <- select(train2_rw_ank,-COS.Intensity) 
colnames(train2_rw_ank)[colnames(train2_rw_ank)=="bin.int"] <- "COS.Intensity"
#Binary for test rw_ank 
test2_rw_ank = mutate(test_rw_ank, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_rw_ank <- select(test2_rw_ank,-COS.Intensity) 
colnames(test2_rw_ank)[colnames(test2_rw_ank)=="bin.int"] <- "COS.Intensity"
#Binary for train hip_ank 
train2_hip_ank = mutate(train_hip_ank, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_hip_ank <- select(train2_hip_ank,-COS.Intensity) 
colnames(train2_hip_ank)[colnames(train2_hip_ank)=="bin.int"] <- "COS.Intensity"
#Binary for test hip_ank 
test2_hip_ank = mutate(test_hip_ank, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_hip_ank <- select(test2_hip_ank,-COS.Intensity)
colnames(test2_hip_ank)[colnames(test2_hip_ank)=="bin.int"] <- "COS.Intensity"
#End of create binary data 

#tree loops here with two prediction classes - nonactive, active 
#compute error for all locations and just lw_ank, rw_ank, hip_ank and take differences 
a<-GE_trees(train2_all,test2_all)
b<-GE_trees(train2_lw_ank,test2_lw_ank)
c<-GE_trees(train2_rw_ank,test2_rw_ank)
d<-GE_trees(train2_hip_ank,test2_hip_ank)
xt2_a<-c(a)
xt2_db<-c(a-b)
xt2_dc<-c(a-c)
xt2_dd<-c(a-d)
#start loop. take random sample from training sets with same index. Compute tree errors and store differences.
for(i in 2:1200){
  index <- sample(1:nrow(train2_all), replace=TRUE) 
  train2a<-train2_all[index,] 
  train2b<-train2_lw_ank[index,] 
  train2c<-train2_rw_ank[index,]
  train2d<-train2_hip_ank[index,]
  a<-GE_trees(train2a,test2_all)
  b<-GE_trees(train2b,test2_lw_ank)
  c<-GE_trees(train2c,test2_rw_ank)
  d<-GE_trees(train2d,test2_hip_ank)
  xt2_a[i]<-a
  xt2_db[i]<-c(a-b)
  xt2_dc[i]<-c(a-c)
  xt2_dd[i]<-c(a-d)
}
#end of tree loops for two classes

#xt_a errors for all locations, #xt_b differences for lw_ank,  #xt_c differences for rw_ank,  #xt_dd differences for hip_ank (binary with a 2)
save(xt_a,xt_db,xt_dc,xt_dd,xt2_a,xt2_db,xt2_dc,xt2_dd,file = "ALtreediff2.RData")


