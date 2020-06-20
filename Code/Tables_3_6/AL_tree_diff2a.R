#install.packages("tree")
library(tree)
library(tidyverse)
library(mosaic)
rm(list=ls())
#Read visit 1 (.2A.txt) as training and visit 2 (.2B.txt) as test data for all locations. 
train_all <- read.table("AL.file.2A.txt",header=TRUE)
test_all <- read.table("AL.file.2B.txt",header=TRUE)
#Read data with just rw_lw, hip_lw and hip_rw locations
train_rw_lw <- train_all[,c(49:72,73:96,97:101)]
test_rw_lw <- test_all[,c(49:72,73:96,97:101)]
train_hip_lw <- train_all[,c(25:48,49:72,97:101)]
test_hip_lw <- test_all[,c(25:48,49:72,97:101)]
train_hip_rw <- train_all[,c(25:48,73:96,97:101)]
test_hip_rw <- test_all[,c(25:48,73:96,97:101)]

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
#compute error for all locations and just rw_lw, hip_lw and hip_rw and take differences
set.seed(98815)
a<-GE_trees(train_all,test_all)
e<-GE_trees(train_rw_lw,test_rw_lw)
f<-GE_trees(train_hip_lw,test_hip_lw)
g<-GE_trees(train_hip_rw,test_hip_rw)
xt_a<-c(a)
xt_de<-c(a-e)
xt_df<-c(a-f)
xt_dg<-c(a-g)
#start loop. take random sample from training sets with same index. Compute bagging errors and store differences.
for(i in 2:1200){
  index <- sample(1:nrow(train_all), replace=TRUE) 
  train2a<-train_all[index,] 
  train2e<-train_rw_lw[index,]
  train2f<-train_hip_lw[index,]
  train2g<-train_hip_rw[index,]
  a<-GE_trees(train2a,test_all)
  e<-GE_trees(train2e,test_rw_lw)
  f<-GE_trees(train2f,test_hip_lw)
  g<-GE_trees(train2g,test_hip_rw)
  xt_a[i]<-a
  xt_de[i]<-c(a-e)
  xt_df[i]<-c(a-f)
  xt_dg[i]<-c(a-g)
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
#Binary for train rw_lw
train2_rw_lw = mutate(train_rw_lw, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_rw_lw <- select(train2_rw_lw,-COS.Intensity) 
colnames(train2_rw_lw)[colnames(train2_rw_lw)=="bin.int"] <- "COS.Intensity"
#Binary for test rw_lw
test2_rw_lw = mutate(test_rw_lw, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_rw_lw <- select(test2_rw_lw,-COS.Intensity)
colnames(test2_rw_lw)[colnames(test2_rw_lw)=="bin.int"] <- "COS.Intensity"
#Binary for train hip_lw
train2_hip_lw = mutate(train_hip_lw, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_hip_lw <- select(train2_hip_lw,-COS.Intensity) 
colnames(train2_hip_lw)[colnames(train2_hip_lw)=="bin.int"] <- "COS.Intensity"
#Binary for test hip_lw
test2_hip_lw = mutate(test_hip_lw, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_hip_lw <- select(test2_hip_lw,-COS.Intensity)
colnames(test2_hip_lw)[colnames(test2_hip_lw)=="bin.int"] <- "COS.Intensity"
#Binary for train hip_rw
train2_hip_rw = mutate(train_hip_rw, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_hip_rw <- select(train2_hip_rw,-COS.Intensity) 
colnames(train2_hip_rw)[colnames(train2_hip_rw)=="bin.int"] <- "COS.Intensity"
#Binary for test hip_rw
test2_hip_rw = mutate(test_hip_rw, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_hip_rw <- select(test2_hip_rw,-COS.Intensity)
colnames(test2_hip_rw)[colnames(test2_hip_rw)=="bin.int"] <- "COS.Intensity"
#End of create binary data 

#tree loops here with two prediction classes - nonactive, active 
#compute error for all locations and just rw_lw, hip_lw and hip_rw and take differences 
a<-GE_trees(train2_all,test2_all)
e<-GE_trees(train2_rw_lw,test2_rw_lw)
f<-GE_trees(train2_hip_lw,test2_hip_lw)
g<-GE_trees(train2_hip_rw,test2_hip_rw)
xt2_a<-c(a)
xt2_de<-c(a-e)
xt2_df<-c(a-f)
xt2_dg<-c(a-g)
#start loop. take random sample from training sets with same index. Compute tree errors and store differences.
for(i in 2:1200){
  index <- sample(1:nrow(train_all), replace=TRUE) 
  train2a<-train2_all[index,] 
  train2e<-train2_rw_lw[index,]
  train2f<-train2_hip_lw[index,]
  train2g<-train2_hip_rw[index,]
  a<-GE_trees(train2a,test2_all)
  e<-GE_trees(train2e,test2_rw_lw)
  f<-GE_trees(train2f,test2_hip_lw)
  g<-GE_trees(train2g,test2_hip_rw)
  xt2_a[i]<-a
  xt2_de[i]<-c(a-e)
  xt2_df[i]<-c(a-f)
  xt2_dg[i]<-c(a-g)
}
#end of tree loops for two classes 

#xt_a errors for all locations, #xt_de differences for rw_lw,  #xt_df differences for hip_lw, #xt_dg differences for hip_rw (binary with a 2)
save(xt_a,xt_de,xt_df,xt_dg,xt2_a,xt2_de,xt2_df,xt2_dg,file = "ALtreediff2a.RData")


