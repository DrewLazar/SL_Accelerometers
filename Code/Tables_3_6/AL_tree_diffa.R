#install.packages("tree")
library(tree)
library(tidyverse)
library(mosaic)
rm(list=ls())
#Read visit 1 (AL.file.2A.txt) as training and visit 2 (AL.file.2B.txt) as test data for all locations.
train_all <- read.table("AL.file.2A.txt",header=TRUE)
test_all <- read.table("AL.file.2B.txt",header=TRUE)
train_lw <- train_all[,c(49:72,97:101)]
test_lw <- test_all[,c(49:72,97:101)]
train_rw <- train_all[,c(73:96,97:101)]
test_rw <- test_all[,c(73:96,97:101)]

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
#compute error for all locations and just lw and rw and take differences
a<-GE_trees(train_all,test_all)
d<-GE_trees(train_lw,test_lw)
e<-GE_trees(train_rw,test_rw)
xt_a<-c(a)
xt_dd<-c(a-d)
xt_de<-c(a-e)
#start loop. take random sample from training sets with same index. Compute tree errors and store differences.
for(i in 2:1200){
  index <- sample(1:nrow(train_all), replace=TRUE) 
  train2a<-train_all[index,] 
  train2d<-train_lw[index,]
  train2e<-train_rw[index,]
  a<-GE_trees(train2a,test_all)
  d<-GE_trees(train2d,test_lw)
  e<-GE_trees(train2e,test_rw)
  xt_a[i]<-a
  xt_dd[i]<-c(a-d)
  xt_de[i]<-c(a-e)
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
#Binary for training rw
train2_rw = mutate(train_rw, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_rw <- select(train2_rw,-COS.Intensity) 
colnames(train2_rw)[colnames(train2_rw)=="bin.int"] <- "COS.Intensity"
#Binary for test rw
test2_rw = mutate(test_rw, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_rw <- select(test2_rw,-COS.Intensity) 
colnames(test2_rw)[colnames(test2_rw)=="bin.int"] <- "COS.Intensity"
#Binary for training lw
train2_lw = mutate(train_lw, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_lw <- select(train2_lw,-COS.Intensity) 
colnames(train2_lw)[colnames(train2_lw)=="bin.int"] <- "COS.Intensity"
#Binary for test lw
test2_lw = mutate(test_lw, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_lw <- select(test2_lw,-COS.Intensity) 
colnames(test2_lw)[colnames(test2_lw)=="bin.int"] <- "COS.Intensity"
#End of create binary data 

#tree loops here with two prediction classes - nonactive, active 
#compute error for all locations and just lw and rw and take differences
a<-GE_trees(train2_all,test2_all)
d<-GE_trees(train2_lw,test2_lw)
e<-GE_trees(train2_rw,test2_rw)
xt2_a<-c(a)
xt2_dd<-c(a-d)
xt2_de<-c(a-e)
#start loop. take random sample from training sets with same index. Compute tree errors and store differences.
for(i in 2:1200){
  index <- sample(1:nrow(train2_all), replace=TRUE) 
  train2a<-train2_all[index,] 
  train2d<-train2_lw[index,]
  train2e<-train2_rw[index,]
  a<-GE_trees(train2a,test2_all)
  d<-GE_trees(train2d,test2_lw)
  e<-GE_trees(train2e,test2_rw)
  xt2_a[i]<-a
  xt2_dd[i]<-c(a-d)
  xt2_de[i]<-c(a-e)
}
#end of tree loops for two classes
#xt_a errors for all locations, xt_dd differences for lw,  xt_de differences for rw (binary with a 2)
save(xt_a,xt_dd,xt_de,xt2_a,xt2_dd,xt2_de,file = "ALtreediff1a.RData")
