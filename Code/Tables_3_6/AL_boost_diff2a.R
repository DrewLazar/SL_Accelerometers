#install.packages("tree")
library(tidyverse)
library(mosaic)
library(C50)
rm(list=ls())
#Read visit 1 (.2A.txt) as training and visit 2 (.2B.txt) as test data for all locations. 
train_all <- read.table("AL.file.2A.txt",header=TRUE)
test_all <- read.table("AL.file.2B.txt",header=TRUE)
#Read data with just hip_ank and rw_lw locations
train_hip_ank <- train_all[,c(1:24,25:48,97:101)]
test_hip_ank <- test_all[,c(1:24,25:48,97:101)]
train_rw_lw <- train_all[,c(49:72,73:96,97:101)]
test_rw_lw <- test_all[,c(49:72,73:96,97:101)]

#Remove missing cases
train_nomiss_all = na.omit(train_all)
test_nomiss_all = na.omit(test_all)
train_nomiss_hip_ank = na.omit(train_hip_ank)
test_nomiss_hip_ank = na.omit(test_hip_ank)
train_nomiss_rw_lw = na.omit(train_rw_lw)
test_nomiss_rw_lw = na.omit(test_rw_lw)


#Function for computing boosting errors
boost_tree = function(train, test, res_col, t){
  tree.train<-C5.0(train[,-res_col],train[,res_col], trials =t)
  pred<-predict(tree.train,test[,-res_col])
  1-mean(pred == test$COS.Intensity)
}

#boost loops here with four prediction classes - SED, LPA, MPA, VPA
#compute error for all locations and just hip_ank and rw_lw and take differences
a<-boost_tree(train_nomiss_all,test_nomiss_all,101,80)
d<-boost_tree(train_nomiss_hip_ank,test_nomiss_hip_ank,53,80)
e<-boost_tree(train_nomiss_rw_lw,test_nomiss_rw_lw,53,80)
xt_a<-c(a)
xt_dd<-c(a-d)
xt_de<-c(a-e)
#start loop. take random sample from training sets with same index. Compute boosting errors and store differences.
for(i in 2:1200){
  index <- sample(1:nrow(train_nomiss_all), replace=TRUE) 
  train2a<-train_nomiss_all[index,] 
  train2d<-train_nomiss_hip_ank[index,]
  train2e<-train_nomiss_rw_lw[index,]
  a<-boost_tree(train2a,test_nomiss_all,101,80)
  d<-boost_tree(train2d,test_nomiss_hip_ank,53,80)
  e<-boost_tree(train2e,test_nomiss_rw_lw,53,80)
  xt_a[i]<-a
  xt_dd[i]<-c(a-d)
  xt_de[i]<-c(a-e)
}
#end of boost loops for four classes 

#Create binary data - with classes active = {MPA,VPA} and nonactive = {SED,LPA}
#Binary for training
train2_nomiss_all = mutate(train_nomiss_all, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_nomiss_all <- select(train2_nomiss_all,-COS.Intensity) 
colnames(train2_nomiss_all)[colnames(train2_nomiss_all)=="bin.int"] <- "COS.Intensity"
#Binary for test
test2_nomiss_all = mutate(test_nomiss_all, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_nomiss_all <- select(test2_nomiss_all,-COS.Intensity)
colnames(test2_nomiss_all)[colnames(test2_nomiss_all)=="bin.int"] <- "COS.Intensity"
#Binary for train hip_ank
train2_nomiss_hip_ank = mutate(train_nomiss_hip_ank, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_nomiss_hip_ank <- select(train2_nomiss_hip_ank,-COS.Intensity) 
colnames(train2_nomiss_hip_ank)[colnames(train2_nomiss_hip_ank)=="bin.int"] <- "COS.Intensity"
#Binary for test hip_ank
test2_nomiss_hip_ank = mutate(test_nomiss_hip_ank, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_nomiss_hip_ank <- select(test2_nomiss_hip_ank,-COS.Intensity)
colnames(test2_nomiss_hip_ank)[colnames(test2_nomiss_hip_ank)=="bin.int"] <- "COS.Intensity"
#Binary for train rw_lw
train2_nomiss_rw_lw = mutate(train_nomiss_rw_lw, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_nomiss_rw_lw <- select(train2_nomiss_rw_lw,-COS.Intensity) 
colnames(train2_nomiss_rw_lw)[colnames(train2_nomiss_rw_lw)=="bin.int"] <- "COS.Intensity"
#Binary for test rw_lw
test2_nomiss_rw_lw = mutate(test_nomiss_rw_lw, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_nomiss_rw_lw <- select(test2_nomiss_rw_lw,-COS.Intensity)
colnames(test2_nomiss_rw_lw)[colnames(test2_nomiss_rw_lw)=="bin.int"] <- "COS.Intensity"
#End of create binary data

#boost loops here with two prediction classes - nonactive, active 
#compute error for all locations and just hip_ank and rw_lw and take differences 
a<-boost_tree(train2_nomiss_all,test2_nomiss_all,101,80)
d<-boost_tree(train2_nomiss_hip_ank,test2_nomiss_hip_ank,53,80)
e<-boost_tree(train2_nomiss_rw_lw,test2_nomiss_rw_lw,53,80)
xt2_a<-c(a)
xt2_dd<-c(a-d)
xt2_de<-c(a-e)
#start loop. take random sample from training sets with same index. Compute bagging errors and store differences. 
for(i in 2:1200){
  index <- sample(1:nrow(train_nomiss_all), replace=TRUE) 
  train2a<-train2_nomiss_all[index,] 
  train2d<-train2_nomiss_hip_ank[index,]
  train2e<-train2_nomiss_rw_lw[index,]
  a<-boost_tree(train2a,test2_nomiss_all,101,80)
  d<-boost_tree(train2d,test2_nomiss_hip_ank,53,80)
  e<-boost_tree(train2e,test2_nomiss_rw_lw,53,80)
  xt2_a[i]<-a
  xt2_dd[i]<-c(a-d)
  xt2_de[i]<-c(a-e)
}
#end of boost loops for two classes

#xt_a errors for all locations, #xt_db differences for hip_ank,  #xt_dc differences for rw_lw (binary with a 2)
save(xt_a,xt_dd,xt_de,xt2_a,xt2_dd,xt2_de,file = "ALboostdiff2a.RData")


