library(tidyverse)
library(mosaic)
library(C50)
rm(list=ls())
#Read visit 1 (.2A.txt) as training and visit 2 (.2B.txt) as test data for all locations.
train_all <- read.table("AL.file.2A.txt",header=TRUE)
test_all <- read.table("AL.file.2B.txt",header=TRUE)
train_hip_lw <- train_all[,c(25:48,49:72,97:101)]
test_hip_lw <- test_all[,c(25:48,49:72,97:101)]
train_hip_rw <- train_all[,c(25:48,73:96,97:101)]
test_hip_rw <- test_all[,c(25:48,73:96,97:101)]

train_nomiss_all = na.omit(train_all)
test_nomiss_all = na.omit(test_all)
train_nomiss_hip_lw = na.omit(train_hip_lw)
test_nomiss_hip_lw = na.omit(test_hip_lw)
train_nomiss_hip_rw = na.omit(train_hip_rw)
test_nomiss_hip_rw = na.omit(test_hip_rw)

#Function for computing boosting errors
boost_tree = function(train, test, res_col, t){
  tree.train<-C5.0(train[,-res_col],train[,res_col], trials =t)
  pred<-predict(tree.train,test[,-res_col])
  1-mean(pred == test$COS.Intensity)
}

#boost loops here with four prediction classes - SED, LPA, MPA, VPA
#compute error for all locations and just hip_lw and hip_rw and take differences
a<-boost_tree(train_nomiss_all,test_nomiss_all,101,80)
f<-boost_tree(train_nomiss_hip_lw,test_nomiss_hip_lw,53,80)
g<-boost_tree(train_nomiss_hip_rw,test_nomiss_hip_rw,53,80)
xt_a<-c(a)
xt_df<-c(a-f)
xt_dg<-c(a-g)
#start loop. take random sample from training sets with same index. Compute boosting errors and store differences.
for(i in 2:1200){
  index <- sample(1:nrow(train_nomiss_all), replace=TRUE) 
  train2a<-train_nomiss_all[index,] 
  train2f<-train_nomiss_hip_lw[index,]
  train2g<-train_nomiss_hip_rw[index,]
  a<-boost_tree(train2a,test_nomiss_all,101,80)
  f<-boost_tree(train2f,test_nomiss_hip_lw,53,80)
  g<-boost_tree(train2g,test_nomiss_hip_rw,53,80)
  xt_a[i]<-a
  xt_df[i]<-c(a-f)
  xt_dg[i]<-c(a-g)
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
#Binary for train hip_lw
train2_nomiss_hip_lw = mutate(train_nomiss_hip_lw, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_nomiss_hip_lw <- select(train2_nomiss_hip_lw,-COS.Intensity) 
colnames(train2_nomiss_hip_lw)[colnames(train2_nomiss_hip_lw)=="bin.int"] <- "COS.Intensity"
#Binary for test hip_lw
test2_nomiss_hip_lw = mutate(test_nomiss_hip_lw, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_nomiss_hip_lw <- select(test2_nomiss_hip_lw,-COS.Intensity)
colnames(test2_nomiss_hip_lw)[colnames(test2_nomiss_hip_lw)=="bin.int"] <- "COS.Intensity"
#Binary for train hip_rw
train2_nomiss_hip_rw = mutate(train_nomiss_hip_rw, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_nomiss_hip_rw <- select(train2_nomiss_hip_rw,-COS.Intensity) 
colnames(train2_nomiss_hip_rw)[colnames(train2_nomiss_hip_rw)=="bin.int"] <- "COS.Intensity"
#Binary for test hip_rw
test2_nomiss_hip_rw = mutate(test_nomiss_hip_rw, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_nomiss_hip_rw <- select(test2_nomiss_hip_rw,-COS.Intensity)
colnames(test2_nomiss_hip_rw)[colnames(test2_nomiss_hip_rw)=="bin.int"] <- "COS.Intensity"
#End of create binary data 

#boost loops here with two prediction classes - nonactive, active 
#compute error for all locations and just hip_lw and hip_rw and take differences 
a<-boost_tree(train2_nomiss_all,test2_nomiss_all,101,80)
f<-boost_tree(train2_nomiss_hip_lw,test2_nomiss_hip_lw,53,80)
g<-boost_tree(train2_nomiss_hip_rw,test2_nomiss_hip_rw,53,80)
xt2_a<-c(a)
xt2_df<-c(a-f)
xt2_dg<-c(a-g)
#start loop. take random sample from training sets with same index. Compute boost errors and store differences. 
for(i in 2:1200){
  index <- sample(1:nrow(train_nomiss_all), replace=TRUE) 
  train2a<-train2_nomiss_all[index,] 
  train2f<-train2_nomiss_hip_lw[index,]
  train2g<-train2_nomiss_hip_rw[index,]
  a<-boost_tree(train2a,test2_nomiss_all,101,80)
  f<-boost_tree(train2f,test2_nomiss_hip_lw,53,80)
  g<-boost_tree(train2g,test2_nomiss_hip_rw,53,80)
  xt2_a[i]<-a
  xt2_df[i]<-c(a-f)
  xt2_dg[i]<-c(a-g)
}
#end of boost loops for two classes

#xt_a errors for all locations, #xt_db differences for hip_lw,  #xt_dc differences for hip_rw (binary with a 2)
save(xt_a,xt_df,xt_dg,xt2_a,xt2_df,xt2_dg,file = "ALboostdiff2b.RData")


