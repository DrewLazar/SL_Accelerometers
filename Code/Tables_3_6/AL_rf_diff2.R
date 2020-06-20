library(randomForest)
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

#Remove missing cases 
train_nomiss_all = na.omit(train_all)
test_nomiss_all = na.omit(test_all)
train_nomiss_lw_ank = na.omit(train_lw_ank)
test_nomiss_lw_ank = na.omit(test_lw_ank)
train_nomiss_rw_ank = na.omit(train_rw_ank)
test_nomiss_rw_ank = na.omit(test_rw_ank)
train_nomiss_hip_ank = na.omit(train_hip_ank)
test_nomiss_hip_ank = na.omit(test_hip_ank)


#Function for computing random forest errors
AL_bagging = function(train, test, b){
  #Rand forest classifier with b available features.
  sb=sqrt(b)
  AL.bag=randomForest(COS.Intensity~.,data=train,mtry=sb,importance=TRUE)
  AL.predict.bag = predict(AL.bag,newdata=test)
  #error
  1-mean(AL.predict.bag ==test$COS.Intensity)
}

#rf loops here with four prediction classes - SED, LPA, MPA, VPA
#compute error for all locations and just lw_ank, rw_ank, hip_ank and take differences
a<-AL_bagging(train_nomiss_all,test_nomiss_all,100)
b<-AL_bagging(train_nomiss_lw_ank,test_nomiss_lw_ank,52)
c<-AL_bagging(train_nomiss_rw_ank,test_nomiss_rw_ank,52)
d<-AL_bagging(train_nomiss_hip_ank,test_nomiss_hip_ank,52)
xt_a<-c(a)
xt_db<-c(a-b)
xt_dc<-c(a-c)
xt_dd<-c(a-d)
#start loop. take random sample from training sets with same index. Compute bagging errors and store differences. 
for(i in 2:1200){
  index <- sample(1:nrow(train_nomiss_all), replace=TRUE) 
  train2a<-train_nomiss_all[index,] 
  train2b<-train_nomiss_lw_ank[index,] 
  train2c<-train_nomiss_rw_ank[index,]
  train2d<-train_nomiss_hip_ank[index,]
  a<-AL_bagging(train2a,test_all,100)
  b<-AL_bagging(train2b,test_nomiss_lw_ank,52)
  c<-AL_bagging(train2c,test_nomiss_rw_ank,52)
  d<-AL_bagging(train2d,test_nomiss_hip_ank,52)
  xt_a[i]<-a
  xt_db[i]<-c(a-b)
  xt_dc[i]<-c(a-c)
  xt_dd[i]<-c(a-d)
}
#end of rf loops for four classes

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
#Binary for train lw_ank
train2_nomiss_lw_ank = mutate(train_nomiss_lw_ank, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_nomiss_lw_ank <- select(train2_nomiss_lw_ank,-COS.Intensity) 
colnames(train2_nomiss_lw_ank)[colnames(train2_nomiss_lw_ank)=="bin.int"] <- "COS.Intensity"
#Binary for test lw_ank
test2_nomiss_lw_ank = mutate(test_nomiss_lw_ank, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_nomiss_lw_ank <- select(test2_nomiss_lw_ank,-COS.Intensity) 
colnames(test2_nomiss_lw_ank)[colnames(test2_nomiss_lw_ank)=="bin.int"] <- "COS.Intensity"
#Binary for train rw_ank
train2_nomiss_rw_ank = mutate(train_nomiss_rw_ank, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_nomiss_rw_ank <- select(train2_nomiss_rw_ank,-COS.Intensity) 
colnames(train2_nomiss_rw_ank)[colnames(train2_nomiss_rw_ank)=="bin.int"] <- "COS.Intensity"
#Binary for test rw_ank
test2_nomiss_rw_ank = mutate(test_nomiss_rw_ank, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_nomiss_rw_ank <- select(test2_nomiss_rw_ank,-COS.Intensity) 
colnames(test2_nomiss_rw_ank)[colnames(test2_nomiss_rw_ank)=="bin.int"] <- "COS.Intensity"
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

#rf loops here with two prediction classes - nonactive, active 
#compute error for all locations and just lw_ank, rw_ank, hip_ank and take differences 
set.seed(98815)
a<-AL_bagging(train2_nomiss_all,test2_nomiss_all,100)
b<-AL_bagging(train2_nomiss_lw_ank,test2_nomiss_lw_ank,52)
c<-AL_bagging(train2_nomiss_rw_ank,test2_nomiss_rw_ank,52)
d<-AL_bagging(train2_nomiss_hip_ank,test2_nomiss_hip_ank,52)
xt2_a<-c(a)
xt2_db<-c(a-b)
xt2_dc<-c(a-c)
xt2_dd<-c(a-d)
#start loop. take random sample from training sets with same index. Compute bagging errors and store differences. 
for(i in 2:1200){
  index <- sample(1:nrow(train2_nomiss_all), replace=TRUE) 
  train2a<-train2_nomiss_all[index,] 
  train2b<-train2_nomiss_lw_ank[index,] 
  train2c<-train2_nomiss_rw_ank[index,]
  train2d<-train2_nomiss_hip_ank[index,]
  a<-AL_bagging(train2a,test2_nomiss_all,100)
  b<-AL_bagging(train2b,test2_nomiss_lw_ank,52)
  c<-AL_bagging(train2c,test2_nomiss_rw_ank,52)
  d<-AL_bagging(train2d,test2_nomiss_hip_ank,52)
  xt2_a[i]<-c(a)
  xt2_db[i]<-c(a-b)
  xt2_dc[i]<-c(a-c)
  xt2_dd[i]<-c(a-d)
}

#end of rf loops for two classes

#xt_a errors for all locations, #xt_b differences for lw_ank,  #xt_c differences for rw_ank,  #xt_dd differences for hip_ank (binary with a 2)
save(xt_a,xt_db,xt_dc,xt_dd,xt2_a,xt2_db,xt2_dc,xt2_dd,file = "ALrfdiff2.RData")


