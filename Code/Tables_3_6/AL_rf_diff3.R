library(randomForest)
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

#Remove missing cases 
train_nomiss_all = na.omit(train_all)
test_nomiss_all = na.omit(test_all)
train_nomiss_hip = na.omit(train_hip)
test_nomiss_hip = na.omit(test_hip)
train_nomiss_ank = na.omit(train_ank)
test_nomiss_ank = na.omit(test_ank)


#Function for computing random forest errors
AL_bagging = function(train, test, b){
  #Random forest classifier with b available features.
  sb=sqrt(b)
  AL.bag=randomForest(COS.Intensity~.,data=train,mtry=sb,importance=TRUE)
  AL.predict.bag = predict(AL.bag,newdata=test)
  #error
  1-mean(AL.predict.bag ==test$COS.Intensity)
}

#rf loops here with four prediction classes - SED, LPA, MPA, VPA
#compute error for all locations and just lw_rw_hip and lw_rw_ank and take differences
a<-AL_bagging(train_nomiss_all,test_nomiss_all,100)
b<-AL_bagging(train_nomiss_ank,test_nomiss_ank,76)
c<-AL_bagging(train_nomiss_hip,test_nomiss_hip,76)
xt_a<-c(a)
xt_db<-c(a-b)
xt_dc<-c(a-c)
#start loop. take random sample from training sets with same index. Compute bagging errors and store differences. 
for(i in 2:1000){
  index <- sample(1:nrow(train_nomiss_all), replace=TRUE) 
  train2a<-train_nomiss_all[index,] 
  train2b<-train_nomiss_ank[index,] 
  train2c<-train_nomiss_hip[index,]
  a<-AL_bagging(train2a,test_nomiss_all,100)
  b<-AL_bagging(train2b,test_nomiss_ank,76)
  c<-AL_bagging(train2c,test_nomiss_hip,76)
  xt_a[i]<-a
  xt_db[i]<-c(a-b)
  xt_dc[i]<-c(a-c)
}
#end of rf loops for four classes

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

#Remove missing cases 
train2_nomiss_all = na.omit(train2_all)
test2_nomiss_all = na.omit(test2_all)
train2_nomiss_hip = na.omit(train2_hip)
test2_nomiss_hip = na.omit(test2_hip)
train2_nomiss_ank = na.omit(train2_ank)
test2_nomiss_ank = na.omit(test2_ank)

#tree loops here with two prediction classes - nonactive, active 
#compute error for all locations and just lw_rw_hip and lw_rw_ank and take differences 
a<-AL_bagging(train2_nomiss_all,test2_nomiss_all,100)
b<-AL_bagging(train2_nomiss_ank,test2_nomiss_ank,76)
c<-AL_bagging(train2_nomiss_hip,test2_nomiss_hip,76)
xt2_a<-c(a)
xt2_db<-c(a-b)
xt2_dc<-c(a-c)
#start loop. take random sample from training sets with same index. Compute rf errors and store differences.
for(i in 2:1000){
  index <- sample(1:nrow(train2_nomiss_all), replace=TRUE) 
  train2a<-train2_nomiss_all[index,] 
  train2b<-train2_nomiss_ank[index,] 
  train2c<-train2_nomiss_hip[index,]
  a<-AL_bagging(train2a,test2_nomiss_all,100)
  b<-AL_bagging(train2b,test2_nomiss_ank,76)
  c<-AL_bagging(train2c,test2_nomiss_hip,76)
  xt2_a[i]<-a
  xt2_db[i]<-c(a-b)
  xt2_dc[i]<-c(a-c)
}
#end of rf loops for two classes 

#xt_a errors for all locations, #xt_db differences for lw_rw_hip,  #xt_dc differences for lw_rw_ank (binary with a 2)
save(xt_a,xt_db,xt_dc,xt2_a,xt2_db,xt2_dc,file = "ALrfdiff3.RData")
