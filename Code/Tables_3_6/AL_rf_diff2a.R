library(randomForest)
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

#Remove missing cases
train_nomiss_all = na.omit(train_all)
test_nomiss_all = na.omit(test_all)
train_nomiss_rw_lw = na.omit(train_rw_lw)
test_nomiss_rw_lw = na.omit(test_rw_lw)
train_nomiss_hip_lw = na.omit(train_hip_lw)
test_nomiss_hip_lw = na.omit(test_hip_lw)
train_nomiss_hip_rw = na.omit(train_hip_rw)
test_nomiss_hip_rw = na.omit(test_hip_rw)

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
#compute error for all locations and just rw_lw, hip_lw and hip_rw and take differences
a<-AL_bagging(train_nomiss_all,test_nomiss_all,100)
e<-AL_bagging(train_nomiss_rw_lw,test_nomiss_rw_lw,52)
f<-AL_bagging(train_nomiss_hip_lw,test_nomiss_hip_lw,52)
g<-AL_bagging(train_nomiss_hip_rw,test_nomiss_hip_rw,52)
xt_a<-c(a)
xt_de<-c(a-e)
xt_df<-c(a-f)
xt_dg<-c(a-g)
#start loop. take random sample from training sets with same index. Compute bagging errors and store differences.
for(i in 2:1200){
  index <- sample(1:nrow(train_nomiss_all), replace=TRUE) 
  train2a<-train_nomiss_all[index,] 
  train2e<-train_nomiss_rw_lw[index,]
  train2f<-train_nomiss_hip_lw[index,]
  train2g<-train_nomiss_hip_rw[index,]
  a<-AL_bagging(train2a,test_nomiss_all,100)
  e<-AL_bagging(train2e,test_nomiss_rw_lw,52)
  f<-AL_bagging(train2f,test_nomiss_hip_lw,52)
  g<-AL_bagging(train2g,test_nomiss_hip_rw,52)
  xt_a[i]<-a
  xt_de[i]<-c(a-e)
  xt_df[i]<-c(a-f)
  xt_dg[i]<-c(a-g)
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

#rf loops here with two prediction classes - nonactive, active 
#compute error for all locations and just rw_lw, hip_lw and hip_rw and take differences 
a<-AL_bagging(train2_nomiss_all,test2_nomiss_all,100)
e<-AL_bagging(train2_nomiss_rw_lw,test2_nomiss_rw_lw,52)
f<-AL_bagging(train2_nomiss_hip_lw,test2_nomiss_hip_lw,52)
g<-AL_bagging(train2_nomiss_hip_rw,test2_nomiss_hip_rw,52)
xt2_a<-c(a)
xt2_de<-c(a-e)
xt2_df<-c(a-f)
xt2_dg<-c(a-g)
#start loop. take random sample from training sets with same index. Compute rf errors and store differences.
for(i in 2:1200){
  index <- sample(1:nrow(train2_nomiss_all), replace=TRUE) 
  train2a<-train2_nomiss_all[index,] 
  train2e<-train2_nomiss_rw_lw[index,]
  train2f<-train2_nomiss_hip_lw[index,]
  train2g<-train2_nomiss_hip_rw[index,]
  a<-AL_bagging(train2a,test2_nomiss_all,100)
  e<-AL_bagging(train2e,test2_nomiss_rw_lw,52)
  f<-AL_bagging(train2f,test2_nomiss_hip_lw,52)
  g<-AL_bagging(train2g,test2_nomiss_hip_rw,52)
  xt2_a[i]<-c(a)
  xt2_de[i]<-c(a-e)
  xt2_df[i]<-c(a-f)
  xt2_dg[i]<-c(a-g)
}
#end of rf loops for two classes

#xt_a errors for all locations, #xt_de differences for rw_lw,  #xt_df differences for hip_lw, #xt_dg differences for hip_rw (binary with a 2)
save(xt_a,xt_de,xt_df,xt_dg,xt2_a,xt2_de,xt2_df,xt2_dg,file = "ALrfdiff2a.RData")


