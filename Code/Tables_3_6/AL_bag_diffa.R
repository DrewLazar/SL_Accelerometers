#install.packages("randomForest")
#install.packages("tree")
library(randomForest)
library(tidyverse)
library(mosaic)
rm(list=ls())
#Read visit 1 (AL.file.2A.txt) as training and visit 2 (AL.file.2B.txt) as test data for all locations.  
train_all <- read.table("AL.file.2A.txt",header=TRUE)
test_all <- read.table("AL.file.2B.txt",header=TRUE)
#Read data with just lw and rw locations
train_lw <- train_all[,c(49:72,97:101)]
test_lw <- test_all[,c(49:72,97:101)]
train_rw <- train_all[,c(73:96,97:101)]
test_rw <- test_all[,c(73:96,97:101)]

#Remove missing cases 
train_nomiss_all = na.omit(train_all)
test_nomiss_all = na.omit(test_all)
train_nomiss_rw = na.omit(train_rw)
test_nomiss_rw = na.omit(test_rw)
train_nomiss_lw = na.omit(train_lw)
test_nomiss_lw = na.omit(test_lw)


#Function for computing bagging errors
AL_bagging = function(train, test, b){
  # Bagging classifier with b available features.
  AL.bag=randomForest(COS.Intensity~.,data=train,mtry=b,importance=TRUE)
  AL.predict.bag = predict(AL.bag,newdata=test)
  #error
  1-mean(AL.predict.bag ==test$COS.Intensity)
}

#bag loops here with four prediction classes - SED, LPA, MPA, VPA
#compute error for all locations and just lw and rw and take differences
a<-AL_bagging(train_nomiss_all,test_nomiss_all,100)
d<-AL_bagging(train_nomiss_lw,test_nomiss_lw,28)
e<-AL_bagging(train_nomiss_rw,test_nomiss_rw,28)
xt_a<-c(a)
xt_dd<-c(a-d)
xt_de<-c(a-e)
#start loop. take random sample from training sets with same index. Compute bagging errors and store differences. 
for(i in 2:1200){
  index <- sample(1:nrow(train_nomiss_all), replace=TRUE) 
  train2a<-train_nomiss_all[index,] 
  train2d<-train_nomiss_lw[index,]
  train2e<-train_nomiss_rw[index,]
  a<-AL_bagging(train2a,test_nomiss_all,100)
  d<-AL_bagging(train2d,test_nomiss_lw,28)
  e<-AL_bagging(train2e,test_nomiss_rw,28)
  xt_a[i]<-a
  xt_dd[i]<-c(a-d)
  xt_de[i]<-c(a-e)
}
#end of bag loops for four classes 

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

#remove missing cases from binary data 
train2_nomiss_all = na.omit(train2_all)
test2_nomiss_all = na.omit(test2_all)
train2_nomiss_rw = na.omit(train2_rw)
test2_nomiss_rw = na.omit(test2_rw)
train2_nomiss_lw = na.omit(train2_lw)
test2_nomiss_lw = na.omit(test2_lw)

#bag loops here with two prediction classes - nonactive, active 
#compute error for all locations and just lw and rw and take differences
set.seed(98815)
a<-AL_bagging(train2_nomiss_all,test2_nomiss_all,100)
d<-AL_bagging(train2_nomiss_lw,test2_nomiss_lw,28)
e<-AL_bagging(train2_nomiss_rw,test2_nomiss_rw,28)
xt2_a<-c(a)
xt2_dd<-c(a-d)
xt2_de<-c(a-e)
#start loop. take random sample from training sets with same index. Compute bagging errors and store differences. 
for(i in 2:1200){
  index <- sample(1:nrow(train2_nomiss_all), replace=TRUE) 
  train2a<-train2_nomiss_all[index,] 
  train2d<-train2_nomiss_lw[index,]
  train2e<-train2_nomiss_rw[index,]
  a<-AL_bagging(train2a,test2_nomiss_all,100)
  d<-AL_bagging(train2d,test2_nomiss_lw,28)
  e<-AL_bagging(train2e,test2_nomiss_rw,28)
  xt2_a[i]<-a
  xt2_dd[i]<-c(a-d)
  xt2_de[i]<-c(a-e)
}
#end of bag loops for two classes

#xt_a errors for all locations, xt_dd differences for ank,  xt_de differences for hip (binary with a 2)
save(xt_a,xt_dd,xt_de,xt2_a,xt2_dd,xt2_de,file = "ALbagdiff1a.RData")
