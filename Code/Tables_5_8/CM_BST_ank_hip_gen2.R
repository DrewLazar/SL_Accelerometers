library(xtable)
library(C50)
library(tidyverse)
library(mosaic)
library(caret)
library(xtable)
rm(list=ls())

train_nomiss_all <- na.omit(read.table("AL.file.2A.txt",header=TRUE))
test_nomiss_all <- na.omit(read.table("AL.file.2B.txt",header=TRUE))

#Create Binary Data
train2_nomiss_all = mutate(train_nomiss_all, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
train2_nomiss_all <- select(train2_nomiss_all,-COS.Intensity) 
colnames(train2_nomiss_all)[colnames(train2_nomiss_all)=="bin.int"] <- "COS.Intensity"
test2_nomiss_all = mutate(test_nomiss_all, bin.int = derivedFactor(
  "active" = COS.Intensity %in% c("MPA", "VPA"),
  "nonactive" = COS.Intensity %in% c("SED", "LPA"),
  .default = NA
))
test2_nomiss_all <- select(test2_nomiss_all,-COS.Intensity)
colnames(test2_nomiss_all)[colnames(test2_nomiss_all)=="bin.int"] <- "COS.Intensity"

#Take data for just ankle and hip
train_nomiss_ank <- train2_nomiss_all[,c(1:24,25:48,97:101)]
test_nomiss_ank <- test2_nomiss_all[,c(1:24,25:48,97:101)]

#Create boosting prediction function
BSTpredictout<-function(train,test,res_col, t) 
{
  tree.train<-C5.0(train[,-res_col],train[,res_col], trials =t)
  BSTpred<-predict(tree.train,test[,-res_col])
}

#create confusion matrix function with error rates, kappa stats 
BSTconfusionMat2 = function(BSTpredict,test){
  BSTpredict <- factor(BSTpredict,levels=c("nonactive","active"))
  test$COS.Intensity <- factor(test$COS.Intensity,levels=c("nonactive","active"))
  dfa2<-cbind(BSTpredict,test$COS.Intensity)
  kstat<-kappa2(dfa2,"unweighted")
  kstate<-kappa2(dfa2,"equal")
  kstats<-kappa2(dfa2,"squared")
  A<-confusionMatrix(BSTpredict,test$COS.Intensity)
  rl<-list("CM"=A[[2]],"acc="=A[[3]][1],"kappa"=kstat[5],"kappae"=kstate[5],"kappas"=kstats[5])
  return(rl)  
}

#Apply boosting algoritm to data with ankle and get confusion matrix, error rate, kappa 
BSTrank_2<-BSTpredictout(train2_nomiss_ank,test2_nomiss_ank,29,80)
BSTcmrank_2<-BSTconfusionMat2(BSTrank_2,test2_nomiss_ank)
print(xtable(BSTcmrank_2[[1]], digits=4,type = "latex"), file = "C:/Users/Drew/Box Sync/Drew Lazar/Rcodes/MedianDiff/ConfusionMatrices/BoostCM_ank/BSTcm_ank2.tex")

#Get confusion matrix, error rate, kappa for Female
test2_nomiss_ank_F <- test2_nomiss_ank[test2_nomiss_ank$Sex == "Female",]
index <- test2_nomiss_ank$Sex=="Female"
BSTrank_2_F <- BSTrank_2[index]
BSTcmFrank_2 <- BSTconfusionMat2(BSTrank_2_F,test2_nomiss_ank_F)
print(xtable(BSTcmFrank_2[[1]], digits=4,type = "latex"), file = "C:/Users/Drew/Box Sync/Drew Lazar/Rcodes/MedianDiff/ConfusionMatrices/BoostCM_ank/BSTcm_ankhipF2.tex")

#Get confusion matrix, error rate, kappa for Male
test2_nomiss_ank_M <- test2_nomiss_ank[test2_nomiss_ank$Sex == "Male",]
index <- test2_nomiss_ank$Sex=="Male"
BSTrank_2_M <- BSTrank_2[index]
BSTcmMrank_2 <- BSTconfusionMat2(BSTrank_2_M,test2_nomiss_ank_M)
print(xtable(BSTcmMrank_2[[1]], digits=4,type = "latex"), file = "C:/Users/Drew/Box Sync/Drew Lazar/Rcodes/MedianDiff/ConfusionMatrices/BoostCM_ank/BSTcm_ankhipM2.tex")


