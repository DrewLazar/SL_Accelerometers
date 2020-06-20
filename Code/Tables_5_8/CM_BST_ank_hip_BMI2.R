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

#Create function to compute BMI
BMIfunction = function(Dataset,BMIL, BMIH) Dataset <- Dataset[(703*(Dataset$Wt)/(Dataset$Ht)^2>BMIL) & (703*(Dataset$Wt)/(Dataset$Ht)^2<BMIH),] 

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

#Get confusion matrix, error rate, kappa for BMI<24.9
test2_nomiss_ank_LB <- BMIfunction(test2_nomiss_ank,1,24.9)
index <- (703*(test2_nomiss_ank$Wt)/(test2_nomiss_ank$Ht)^2>1) & (703*(test2_nomiss_ank$Wt)/(test2_nomiss_ank$Ht)^2<24.9)
BSTrank_2_LB <- BSTrank_2[index]
BSTcmLBrank_2 <- BSTconfusionMat2(BSTrank_2_LB,test2_nomiss_ank_LB)
print(xtable(BSTcmLBrank_2[[1]], digits=4,type = "latex"), file = "C:/Users/Drew/Box Sync/Drew Lazar/Rcodes/MedianDiff/ConfusionMatrices/BoostCM_ank/BSTcm_ankhipLB2.tex")

#Get confusion matrix, error rate, kappa for 24.9<BMI<30
test2_nomiss_ank_MB <- BMIfunction(test2_nomiss_ank,24.9,30)
index <- (703*(test2_nomiss_ank$Wt)/(test2_nomiss_ank$Ht)^2>24.9) & (703*(test2_nomiss_ank$Wt)/(test2_nomiss_ank$Ht)^2<30)
BSTrank_2_MB <- BSTrank_2[index]
BSTcmMBrank_2 <- BSTconfusionMat2(BSTrank_2_MB,test2_nomiss_ank_MB)
print(xtable(BSTcmMBrank_2[[1]], digits=4,type = "latex"), file = "C:/Users/Drew/Box Sync/Drew Lazar/Rcodes/MedianDiff/ConfusionMatrices/BoostCM_ank/BSTcm_ankhipMB2.tex")

#Get confusion matrix, error rate, kappa for 30<BMI<50
test2_nomiss_ank_HB <- BMIfunction(test2_nomiss_ank,30,50)
index <- (703*(test2_nomiss_ank$Wt)/(test2_nomiss_ank$Ht)^2>30) & (703*(test2_nomiss_ank$Wt)/(test2_nomiss_ank$Ht)^2<50)
BSTrank_2_HB <- BSTrank_2[index]
BSTcmHBrank_2 <- BSTconfusionMat2(BSTrank_2_HB,test2_nomiss_ank_HB)
print(xtable(BSTcmHBrank_2[[1]], digits=4,type = "latex"), file = "C:/Users/Drew/Box Sync/Drew Lazar/Rcodes/MedianDiff/ConfusionMatrices/BoostCM_ank/BSTcm_ankhipHB2.tex")