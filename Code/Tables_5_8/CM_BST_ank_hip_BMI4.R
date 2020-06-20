library(xtable)
library(C50)
library(tidyverse)
library(mosaic)
library(caret)
library(xtable)
rm(list=ls())


train_nomiss_all <- na.omit(read.table("AL.file.2A.txt",header=TRUE))
test_nomiss_all <- na.omit(read.table("AL.file.2B.txt",header=TRUE))

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

#Create confuction matrix function with error rates, kappa stats 
BSTconfusionMat4 = function(BSTpredict,test){
  BSTpredict <- factor(BSTpredict,levels=c("SED","LPA","MPA","VPA"))
  test$COS.Intensity <- factor(test$COS.Intensity,levels=c("SED","LPA","MPA","VPA"))
  dfa2<-cbind(BSTpredict,test$COS.Intensity)
  kstat<-kappa2(dfa2,"unweighted")
  kstate<-kappa2(dfa2,"equal")
  kstats<-kappa2(dfa2,"squared")
  A<-confusionMatrix(BSTpredict,test$COS.Intensity)
  rl<-list("CM"=A[[2]],"acc="=A[[3]][1],"kappa"=kstat[5],"kappae"=kstate[5],"kappas"=kstats[5])
  return(rl)  
}

#Apply boosting algoritm to data with ankle and get confusion matrix, error rate, kappa 
BSTrank_4<-BSTpredictout(train_nomiss_ank,test_nomiss_ank,29,80)
BSTcmrank_4<-BSTconfusionMat4(BSTrank_4,test_nomiss_ank)
print(xtable(BSTcmrank_4[[1]], digits=4,type = "latex"), file = "C:/Users/Drew/Box Sync/Drew Lazar/Rcodes/MedianDiff/ConfusionMatrices/BoostCM_ank/BSTcm_ank4.tex")

#Get confusion matrix, error rate, kappa for BMI<24.9
test_nomiss_ank_LB <- BMIfunction(test_nomiss_ank,1,24.9)
index <- (703*(test_nomiss_ank$Wt)/(test_nomiss_ank$Ht)^2>1) & (703*(test_nomiss_ank$Wt)/(test_nomiss_ank$Ht)^2<24.9)
BSTrank_4_LB <- BSTrank_4[index]
BSTcmLBrank_4 <- BSTconfusionMat4(BSTrank_4_LB,test_nomiss_ank_LB)
print(xtable(BSTcmLBrank_4[[1]], digits=4,type = "latex"), file = "C:/Users/Drew/Box Sync/Drew Lazar/Rcodes/MedianDiff/ConfusionMatrices/BoostCM_ank/BSTcm_ankhipLB4.tex")

#Get confusion matrix, error rate, kappa for 24.9<BMI<30
test_nomiss_ank_MB <- BMIfunction(test_nomiss_ank,24.9,30)
index <- (703*(test_nomiss_ank$Wt)/(test_nomiss_ank$Ht)^2>24.9) & (703*(test_nomiss_ank$Wt)/(test_nomiss_ank$Ht)^2<30)
BSTrank_4_MB <- BSTrank_4[index]
BSTcmMBrank_4 <- BSTconfusionMat4(BSTrank_4_MB,test_nomiss_ank_MB)
print(xtable(BSTcmMBrank_4[[1]], digits=4,type = "latex"), file = "C:/Users/Drew/Box Sync/Drew Lazar/Rcodes/MedianDiff/ConfusionMatrices/BoostCM_ank/BSTcm_ankhipMB4.tex")

#Get confusion matrix, error rate, kappa for 30<BMI<50
test_nomiss_ank_HB <- BMIfunction(test_nomiss_ank,30,50)
index <- (703*(test_nomiss_ank$Wt)/(test_nomiss_ank$Ht)^2>30) & (703*(test_nomiss_ank$Wt)/(test_nomiss_ank$Ht)^2<50)
BSTrank_4_HB <- BSTrank_4[index]
BSTcmHBrank_4 <- BSTconfusionMat4(BSTrank_4_HB,test_nomiss_ank_HB)
print(xtable(BSTcmHBrank_4[[1]], digits=4,type = "latex"), file = "C:/Users/Drew/Box Sync/Drew Lazar/Rcodes/MedianDiff/ConfusionMatrices/BoostCM_ank/BSTcm_ankhipHB4.tex")


