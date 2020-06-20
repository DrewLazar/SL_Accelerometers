library(xtable)
library(C50)
library(tidyverse)
library(mosaic)
library(caret)
library(xtable)
rm(list=ls())

train_nomiss_all <- na.omit(read.table("AL.file.2A.txt",header=TRUE))
test_nomiss_all <- na.omit(read.table("AL.file.2B.txt",header=TRUE))

#Take data for just ankle
train_nomiss_ank <- train_nomiss_all[,c(1:24,97:101)]
test_nomiss_ank <- test_nomiss_all[,c(1:24,97:101)]

#Create boosting prediction function
BSTpredictout<-function(train,test,res_col, t) 
{
tree.train<-C5.0(train[,-res_col],train[,res_col], trials =t)
BSTpred<-predict(tree.train,test[,-res_col])
}

#Create confusion matrix function with error rates, kappa stats 
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

#Get confusion matrix, error rate, kappa for Female
test_nomiss_ank_F <- test_nomiss_ank[test_nomiss_ank$Sex == "Female",]
index <- test_nomiss_ank$Sex=="Female"
BSTrank_4_F <- BSTrank_4[index]
BSTcmFrank_4 <- BSTconfusionMat4(BSTrank_4_F,test_nomiss_ank_F)
print(xtable(BSTcmFrank_4[[1]], digits=4,type = "latex"), file = "C:/Users/Drew/Box Sync/Drew Lazar/Rcodes/MedianDiff/ConfusionMatrices/BoostCM_ank/BSTcm_ankF4.tex")

#Get confusion matrix, error rate, kappa for Male 
test_nomiss_ank_M <- test_nomiss_ank[test_nomiss_ank$Sex == "Male",]
index <- test_nomiss_ank$Sex=="Male"
BSTrank_4_M <- BSTrank_4[index]
BSTcmMrank_4 <- BSTconfusionMat4(BSTrank_4_M,test_nomiss_ank_M)
print(xtable(BSTcmMrank_4[[1]], digits=4,type = "latex"), file = "C:/Users/Drew/Box Sync/Drew Lazar/Rcodes/MedianDiff/ConfusionMatrices/BoostCM_ank/BSTcm_ankM4.tex")

