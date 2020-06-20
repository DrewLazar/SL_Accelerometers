library(xtable)
library(C50)
library(caret)
library(dplyr)
library(irr)
library(mosaic)
rm(list=ls())

train_nomiss_all <- na.omit(read.table("AL.file.2A.txt",header=TRUE))
test_nomiss_all <- na.omit(read.table("AL.file.2B.txt",header=TRUE))

#Take data for just ankle and hip
train_nomiss_ank <- train2_nomiss_all[,c(1:24,25:48,97:101)]
test_nomiss_ank <- test2_nomiss_all[,c(1:24,25:48,97:101)]

#Create boosting prediction function
BSTpredictout<-function(train,test,res_col, t) 
{
  tree.train<-C5.0(train[,-res_col],train[,res_col], trials =t)
  BSTpred<-predict(tree.train,test[,-res_col])
}

#create confuction matrix function with error rates, kappa stats 
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

#Get confusion matrix, error rate, kappa for ages 18-39
test_nomiss_ank_YA <- test_nomiss_ank[test_nomiss_ank$Age %in% 18:39,]
index <- test_nomiss_ank$Age %in% 18:39
BSTrank_4_YA <- BSTrank_4[index]
BSTcmYArank_4 <- BSTconfusionMat4(BSTrank_4_YA,test_nomiss_ank_YA)
print(xtable(BSTcmYArank_4[[1]], digits=4,type = "latex"), file = "C:/Users/Drew/Box Sync/Drew Lazar/Rcodes/MedianDiff/ConfusionMatrices/BoostCM_ank/BSTcm_ankhipYA4.tex")

#Get confusion matrix, error rate, kappa for ages 40-59
test_nomiss_ank_MA <- test_nomiss_ank[test_nomiss_ank$Age %in% 40:59,]
index <- test_nomiss_ank$Age %in% 40:59
BSTrank_4_MA <- BSTrank_4[index]
BSTcmMArank_4 <- BSTconfusionMat4(BSTrank_4_MA,test_nomiss_ank_MA)
print(xtable(BSTcmMArank_4[[1]], digits=4,type = "latex"), file = "C:/Users/Drew/Box Sync/Drew Lazar/Rcodes/MedianDiff/ConfusionMatrices/BoostCM_ank/BSTcm_ankhipMA4.tex")

#Get confusion matrix, error rate, kappa for ages 60+
test_nomiss_ank_OA <- test_nomiss_ank[test_nomiss_ank$Age > 60,]
index <- test_nomiss_ank$Age > 60
BSTrank_4_OA <- BSTrank_4[index]
BSTcmOArank_4 <- BSTconfusionMat4(BSTrank_4_OA,test_nomiss_ank_OA)
print(xtable(BSTcmOArank_4[[1]], digits=4,type = "latex"), file = "C:/Users/Drew/Box Sync/Drew Lazar/Rcodes/MedianDiff/ConfusionMatrices/BoostCM_ank/BSTcm_ankhipOA4.tex")
