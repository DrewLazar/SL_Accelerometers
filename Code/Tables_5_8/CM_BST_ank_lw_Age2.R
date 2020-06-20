library(xtable)
library(C50)
library(caret)
library(dplyr)
library(irr)
library(mosaic)
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

#Take data from just ankle and lw 
train_nomiss_ank <- train2_nomiss_all[,c(1:24,49:72,97:101)]
test_nomiss_ank <- test2_nomiss_all[,c(1:24,49:72,97:101)]

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
set.seed(98815)
BSTrank_2<-BSTpredictout(train2_nomiss_ank,test2_nomiss_ank,29,80)
BSTcmrank_2<-BSTconfusionMat2(BSTrank_2,test2_nomiss_ank)
print(xtable(BSTcmrank_2[[1]], digits=4,type = "latex"), file = "C:/Users/Drew/Box Sync/Drew Lazar/Rcodes/MedianDiff/ConfusionMatrices/BoostCM_ank/BSTcm_ank2.tex")

#Get confusion matrix, error rate, kappa for ages 18-39
test2_nomiss_ank_YA <- test2_nomiss_ank[test2_nomiss_ank$Age %in% 18:39,]
index <- test2_nomiss_ank$Age %in% 18:39
BSTrank_2_YA <- BSTrank_2[index]
BSTcmYArank_2 <- BSTconfusionMat2(BSTrank_2_YA,test2_nomiss_ank_YA)
print(xtable(BSTcmYArank_2[[1]], digits=4,type = "latex"), file = "C:/Users/Drew/Box Sync/Drew Lazar/Rcodes/MedianDiff/ConfusionMatrices/BoostCM_ank_lw/BSTcmankYA2.tex")

#Get confusion matrix, error rate, kappa for ages 40-59
test2_nomiss_ank_MA <- test2_nomiss_ank[test2_nomiss_ank$Age %in% 40:59,]
index <- test2_nomiss_ank$Age %in% 40:59
BSTrank_2_MA <- BSTrank_2[index]
BSTcmMArank_2 <- BSTconfusionMat2(BSTrank_2_MA,test2_nomiss_ank_MA)
print(xtable(BSTcmMArank_2[[1]], digits=4,type = "latex"), file = "C:/Users/Drew/Box Sync/Drew Lazar/Rcodes/MedianDiff/ConfusionMatrices/BoostCM_ank_lw/BSTcmankMA2.tex")

#Get confusion matrix, error rate, kappa for ages 60+
test2_nomiss_ank_OA <- test2_nomiss_ank[test2_nomiss_ank$Age > 60,]
index <- test2_nomiss_ank$Age > 60
BSTrank_2_OA <- BSTrank_2[index]
BSTcmOArank_2 <- BSTconfusionMat2(BSTrank_2_OA,test2_nomiss_ank_OA)
print(xtable(BSTcmOArank_2[[1]], digits=4,type = "latex"), file = "C:/Users/Drew/Box Sync/Drew Lazar/Rcodes/MedianDiff/ConfusionMatrices/BoostCM_ank_lw/BSTcmankOA2.tex")
 









