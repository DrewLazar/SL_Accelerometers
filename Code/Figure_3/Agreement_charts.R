#install.packages("vcd")
library(irr)
library(vcd)
library(tidyverse)
library(mosaic)
library(C50)
library(caret)
rm(list=ls())
setwd("C:/Users/dmlazar/Box/Drew Lazar/Rcodes/MedianDiff/Agreement_Charts")
train_nomiss_all <- na.omit(read.table("AL.file.2A.txt",header=TRUE))
index <- train_nomiss_all$Wt == 94.8
train_nomiss_all$Age[index] <- 29 
test_nomiss_all <- na.omit(read.table("AL.file.2B.txt",header=TRUE))
train_nomiss_lw_ank <- train_nomiss_all[,c(1:24,49:72,97:101)]
test_nomiss_lw_ank <- test_nomiss_all[,c(1:24,49:72,97:101)]

BSTpredictout<-function(train,test,res_col, t) 
{
  tree.train<-C5.0(train[,-res_col],train[,res_col], trials =t)
  BSTpred<-predict(tree.train,test[,-res_col])
}

BSTconfusionMat4 = function(BSTpredict,test){
  BSTpredict <- factor(BSTpredict,levels=c("SED","LPA","MPA","VPA"))
  test$COS.Intensity <- factor(test$COS.Intensity,levels=c("SED","LPA","MPA","VPA"))
  A<-confusionMatrix(BSTpredict,test$COS.Intensity)
  return(A[[2]])  
}
#Overall agreement plot 
BST_lwrank_4<-BSTpredictout(train_nomiss_lw_ank,test_nomiss_lw_ank,53,80)
BSTcm_lwrank_4<-BSTconfusionMat4(BST_lwrank_4,test_nomiss_lw_ank)
agreementplot(BSTcm_lwrank_4, xlab="Predicted class", ylab = "True class")
#Add main= for title


test_nomiss_lw_ank_OA <- test_nomiss_lw_ank[test_nomiss_lw_ank$Age > 55,]
index <- test_nomiss_lw_ank$Age > 55
BST_lwrank_4_OA <- BST_lwrank_4[index]
BSTcmOA_lwrank_4 <- BSTconfusionMat4(BST_lwrank_4_OA,test_nomiss_lw_ank_OA)
agreementplot(BSTcmOA_lwrank_4, xlab="Predicted class", ylab = "True class")

BMIfunction = function(Dataset,BMIL, BMIH) Dataset <- Dataset[(703*(Dataset$Wt)/(Dataset$Ht)^2>BMIL) & (703*(Dataset$Wt)/(Dataset$Ht)^2<BMIH),] 
test_nomiss_lw_ank_HB <- BMIfunction(test_nomiss_lw_ank,30,50)
index <- (703*(test_nomiss_lw_ank$Wt)/(test_nomiss_lw_ank$Ht)^2>30) & (703*(test_nomiss_lw_ank$Wt)/(test_nomiss_lw_ank$Ht)^2<50)
BST_lwrank_4_HB <- BST_lwrank_4[index]
BSTcmHB_lwrank_4 <- BSTconfusionMat4(BST_lwrank_4_HB,test_nomiss_lw_ank_HB)
agreementplot(BSTcmHB_lwrank_4, xlab="Predicted class", ylab = "True class")

test_nomiss_lw_ank_M <- test_nomiss_lw_ank[test_nomiss_lw_ank$Sex == "Male",]
index <- test_nomiss_lw_ank$Sex=="Male"
BST_lwrank_4_M <- BST_lwrank_4[index]
BSTcmM_lwrank_4 <- BSTconfusionMat4(BST_lwrank_4_M,test_nomiss_lw_ank_M)
agreementplot(BSTcmM_lwrank_4 , xlab="Predicted class", ylab = "True class")
