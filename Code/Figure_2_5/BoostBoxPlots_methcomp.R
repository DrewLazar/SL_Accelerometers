rm(list=ls())
load("ALankdiff1.RData")
load("ALankdiff1a.RData")
xt_b_ank<-xt_db
xt_c_ank<-xt_dc
xt_d_ank<-xt_dd
load("ALank_lwdiff1.RData")
load("ALank_lwdiff1a.RData")
xt_b_anklw<-xt_db
xt_c_anklw<-xt_dc
xt_d_anklw<-xt_dd
load("ALank_hipdiff1.RData")
load("ALank_hipdiff1a.RData")
xt_b_ankhip<-xt_db
xt_c_ankhip<-xt_dc
xt_d_ankhip<-xt_dd

#createboxplots - Figure 2 - compare methods two classes - 
par(mar=par('mar')+c(0,.5,0,0));
boxplot(xt_b_ank,xt_c_ank,xt_d_ank,xt_b_anklw,xt_c_anklw,xt_d_anklw,xt_b_ankhip,xt_c_ankhip,xt_d_ankhip,
        at=c(1,2,3,4,5,6,7,8,9),
        names=c("ANK","ANK","ANK","ANK_LW","ANK_LW","ANK_LW","ANK_HIP","ANK_HIP","ANK_HIP"),
        las=2,
        col=c("blue","red","yellow","blue","red","yellow","blue","red","yellow"),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)
points(xt_b_ank[1],1,col=9,pch=3)
points(xt_c_ank[1],2,col=9,pch=3)
points(xt_d_ank[1],3,col=9,pch=3)
points(xt_b_anklw[1],4,col=9,pch=3)
points(xt_c_anklw[1],5,col=9,pch=3)
points(xt_d_anklw[1],6,col=9,pch=3)
points(xt_b_ankhip[1],7,col=9,pch=3)
points(xt_c_ankhip[1],8,col=9,pch=3)
points(xt_d_ankhip[1],9,col=9,pch=3)
legend("right", inset=.01, title="Method",
       c("Boost","Bag","RF","test error"), fill=c("blue","red","yellow","transparent"), border="white",horiz=FALSE, cex=0.9,pch = c(NA, NA, NA, 3))