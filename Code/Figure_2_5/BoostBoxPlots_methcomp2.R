rm(list=ls())
load("ALankdiff1a.RData")
xt2_b_ank<-xt2_db
xt2_c_ank<-xt2_dc
xt2_d_ank<-xt2_dd
load("ALank_lwdiff1.RData")
load("ALank_lwdiff1a.RData")
xt2_b_anklw<-xt2_db
xt2_c_anklw<-xt2_dc
xt2_d_anklw<-xt2_dd
load("ALank_hipdiff1.RData")
load("ALank_hipdiff1a.RData")
xt2_b_ankhip<-xt2_db
xt2_c_ankhip<-xt2_dc
xt2_d_ankhip<-xt2_dd

#createboxplots - Figure 2 - compare methods four classes
par(mar=par('mar')+c(0,.5,0,0));
boxplot(xt2_b_ank,xt2_c_ank,xt2_d_ank,xt2_b_anklw,xt2_c_anklw,xt2_d_anklw,xt2_b_ankhip,xt2_c_ankhip,xt2_d_ankhip,
        at=c(1,2,3,4,5,6,7,8,9),
        names=c("ANK","ANK","ANK","ANK_LW","ANK_LW","ANK_LW","ANK_HIP","ANK_HIP","ANK_HIP"),
        las=2,
        col=c("blue","red","yellow","blue","red","yellow","blue","red","yellow"),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)
points(xt2_b_ank[1],1,col=9,pch=3)
points(xt2_c_ank[1],2,col=9,pch=3)
points(xt2_d_ank[1],3,col=9,pch=3)
points(xt2_b_anklw[1],4,col=9,pch=3)
points(xt2_c_anklw[1],5,col=9,pch=3)
points(xt2_d_anklw[1],6,col=9,pch=3)
points(xt2_b_ankhip[1],7,col=9,pch=3)
points(xt2_c_ankhip[1],8,col=9,pch=3)
points(xt2_d_ankhip[1],9,col=9,pch=3)
legend("left", inset=.01, title="Method",
       c("Boost","Bag","RF","test error"), fill=c("blue","red","yellow","transparent"), border="white",horiz=FALSE, cex=0.9,pch = c(NA, NA, NA, 3))
