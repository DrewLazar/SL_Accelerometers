setwd("C:/Users/dmlazar/Dropbox/Shared writing folder accelerometer modeling project/DataCode/bootstrap_data")

#load bootstrapping results for boosting, single placement, differences with all 
load("ALboostdiff1.RData")
load("ALboostdiff1a.RData")

#createboxplots - boosting comparison with all,
#four classes and single placement - Figure 1 (a)
par(mar=par('mar')+c(0,.5,0,0));
boxplot(xt_db,xt_dc,xt_dd,xt_de,
        at=c(1,2,3,4),
        names=c("ANK","HIP","LW","RW"),
        las=2,
        col=c("darkviolet","yellow","orange","red","blue"),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
      )
points(xt_db[1],1,col=9,pch=3)
points(xt_dc[1],2,col=9,pch=3)
points(xt_dd[1],3,col=9,pch=3)
points(xt_de[1],4,col=9,pch=3)
legend("topright",inset=.1, c("test errors"), fill=c("transparent"), border="white",horiz=FALSE, cex=0.9,pch = c(3))

#createboxplots - boosting comparison with all,
#two classes and single placement - Figure 4 (a)
par(mar=par('mar')+c(0,.5,0,0));
boxplot(xt2_db,xt2_dc,xt2_dd,xt2_de,
        at=c(1,2,3,4),
        names=c("ANK","HIP","LW","RW"),
        las=2,
        col=c("darkviolet","yellow","orange","red","blue"),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)
points(xt2_db[1],1,col=9,pch=3)
points(xt2_dc[1],2,col=9,pch=3)
points(xt2_dd[1],3,col=9,pch=3)
points(xt2_de[1],4,col=9,pch=3)

rm(list=ls())
#load bootstrapping results for boosting, two placements, differences with all 
load("ALboostdiff2.RData")
load("ALboostdiff2a.RData")
load("ALboostdiff2b.RData")

#createboxplots - boosting comparison with all,
#four classes and two placements - Figure 1 (b)
boxplot(xt_db,xt_dc,xt_dd,xt_de,xt_df,xt_dg,
        at=c(1,2,3,4,5,6),
        names=c("LW_ANK","RW_ANK","HIP_ANK","RW_LW","HIP_LW","HIP_RW"),
        las=2,
        col=c("darkviolet","yellow","orange","red","blue","green"),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)
points(xt_db[1],1,col=9,pch=3)
points(xt_dc[1],2,col=9,pch=3)
points(xt_dd[1],3,col=9,pch=3)
points(xt_de[1],4,col=9,pch=3)
points(xt_df[1],5,col=9,pch=3)
points(xt_dg[1],6,col=9,pch=3)
legend("topright",inset=.1, c("test errors"), fill=c("transparent"), border="white",horiz=FALSE, cex=0.9,pch = c(3))

#createboxplots - boosting comparison with all,
#two classes and two placements - Figure 4 (b)
boxplot(xt2_db,xt2_dc,xt2_dd,xt2_de,xt2_df,xt2_dg,
        at=c(1,2,3,4,5,6),
        names=c("LW_ANK","RW_ANK","HIP_ANK","RW_LW","HIP_LW","HIP_RW"),
        las=2,
        col=c("darkviolet","yellow","orange","red","blue","green"),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)
points(xt_db[1],1,col=9,pch=3)
points(xt_dc[1],2,col=9,pch=3)
points(xt_dd[1],3,col=9,pch=3)
points(xt_de[1],4,col=9,pch=3)
points(xt_df[1],5,col=9,pch=3)
points(xt_dg[1],6,col=9,pch=3)
legend("topright",inset=.1, c("test errors"), fill=c("transparent"), border="white",horiz=FALSE, cex=0.9,pch = c(3))






