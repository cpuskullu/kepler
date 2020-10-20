setwd("D:/Akademik/PROJE-MFAG113F353/KeplerLCs")

gozlem <- read.table("KOI2_K2/Q00SC125-127_kepler2_SYNFITTER-v24_tut2n.txt", 
                     header=FALSE, sep = "\t", 
                     col.names=c("GE", "GA", "GH", "ME", "MA", "AE", "AA", "GE2", "GA2", "GH2", "AE2", "AA2"))

# Geçiþ Eðrisi
par(fig=c(0,1,0.14,1), bg=NA, las=1, ps=10)
plot(gozlem$GE, gozlem$GA, 
     xlab="", ylab="Normalize Aký", pch = 19, col="#00000060",
     xlim=c(-0.08, 0.08), ylim=c(0.9920, 1.0010), tcl=0.2, xaxt='n',
     cex=0.6, cex.lab=1.0, cex.axis=1.0, cex.main=0.5, cex.sub=0.1)

axis(side=1, at = seq(-10, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=1, at = seq(-10, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=2, at = seq(0, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=2, at = seq(0, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=3, at = seq(-10, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=3, at = seq(-10, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=4, at = seq(0, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=4, at = seq(0, 10, by = 0.005), labels=FALSE, tcl=0.1)

# Üst Yazý
text(x=0, y=1.0005, label="HAT-P-7 b (BJD 125.0 - 127.8)", pos=3, cex=0.9)

# Hata Çubuklarý
arrows(gozlem$GE, gozlem$GA-gozlem$GH, 
       gozlem$GE, gozlem$GA+gozlem$GH, code=0, angle=90, length=0.1, col="#00000030")

# Model Eðri
lines(gozlem$ME, gozlem$MA, col="#FF0000", lwd="2.4")

#if(FALSE){
# 2.Min Eðrisi
par(fig=c(0.56,0.96,0.16,0.56), new=TRUE)

plot(gozlem$GE2, gozlem$GA2, 
     xlab="", ylab="", pch = 19, col="#00000060",
     xlim=c(0.35, 0.65), ylim=c(0.9998, 1.0002), tcl=0.2, xaxt='n', yaxt='n',
     cex=0.2, cex.lab=0.2, cex.axis=0.6
     )

axis(side=1, at = seq(-10, 10, by = 0.05), labels=FALSE, tcl=0.2,)
axis(side=1, at = seq(-10, 10, by = 0.01), labels=FALSE, tcl=0.1)
axis(side=2, at = seq(0, 10, by = 0.0002), labels=FALSE, tcl=0.2)
axis(side=2, at = seq(0, 10, by = 0.0001), labels=FALSE, tcl=0.1)
axis(side=3, at = seq(-10, 10, by = 0.05), labels=TRUE, tcl=0.2, cex.axis=0.5, mgp=c(3, .1, 0))
axis(side=3, at = seq(-10, 10, by = 0.01), labels=FALSE, tcl=0.1)
axis(side=4, at = seq(0, 10, by = 0.0002), labels=TRUE, tcl=0.2, cex.axis=0.5, mgp=c(3, .1, 0))
axis(side=4, at = seq(0, 10, by = 0.0001), labels=FALSE, tcl=0.1)

#lines(gozlem$ME, gozlem$MA, col="red", lwd="2.4")

smoothingSpline = smooth.spline(gozlem$ME, gozlem$MA, spar=0.75)
lines(smoothingSpline, col="red", lwd="2.4")
#}

# Artýklar Eðrisi
par(fig=c(0,1,0,0.4), new=TRUE, las=1)
plot(gozlem$AE, gozlem$AA, 
     xlab="Evre", ylab="Artýk.", pch = 19,  col="#00000080", 
     xlim=c(-0.08, 0.08), ylim=c(-0.001, 0.001), tcl=0.2, yaxt='n',
     at = seq(-10, 10, by = 0.02),
     cex=0.6, cex.lab=1.0, cex.axis=1.0)

axis(side=1, at = seq(-10, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=1, at = seq(-10, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=2, at = seq(-10, 10, by = 0.001), labels=TRUE, tcl=0.2)
axis(side=2, at = seq(-10, 10, by = 0.001), labels=FALSE, tcl=0.1)
axis(side=3, at = seq(-10, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=3, at = seq(-10, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=4, at = seq(-10, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=4, at = seq(-10, 10, by = 0.005), labels=FALSE, tcl=0.1)

abline(h=0, col="red", lwd="2")