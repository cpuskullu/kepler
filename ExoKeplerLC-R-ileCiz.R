#####################################################################################
#                        -- ExoLC-Ciz.R - 05.12.2015 --                             #
# HESAP:: YOK                                                                       #
# CIZIM:: Dizindeki .ciz uzantili isik egrisi verisi dosyalarini okur               #
# ve ayri ayri cizdirir                                                             #
#####################################################################################

### --BUYUK HARFLERLE-- YAZILAN BASLIKLAR ICINDEKI GIRDILER DUZENLENMELIDIR. Kucuk Harflerle yazilan basliklar altinda islemler yapilir. ###

####################
### Kutuphaneler ###
library(scales) # renk saydamligi -alpha() icin kullaniliyor
              
###################
### --AYARLAR-- ###
setwd("D:/Akademik/TEZ/DR/ExoLCA/Kepler491 (KOI201)/WinFitter v2.6/SC Phase Binned")
dosyaadikisaltmasi <- "Kepler491b"
# Girdi Sutunlari: ObsPhase ObsFlux/Mag Error ModPhase ModFlux/Mag ResPhase ResFlux/Mag
# Disarida birakilacak dosyanin [.ciz] uzantisini [.iptal] yapiniz.
dosyalar <- list.files(path = ".", pattern = "*.ciz", all.files = FALSE)
# Girdi Sutunlari: Date/Time Telescope+Dedector
#gozlembaslik <- "#bos.in"
#baslik <- read.table(gozlembaslik, header=FALSE, sep = "\t", col.names=c("Tarih", "TelCCD"))
ciktidosya <- paste0("#",dosyaadikisaltmasi,"_R","_Birlestirilmis",".out")
### --AYARLAR-- ###
###################

dosyasayisi <- length(dosyalar)
#file.remove(ciktidosya)

# TEK DOSYA CIZDIRMEK ICIN: TRUE
if(F){
dosyalar[1] <- "Q04-13SC-ALLbinned_kepler485b_SYNFITTER-v32.sciz"
dosyasayisi <- 1
}

###################
### --GORUNUM-- ###
# Metin
bicem = "serif"  # "serif", "sans", "mono", "symbol"
gorun = 1 # 1:plain, 2:bold, 3: italic
metinboyut <- 18

# Grafik
graftur <- "p"
icerikboyut <- 1
kalinlik <- 1
modelkalinlik <- 3
noktatur <- 16
noktarenk <- alpha("black",1)
modelrenk <- alpha("red",1)
hatacubukciz <- "artikta"    # veride, artikta, herkisi, hicbiri
hatacubukrenk <- "gray60"
hatacubukkalinlik <- 1

# Eksen
etiket <- T
xi <- -0.025
xs <- 0.025
dx_ana <- 0.01
dx_ara <- 0.005
dx <- 0.005
yi <- 0.988
ys <- 1.004
dy_ana <- 0.004
dy_ara <- 0.002
ciftsayi <- T
if(ciftsayi){dy <- 0.002} else {dy <- 0.}
anaciz <- 0.8
araciz <- 0.4
kesisme <- "i" # Eksenlerin kesisme durumu:  "r" (regular), "i" (internal)

grafikbasligi <- 0
eksenbasligi <- 1

##############################
### --Satir/Sutun Cizimi-- ###
# Satir-sutun toplu cizim icin T; Tek tek cizim icin F
satsutciz <- T
satirs <- 2
sutuns <- 2
arabosluk <- 0

if(satsutciz){
metinboyut <- 20
icerikboyut <- 0.6
kalinlik <- 2
eksenbasligi <- 2
modelkalinlik <- 2
par(mfrow = c(satirs, sutuns), oma=c(8,9,1,1))  ##  plot.new() skips a position.
etiket <- F; etiket_x <- F; etiket_y <- F
}
### --Satir/Sutun Cizimi-- ###
##############################

if(eksenbasligi){
eksenbas_y <- "Normalised Flux"   # "Normalised Flux"
eksenbas_x <- "Phase"  # "Phase"
kenaralt <- 5
kenarsol <- 6
kenarust <- 1
kenarsag <- 1
} else {
eksenbas_y <- ""
eksenbas_x <- ""
kenaralt <- 2
kenarsol <- 4
kenarust <- 1
kenarsag <- 1
}

if(!etiket){
kenaralt <- 0
kenarsol <- 0
kenarust <- 0
kenarsag <- 0
}

# ArtikEgrisini Ayri Ciz/Cizme
artikegr_ayri <- F
artikegr_orjin <- 0.990
etiket_artik <- "Res."

### --GORUNUM-- ###
###################

for(i in 1:dosyasayisi){

################
### Veri Oku ###
gozlem <- read.table(dosyalar[i], 
                     header=FALSE, sep = "\t", 
                     col.names=c("GE", "GA", "GH", "ME", "MA", "AE", "AA"))

m  <- cbind(paste(gozlem$GE[!is.na(gozlem$GE)]), paste(gozlem$GA[!is.na(gozlem$GA)]), paste(gozlem$GH[!is.na(gozlem$GH)]))
write.table(m, file=ciktidosya, col.names=FALSE, row.names=FALSE, quote=FALSE, append = TRUE, sep = "\t")
##########################
### --GRAFIGI YAZDIR-- ###
# pdf(), png(), postscript(): saydamligi desteklemez: pdf'i eps'ye cevir.
yazdir <- 0 
if(yazdir){
pdf(paste0(dosyalar[i],".pdf"))#, width = 40, height = 30)
# En altta dev.off komutunu kapatmayi unutma
}
### --GRAFIGI YAZDIR-- ###
##########################

########################                     
### --Geçiþ Eðrisi-- ###
par(mar=c(kenaralt+arabosluk,kenarsol+arabosluk,kenarust,kenarsag),
     cex=icerikboyut, ps=metinboyut, font=gorun, family=bicem, cex.axis=1,
     pch=noktatur, bg=NA, las=1, lwd=kalinlik, xaxs=kesisme, yaxs=kesisme)

plot(gozlem$GE, gozlem$GA, type="n", xlab="", ylab="", axes=F,   
     xlim=c(xi, xs), ylim=c(yi, ys), col=noktarenk)

######################                     
### --Model Egri-- ###
lines(gozlem$ME, gozlem$MA, col=modelrenk, lwd=modelkalinlik)

if((i %% sutuns)==1) {etiket_y <- T};  if(i>sutuns*(satirs-1)) {etiket_x <- T}
#etiket_x <- T
axis(side=1, at=seq(xi+dx,xs,by=dx_ana), tcl=anaciz, lwd=par("lwd"), labels=etiket_x)
axis(side=1, at=seq(xi,xs,by=dx_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=2, at=seq(yi+dy,ys,by=dy_ana), tcl=anaciz, lwd=par("lwd"), labels=etiket_y)
axis(side=2, at=seq(yi,ys,by=dy_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=3, at=seq(xi+dx,xs,by=dx_ana), tcl=anaciz, lwd=par("lwd"), labels=FALSE)
axis(side=3, at=seq(xi,xs,by=dx_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=4, at=seq(yi+dy,ys,by=dy_ana), tcl=anaciz, lwd=par("lwd"), labels=FALSE)
axis(side=4, at=seq(yi,ys,by=dy_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
if(!etiket){etiket_x <- F; etiket_y <- F}

if(!satsutciz){
mtext(eksenbas_x, side=1, line=3)
mtext(eksenbas_y, side=2, line=4, las=0)}
box()

if(hatacubukciz=="veride" || hatacubukciz=="herikisi"){
# Hata Cubuklari: noktalardan once
arrows(gozlem$GE, gozlem$GA-gozlem$GH, gozlem$GE, gozlem$GA+gozlem$GH,  
       code=0, angle=90, col=hatacubukrenk, lwd=hatacubukkalinlik)
}
# Noktalar: hata cubuklarindan sonra
points(gozlem$GE, gozlem$GA, col=noktarenk)

#text(x=0, y=1.0105, label=baslik[i,1])
if(grafikbasligi){
# Grafik Basligi
#text(x=0, y=1.0350, label=baslik[i,1])
#text(x=0, y=1.0320, label=baslik[i,2])
}


####################                     
### --Artiklar-- ###
if(artikegr_ayri){
par(new=TRUE, mar=c(kenaralt,kenarsol,kenarust+20,kenarsag))
plot(gozlem$AE, gozlem$AA, axes=F, 
     xlim=c(xi, xs), ylim=c(-0.02, 0.02), tcl=anaciz, col=noktarenk)

axis(side=3, at=seq(xi,xs,by=dx_ana), tcl=anaciz, lwd=par("lwd"), labels=FALSE)
axis(side=3, at=seq(xi,xs,by=dx_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
box()
abline(h=0, col=modelrenk, lwd=modelkalinlik)

} else { 
par(new=TRUE)
plot(gozlem$AE, gozlem$AA+artikegr_orjin, type="n", xlab="", ylab="", axes=F,
     xlim=c(xi, xs), ylim=c(yi, ys), col=noktarenk) 

if(hatacubukciz=="artikta" || hatacubukciz=="herikisi"){
# Hata Cubuklari: noktalardan once
arrows(gozlem$AE, gozlem$AA+artikegr_orjin-gozlem$GH, 
     gozlem$AE, gozlem$AA+artikegr_orjin+gozlem$GH, code=0, angle=90, 
     length=0.1, col=hatacubukrenk, lwd=hatacubukkalinlik)
}
# Noktalar: hata cubuklarindan sonra
abline(h=artikegr_orjin, col=modelrenk, lwd=modelkalinlik) 
points(gozlem$AE, gozlem$AA+artikegr_orjin, col=noktarenk)

}

########################                     
### --2.Min Eðrisi-- ###
if(F){
par(fig=c(0.23,0.77,0.46,0.89), cex=0.4, new=TRUE)
plot(gozlem$GE, gozlem$GA, font=gorun, family=bicem,
     xlab="", ylab="", pch = 16, col="#00000040",
     xlim=c(0.0, 1), ylim=c(0.9999, 1.0000), tcl=0.2, xaxt='n', yaxt='n',
     cex=0.1, cex.lab=0.2, cex.axis=0.6, cex.main=0.5, cex.sub=0.1)

axis(side=1, at = seq(-10, 10, by = 0.1), labels=T, tcl=0.2)
axis(side=1, at = seq(-10, 10, by = 0.05), labels=FALSE, tcl=0.1)
axis(side=2, at = seq(0, 10, by = 0.0001), labels=T, tcl=0.2)
axis(side=2, at = seq(0, 10, by = 0.00005), labels=FALSE, tcl=0.1)
axis(side=3, at = seq(-10, 10, by = 0.1), labels=FALSE, tcl=0.2)
axis(side=3, at = seq(-10, 10, by = 0.05), labels=FALSE, tcl=0.1)
axis(side=4, at = seq(0, 10, by = 0.0001), labels=FALSE, tcl=0.2)
axis(side=4, at = seq(0, 10, by = 0.00005), labels=FALSE, tcl=0.1)

# smoothingSpline = smooth.spline(gozlem$ME, gozlem$MA, spar=0.05)
# lines(smoothingSpline, col="red", lwd="2.4")
lines(gozlem$ME, gozlem$MA, col="red", lwd="2.4")
}
### --2.Min Eðrisi-- ###
########################                     

if(yazdir){
dev.off() }
}

if(satsutciz){
mtext(eksenbas_x, side=1, line=5, cex=par("cex")+0.4, outer = TRUE)
mtext(eksenbas_y, side=2, line=6, cex=par("cex")+0.4, las=0, outer = TRUE)}
