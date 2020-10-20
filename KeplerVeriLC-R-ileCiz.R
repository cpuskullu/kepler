#####################################################################################
#                        -- ExoLC-Ciz.R - 05.12.2015 --                             #
# HESAP:: YOK                                                                       #
# CIZIM:: Dizindeki .ciz uzantili isik egrisi verisi dosyalarini okur               #
# ve ayri ayri cizdirir                                                             #
#####################################################################################

### --BUYUK HARFLERLE-- YAZILAN BASLIKLAR ICINDEKI GIRDILER DUZENLENMELIDIR. Kucuk Harflerle yazilan basliklar altinda islemler yapilir. ###

####################
### Kutuphaneler ###
#library(scales) # renk saydamligi -alpha() icin kullaniliyor
              
###################
### --AYARLAR-- ###
setwd("J:/Akademik/Araþtýrma Projeleri/2019 - BAP (Ortak)/Veri/KOI0202/Kepler412SC/Extracted Data")
dosyaadikisaltmasi <- "Kepler412b"
# Girdi Sutunlari: ObsPhase ObsFlux/Mag Error ModPhase ModFlux/Mag ResPhase ResFlux/Mag
# Disarida birakilacak dosyanin [.ciz] uzantisini [.iptal] yapiniz.
dosyalar <- list.files(path = ".", pattern = "*.tbl", all.files = FALSE)
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
dosyalar[1] <- "Q04SC384-413_kepler412-1.tbl"
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
noktarenk <- "black" #noktarenk <- alpha("black",1)
modelrenk <- "red" #modelrenk <- alpha("red",1)
hatacubukciz <- "artikta"    # veride, artikta, herkisi, hicbiri
hatacubukrenk <- "gray60"
hatacubukkalinlik <- 1

# Eksen
etiket <- T
#xi <- -0.025
#xs <- 0.025
dx_ana <- 0.01
dx_ara <- 0.005
dx <- 0.005
#yi <- 0.988
#ys <- 1.004
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
satsutciz <- F
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
                     col.names=c("GE", "GA", "GH")) #, "ME", "MA", "AE", "AA"))

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
#par(mar=c(kenaralt+arabosluk,kenarsol+arabosluk,kenarust,kenarsag),
#     cex=icerikboyut, ps=metinboyut, font=gorun, family=bicem, cex.axis=1,
#     pch=noktatur, bg=NA, las=1, lwd=kalinlik, xaxs=kesisme, yaxs=kesisme)

plot(gozlem$GE, gozlem$GA, xlab="", ylab="", axes=T, col=noktarenk)


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
# lines(gozlem$ME, gozlem$MA, col="red", lwd="2.4")
}
### --2.Min Eðrisi-- ###
########################                     

if(yazdir){
dev.off() }
}

if(satsutciz){
mtext(eksenbas_x, side=1, line=5, cex=par("cex")+0.4, outer = TRUE)
mtext(eksenbas_y, side=2, line=6, cex=par("cex")+0.4, las=0, outer = TRUE)}
