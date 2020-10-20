#########################################################################################
#                      -- KeplerVeriAralikAc.R - 23.02.2018 --                          #
# HESAP:: Evrelendirme yapar, evre cozunurlugu <yuvarla_cozunurluk> ile kontrol         #
#         edilir. ... girdi dosyalarini <ciktiDizini>'ne kaydeder.                #
# CIZIM:: YOK                                                                           #
# CIKTI::                                                                               #
#########################################################################################

### --BUYUK HARFLERLE-- YAZILAN BASLIKLAR ICINDEKI GIRDILER DUZENLENMELIDIR. Kucuk Harflerle yazilan basliklar altinda islemler yapilir. ###

####################
### Kutuphaneler ###
library(reader)
#library(scales)

###################
### --AYARLAR-- ###
setwd("D:/Akademik/TEZ/DR/ExoLCA/Kepler485 (KOI186)/Veri/Kepler485SC/Extracted Data/")
dosyalar <- list.files(path = ".", pattern = "*.tbl", all.files = FALSE, include.dirs = FALSE) #Q04SC*.*.tbl
hedefAdi <- "Kepler485"  # "Kepler485Q04"
veriTuru <- "SC"
ciktiDizini <- "Phased"
evrelendir <- T

# DÝKKAT: Cikti dizinindeki *Run1.dat dosyalari program her calistirildiginda silinir!
if(dir.exists(ciktiDizini)){ unlink(paste0(ciktiDizini,"/*.tbl"), force=TRUE) } else { dir.create(ciktiDizini, showWarnings = TRUE, recursive = FALSE) }

# Normalizasyon Degerleri Dosyasi 
# Normalizasyonlar, [TumVeridenIsikEgrisiAcma] programiyla yapilabilir.
normalizet <- T
normalizasyonDegerDosyasi <- "Kepler485SC_NormDegerleri.in"  
evrelendir_aralik <- c(-0.03,0.03) # yazdirilacak evre araligini belirler.
yuvarla_cozunurluk <- 8 # evrenin noktadan sonra kac haneli hesaplanacagini belirler.
T_on <- 2454833.0000000 # Kepler icin = 2454833.
TBKJD <- F # Eger 'T' ise T_on olmadan yazilir. 'F' ise BJD olarak yani T=T_cevrim+T_on
T_cevrimDosyasi <- paste0(hedefAdi,veriTuru,"-T0.out")
if(file.exists(T_cevrimDosyasi)){file.remove(T_cevrimDosyasi)}
# Evrelendirilen aralik kullanilarak tum veri tek bir dosyaya yazdirilir.
TumVeriDosyasi <- paste0(hedefAdi,veriTuru,"-TumVeri[",evrelendir_aralik[2],"].out")
if(file.exists(TumVeriDosyasi)){file.remove(TumVeriDosyasi)}

# OKUNACAK VE ISLENECEK SÜTUNLAR
sutun <- c(1, 2, 3)
evre_eklenecek_sutun <- 4

# DONEM SAYISI
donem_sayi <- 1  # Bastan itibaren kac donem degeri kullanilacagini belirler.
kod <- c(); P <- c(); T0 <- c(); # evrelendir_aralik <- c()
# cift [[]] kullanimiyla cok boyutlu vektor atamasi yapilabilir.

# DONEMLER # Sadece tek bir evrelendirme icin calisir. Coklu islem icin sonraki surumlerde kullanilacak
kod[1] <- "b"
P[1] <- 3.243259796
T0[1] <- (2454966.6690442 - T_on)
#evrelendir_aralik[[1]] <- c(-0.5,0.5)
kod[2] <- "c"
P[2] <- 27.402900
T0[2] <- 873.475000
#evrelendir_aralik[[2]] <- c(-0.0066,0.0066)
kod[3] <- "d"
P[3] <- 52.090200
T0[3] <- 888.011000
#evrelendir_aralik[[3]] <- c(-0.0052,0.0052)
kod[4] <- "e"
P[4] <- 81.065900
T0[4] <- 869.126000
#evrelendir_aralik[[4]] <- c(-0.0033,0.0033)
### --AYARLAR-- ###
###################

# DONGU: i - dosyalar
for(i in 1:length(dosyalar)){
donem_atla <- 0

  for(k in 1:donem_sayi){
  veri <- read.table(dosyalar[i], header=FALSE, sep = "\t", stringsAsFactors=FALSE)

    for(j in 1:donem_sayi){
#    if(j==donem_atla) next # donem_atla numarali iterasyonu gec.

    # Evrelendirme (-0.5 ile 0.5 arasinda)
    evre <- (veri[,sutun[1]]-T0[j])/P[j] - as.integer((veri[,sutun[1]]-T0[j])/P[j]) # as.integer, yapisinin Excel ile farkliligi nedeniyle evre hesabi Excel'den farklidir.
    veri[,evre_eklenecek_sutun] <- round(ifelse(evre<(-0.5), evre+1, ifelse(evre>(0.5), evre-1, evre)), yuvarla_cozunurluk)

#    plot(veri[,1][1:which(veri[,evre_eklenecek_sutun] == evrelendir_aralik[2])[1] * 4], veri[,2][1:which(veri[,evre_eklenecek_sutun] == evrelendir_aralik[2])[1] * 4], main=dosyalar[i], xlab="BKJD", ylab="Flux")
    
    # verinin BJD araliklari
    BKJD_aralik <- paste0(round(veri[1,1],0),"-",round(veri[length(veri[,1]),1],0))
    
    # Cevrime gore hesaplanan T zamani
    cevrim <- as.integer((veri[1,1]-T0[j])/P[j])
    T_cevrim <- (P[j] * cevrim + T0[j]) # + T_on  # ARA: R'da buyuk sayilarla calisma!
    if(TBKJD){ Time <- T_cevrim } else{ Time <- T_cevrim+T_on }
    write(paste0(strsplit(basename(dosyalar[i]), "\\.")[[1]][1],"\t",BKJD_aralik,"\t",Time), file=T_cevrimDosyasi, sep="\n", append=TRUE)
    
    # veri istenilen araliga sinirlandiriliyor.
    veri <- veri[veri[,4] > evrelendir_aralik[1] & veri[,4] < evrelendir_aralik[2],]
    
    # veri normalize ediliyor
    if(normalizet){
    Norm <- as.numeric(n.readLines(normalizasyonDegerDosyasi, 1, skip = i-1, header = FALSE))
    metin_flux <- "normalized to 1"
    } else {Norm <- 1; metin_flux <- "raw flux"}
    y <- veri[,2]/Norm
    e <- veri[,3]/Norm
    
    # veri yazdirilmak uzere hazirlaniyor
    ciktiAdi <- paste0(hedefAdi,veriTuru,BKJD_aralik,"P",strsplit(as.character(evrelendir_aralik[2]),"\\.")[[1]][2])
    ciktiDosyasi <- paste0(ciktiDizini,"/",i,"_",ciktiAdi,".tbl")
    
    # Sutunlar birlestiriliyor
    if(evrelendir){x <- veri[,4]} else{x <- veri[,1]}
    m <- cbind(x,y,e)
    # evreye gore siralaniyor
    m <- m[order(m[,1]),]           
   
    #Dosya islemleri
    if(file.exists(ciktiDosyasi)){file.remove(ciktiDosyasi)}
    file.create(ciktiDosyasi)
    write(paste0(hedefAdi,kod[j],", ",strsplit(basename(dosyalar[i]), "\\SC")[[1]][1]," ", veriTuru, ", ", metin_flux, "\n", "BKJD ", round(veri[1,1],3)," - ",round(veri[length(veri[,1]),1],3)," / T",ifelse(TBKJD,"[BKJD] = ","[BJD] = "),Time,"\n"), file=ciktiDosyasi)
    

    # Her araliga iliskin dosyalar teker teker yaziliyor 
    write.table(m, file=ciktiDosyasi, col.names=FALSE, row.names=FALSE, quote=FALSE, append=TRUE, sep = "\t")
   
    }
  }   
  # Butun veriyi icerir bir dosyaya yaziliyor
  write.table(m, file=TumVeriDosyasi, col.names=FALSE, row.names=FALSE, quote=FALSE, append=TRUE, sep = "\t")
}

# TumVeri dosyasi yeniden aciliyor ve ilk sutuna [evreye] gore sýralanýyor.
TumVeri <- read.table(TumVeriDosyasi, header=FALSE, sep = "\t", stringsAsFactors=FALSE)
TumVeri <- TumVeri[order(TumVeri[,1]),]
write.table(TumVeri, file=TumVeriDosyasi, col.names=FALSE, row.names=FALSE, quote=FALSE, append=FALSE, sep = "\t")  
  
####################
### --KOD SONU-- ###
winDialog("ok", paste0("Kod Sonlandi!","\n\nBILGI: ",
  "\nIslenen Dosya Top.Sayisi = ", length(dosyalar)))
  #"\nAtilan Satir Top.Sayisi = ", TopGecersizSayisi))
### --KOD SONU-- ###
####################