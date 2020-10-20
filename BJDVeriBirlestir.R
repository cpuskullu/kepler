#########################################################################################
#                       -- BJDVeriBirlestir.R - 31.08.2017 --                           #
# HESAP:: Evrelendirme yapar, evre cozunurlugu <yuvarla_cozunurluk> ile kontrol         #
#         edilir. WinFitter girdi dosyalarini <ciktiDizini>'ne kaydeder.                #
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
setwd("D:/Akademik/TEZ/DR/ExoLCA/Kepler491 (KOI201)/Veri/Kepler491LC/Extracted Data/")
dosyalar <- list.files(path = ".", pattern = "*.tbl", all.files = FALSE, include.dirs = FALSE) #Q04SC*.*.tbl
hedefAdi <- "Kepler491"  # "Kepler485Q04"
veriTuru <- "LC"

# Normalizasyon Degerleri Dosyasi 
# Normalizasyonlar, [TumVeridenIsikEgrisiAcma] programiyla yapilabilir.
normalizasyonDegerDosyasi <- "Kepler491LC_NormDegerleri.in"  
evrelendir_aralik <- c(-0.015,0.015) # yazdirilacak evre araligini belirler.
aralikGecis <- F # Araligin ici mi (gecis) yoksa disi mi alinacak?
yuvarla_cozunurluk <- 8 # evrenin noktadan sonra kac haneli hesaplanacagini belirler.
T_on <- 2454833.0000000 # Kepler icin = 2454833.
TBKJD <- F # Eger 'T' ise T_on olmadan yazilir. 'F' ise BJD olarak yani T=T_cevrim+T_on
T_cevrimDosyasi <- paste0(hedefAdi,veriTuru,"-T0.out")
if(file.exists(T_cevrimDosyasi)){file.remove(T_cevrimDosyasi)}
# Evrelendirilen aralik kullanilarak tum veri tek bir dosyaya yazdirilir.
TumVeriDosyasi <- paste0(hedefAdi,veriTuru,"-BJDTumVeri[",evrelendir_aralik[2],"].out")
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
P[1] <- 4.225384512
T0[1] <- (2454970.5606743 - T_on)

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
    
    # verinin BJD araliklari
    BKJD_aralik <- paste0(round(veri[1,1],0),"-",round(veri[length(veri[,1]),1],0))
    
    # Cevrime gore hesaplanan T zamani
    cevrim <- as.integer((veri[1,1]-T0[j])/P[j])
    T_cevrim <- (P[j] * cevrim + T0[j]) # + T_on  # ARA: R'da buyuk sayilarla calisma!
    if(TBKJD){ Time <- T_cevrim } else{ Time <- T_cevrim+T_on }
    write(paste0(strsplit(basename(dosyalar[i]), "\\.")[[1]][1],"\t",BKJD_aralik,"\t",Time), file=T_cevrimDosyasi, sep="\n", append=TRUE)
    
    # veri istenilen araliga sinirlandiriliyor.
    if(aralikGecis){
      veri <- veri[veri[,4] > evrelendir_aralik[1] & veri[,4] < evrelendir_aralik[2],]
    } else {
      veri <- veri[veri[,4] < evrelendir_aralik[1] | veri[,4] > evrelendir_aralik[2],]
    }
    # veri normalize ediliyor
    Norm <- as.numeric(n.readLines(normalizasyonDegerDosyasi, 1, skip = i-1, header = FALSE))
    y <- veri[,2]/Norm
    e <- veri[,3]/Norm
  
    # Sutunlar birlestiriliyor     
    m <- cbind(veri[,1],y,e,veri[,4])
    # ilk sutuna gore siralaniyor
    m <- m[order(m[,1]),]      

    }
  }   
  # Butun veriyi icerir bir dosyaya yaziliyor
  write.table(m, file=TumVeriDosyasi, col.names=FALSE, row.names=FALSE, quote=FALSE, append=TRUE, sep = "\t")
}
# TumVeri dosyasi yeniden aciliyor ve ilk sutuna [BJD] gore sýralanýyor.
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