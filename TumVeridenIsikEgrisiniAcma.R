#########################################################################################
#                   -- TumVeridenIsikEgrisiniAcma.R - 26.03.2017 --                     #
# GIRDI:: Kepler ham verisi (*.tbl) ya da temizlenmis verisi (*.cbtl)                   #
# ISLEM:: Evrelendirme yapar, evre cozunurlugu <yuvarla_cozunurluk> ile kontrol         #
#         edilir. Verideki tum isik egrilerini tek tek ayirir ve kaydeder.              #
# CIZIM:: Sadece göstermelik cizim yapar, kaydetmez.                                    #
# CIKTI:: Verideki her bir isik egrisini verilen yorunge parametrelerine gore           #
#         ayirir, asil dosyadaki gozlem zamani, isik akisi ve hatasina ek olarak        #
#         evre sutunu acar, dosyalara kaydeder. Dosya isimlerini sonuna ekledigi        #
#         numaralarla siralar. Okunan veriden secili sutunlar: Q##TIME_hedefAdi.tbl     #
#########################################################################################

### --BUYUK HARFLERLE-- YAZILAN BASLIKLAR ICINDEKI GIRDILER DUZENLENMELIDIR. Kucuk Harflerle yazilan basliklar altinda islemler yapilir. ###

####################
### Kutuphaneler ###
library(reader)

###################
### --AYARLAR-- ###
# setwd("D:/Akademik/PROJE-MFAG113F353/KeplerLCs/Kepler13A/Veri/Kepler13SC-temiz")
setwd("J:/Akademik/Araştırma Projeleri/2019 - BAP (Ortak)/Veri/KOI913 (K723b)")
dosyalar <- list.files(path = ".", pattern = "*.tbl", all.files = FALSE) # uzantı: *.ctbl
hedefAdi <- "kepler723"
veriTuru <- "LC"

ciktiDizini <- "Extracted Data"
# DİKKAT: Cikti dizini icinde *.tbl dosyalari program her calistirildiginda silinir!
if(dir.exists(ciktiDizini)){ unlink(paste0(ciktiDizini,"/*.tbl"), force=TRUE) } else { dir.create(ciktiDizini, showWarnings = TRUE, recursive = FALSE) }

# CIKTI NORMALIZASYON DOSYASI
normalizasyonDegerDosyasi <- paste0(ciktiDizini,"/",hedefAdi,veriTuru,"_NormDegerleri.in")
normalizasyonIslemi <- F

# Ekrana grafigi çizdirilecek sütun numaralari plot( veri[,#]  vektörü içinde # yerine yazilmalidir.
# OKUNACAK VE ISLENECEK SÜTUNLAR
sutun <- c(1, 2) #, 3)
evre_eklenecek_sutun <- 3
evresutunekle <- 0  # evre sutunu yazdirilacaksa 1; degilse 0
ToplamVeriSayisi <- c()
  
# DONEM SAYISI
donem_sayi <- 1
kod <- c(); P <- c(); T0 <- c(); ac_aralik <- c()
# cift [[]] kullanimiyla cok boyutlu vektor atamasi yapilabilir.

# DONEMLER 
kod[1] <- "b"
P[1] <- 4.082274889
T0[1] <- 169.6375652 #(2455010.25005 - 2454833)
ac_aralik[[1]] <- c(-0.075,0.075)

# Coklu islem icin sonraki surumlerde kullanilacak
kod[2] <- "c"
P[2] <- 27.402900
T0[2] <- 873.475000
ac_aralik[[2]] <- c(-0.0066,0.0066)
kod[3] <- "d"
P[3] <- 52.090200
T0[3] <- 888.011000
ac_aralik[[3]] <- c(-0.0052,0.0052)
kod[4] <- "e"
P[4] <- 81.065900
T0[4] <- 869.126000
ac_aralik[[4]] <- c(-0.0033,0.0033)
### --AYARLAR-- ###
###################

# Normalizasyon Dosyasi
if(normalizasyonIslemi){if (file.exists(normalizasyonDegerDosyasi)) file.remove(normalizasyonDegerDosyasi)}
    
# Islenen Veri Sayisi dosyasi (IE: Isik Egrisi)
veriSayidosyasi <- paste0(ciktiDizini,"/",hedefAdi,veriTuru,"-IESayisi.out")
if (file.exists(veriSayidosyasi)) file.remove(veriSayidosyasi)

# DONGU: i - dosyalar
for(i in 1:length(dosyalar)){
donem_atla <- 0

  for(k in 1:donem_sayi){
  veri <- read.table(dosyalar[i], header=FALSE, sep = "", stringsAsFactors=FALSE)

    for(j in 1:donem_sayi){
#    if(j==donem_atla) next # donem_atla numarali iterasyonu gec.

    # Evrelendirme (-0.5 ile 0.5 arasinda)
    evre <- (veri[,sutun[1]]-T0[j])/P[j] - as.integer((veri[,sutun[1]]-T0[j])/P[j]) # as.integer, yapisinin Excel ile farkliligi nedeniyle evre hesabi Excel'den farklidir.
    yuvarla_cozunurluk <- 2  # SC verisinde 3 / LC verisinde 1 ve 2 calisiyor. IESayisi.out toplam veri noktasi sayisi ile DataPoints.xlsx tablosundaki veri karşılıklı kontrol edilebilir.
    veri[,evre_eklenecek_sutun] <- round(ifelse(evre<(-0.5), evre+1, ifelse(evre>(0.5), evre-1, evre)), yuvarla_cozunurluk)

    if(normalizasyonIslemi){
      repeat{
      # Tum isik egrisini ekrana cizdirmek icin:
      plot(veri[,1],veri[,2], main=dosyalar[i], xlab="BKJD", ylab="Flux")
      
      # Sadece ilk zamandan baslayip dar bir araligi cizdirmek icin:      
#      plot(veri[,1][1:which(abs(veri[,evre_eklenecek_sutun]-0.5)==min(abs(veri[,evre_eklenecek_sutun]-0.5)))[1] * 20], veri[,2][1:which(abs(veri[,evre_eklenecek_sutun]-0.5)==min(abs(veri[,evre_eklenecek_sutun]-0.5)))[1] * 20], main=dosyalar[i], xlab="BKJD", ylab="Flux")
      mtext("Normalizasyon cizgisini seçin\n Tamamsa diger grafik icin 'z'ye BAS; ayni grafik icin baska tusa BAS", cex=0.6, side = 3, line = 0)     

        tik <- locator(n=1, type = "l")
        abline(h=tik$y, col="red", lty="solid", lwd=3)      
  
      tus <- function(key) {
            if (key == "z") return(invisible(1))           
            else return(invisible(0))
      }      
      cikis <- getGraphicsEvent(prompt = "Diger grafik icin 'z'ye BAS; ayni grafik icin baska tusa BAS", onKeybd = tus)
      if(cikis){print("z'ye basildi, donguden cikildi"); break}
      }
      # normalizasyon degerleri dosyaya yazdiriliyor (quarter dosyasi sayisi kadar)          
      # write(tik$y, file=normalizasyonDegerDosyasi, append=TRUE, , sep="\n")      
    }     
    
    # ARALIK ACMA
    # Araliklar, veriden aciliyor
    ilk_min_satir <- which(veri[,evre_eklenecek_sutun] == 0)[1]
    ilk_min <- veri[,1][which(veri[,evre_eklenecek_sutun] == 0)[1]]
    
    # bulunan ilk min. degerinden ilk isik egrisi baslangici belirleniyor.
    baslangic <- ilk_min - (P[j]/2)
    
    
      if(winDialog("yesno", paste0(dosyalar[i]," dosyasindaki\n","ilk minimum ", ilk_min_satir, ". satirda bulundu.\nBu degerin zamani ", round(ilk_min,yuvarla_cozunurluk),"\'dir.\nDogruysa EVET'e tiklayin.")) == "YES"){

        # verideki isik egrisi sayisi
        verideki_IE_sayisi <- ceiling((veri[length(veri[,1]),1]-veri[,1][which(veri[,evre_eklenecek_sutun] == 0)[1]])/P[j])
        
        # acilacak isik egrileri icin dongu baslatiliyor
        for(m in 1:verideki_IE_sayisi)
        {
          
          # Surum.1 satirlari
          #ac_basla_deger <- round(baslangic + (m-1)*P[j], yuvarla_cozunurluk)
          #ac_bitis_deger <- round(ac_basla_deger + P[j], yuvarla_cozunurluk)
          
          # Surum.2 satirlari
          # isik egrileri icin degerler belirleniyor
          ac_basla_deger_0 <- round(baslangic + (m-1)*P[j], yuvarla_cozunurluk)
          ac_basla_deger <- round((ac_basla_deger_0 + P[j]/2) + (P[j]*ac_aralik[[1]][1]), yuvarla_cozunurluk)
          ac_bitis_deger <- round((ac_basla_deger_0 + P[j]/2) + (P[j]*ac_aralik[[1]][2]), yuvarla_cozunurluk)
          # /Surum.2
          
          # Surum.1 satirlari
          # isik egrilerinin satirlari belirleniyor. m==1 durumu icin 1.satira gidiliyor ve verideki tum onceki satirlar, sonraki isik egrisine ilave ediliyor.
          #ac_basla <- ifelse(m == 1, 1, which(abs(round(veri[,1],yuvarla_cozunurluk)-ac_basla_deger)==min(abs(round(veri[,1],yuvarla_cozunurluk)-ac_basla_deger)))[1])
          
          # Surum.2 satirlari
          # isik egrilerinin satirlari belirleniyor.
#          ac_basla <- which(round(veri[,1], yuvarla_cozunurluk) == ac_basla_deger)[1]
          ac_basla <- which.min(abs(round(veri[,1], yuvarla_cozunurluk)-ac_basla_deger))[1]
          #abs(x - your.number) == min(abs(x - your.number))
          ac_bitis <- ifelse(m == verideki_IE_sayisi, length(veri[,1]), which.min(abs(round(veri[,1],yuvarla_cozunurluk)-ac_bitis_deger))[1])
          # /Surum.2
          
          if(is.na(ac_bitis)){next}
          if(is.na(ac_basla)){ 
              for(t in 1:ortaralik){ 
                if(veri[ac_bitis-t,1] < ac_basla_deger){ 
                  ac_basla <- ac_bitis-t+1 
                  break } 
              } 
            }       

          if(m == 2) { ortaralik <- ac_bitis-ac_basla }
          ortaralik <- ac_bitis-ac_basla          
          
          # Cikti dosyasi adi belirleniyor
          anaDosyaAdi <- strsplit(basename(dosyalar[i]), "\\.")[[1]][1]
          ciktiAdi <- paste0(anaDosyaAdi,"-",m)
          ciktiDosyasi <- paste0(ciktiDizini,"/",ciktiAdi,".tbl")

          # veri, dosyaya yazdiriliyor.
          write.table(veri[ac_basla:ac_bitis,-evre_eklenecek_sutun-evresutunekle], file=ciktiDosyasi, col.names=FALSE, row.names=FALSE, quote=FALSE, append=FALSE, sep = "\t")
          
          # normalizasyon degerleri dosyaya yazdiriliyor (tum cikti dosyasi sayisi kadar)          
          if(normalizasyonIslemi){write(tik$y, file=normalizasyonDegerDosyasi, append=TRUE, , sep="\n")}
          
          # Acilan her dosyanin veri sayisi yaziliyor.
          # AnaDosyaAdi, Acilan(Parcalanan)Dosya Sayisi, DosyadakiVeriSayisi, Zaman Araliklari
          write(paste0("DOSYA ",m,":","\t",anaDosyaAdi,"\t",m,"\t",length(veri[ac_basla:ac_bitis,-evre_eklenecek_sutun-evresutunekle][,1]),"\t",ac_basla_deger,":",ac_bitis_deger), file=veriSayidosyasi, append=TRUE, sep="\n")                    
                                 
        } # DONGU: m SONU
      } else {winDialog("ok", "Koddaki cozunurlugu degistiriniz")} 

    }

  # Okunan her dosyanin veri sayisi yaziliyor.
  # AnaDosyaAdi, Acilan(Parcalanan)Dosya Sayisi, AnaDosyaVeriSayisi
  write(paste0("PARCA TOPLAMI:","\t",anaDosyaAdi,"\t",m,"\t",length(veri[,sutun[1]])), file=veriSayidosyasi, append=TRUE, sep="\n")
  ToplamVeriSayisi <- append(ToplamVeriSayisi,length(veri[,sutun[1]]))

  ##########################
  ### --GRAFIGI YAZDIR-- ###
  # pdf(), png(), postscript(): saydamligi desteklemez: pdf'i eps'ye cevir.
  yazdir <- 0
  if(yazdir){
  png(paste0(ciktiAdi,".png"))#, width = 40, height = 30)
  # En altta dev.off komutunu kapatmali; bu nedenle 'yazdir' degiseni ile kontrol ediliyor.
  }
  ### --GRAFIGI YAZDIR-- ###
  ##########################

  if(yazdir){
  dev.off() }

  } # DONGU: k SONU
} # Dosyalar dongu sonu: for()

write(paste0("TUMVERI TOPLAMI:","\t\t\t\t\t",sum(ToplamVeriSayisi)), file=veriSayidosyasi, append=TRUE, sep="\n")

####################
### --KOD SONU-- ###
winDialog("ok", paste0("Kod Sonlandi!","\n\nBILGI: ",
  "\nIslenen Dosya Top.Sayisi = ", length(dosyalar)))
  #"\nAtilan Satir Top.Sayisi = ", TopGecersizSayisi))
### --KOD SONU-- ###
####################


#########################################################################################
#                       TumVeridenIsikEgrisiniAcma.R                                    #
#                               07.10.2020                                              #
#                         ** Geliştirme Notlari **                                      #
#                                                                                       #
#  s2: ac_aralik() koda eklendi. Boylece isik egrisi, istenilen evre araliginda         #
#      acilabiliyor.                                                                    #   #                                                                                       #
#########################################################################################