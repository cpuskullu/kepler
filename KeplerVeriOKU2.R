#########################################################################################
#                        -- KeplerVeriOKU2.R - 08.08.2016 --                            #
# GIRDI:: NEA Arayüzünden çekilen veri *.tbl formatýnda                                 #
# ISLEM:: Ýki sütunlu veriyi okur, (arayüz hatasýný vermiyor)                           #
# CIZIM:: Girdi dosyalarinin BKJD araliklarinda cizim yapar. (png)                      #
# CIKTI:: Girdi dosyasi basliklarindan okunan bilgilerle dosya ismi olusturulur.        #
#         ISLEM ile ayrilan veriyi (tbl) ve grafigi (png) kaydeder.                     #
#         Q##[VERITURU][QTR-ARALIKLARI]_yildizadi                                       #
#########################################################################################

### --BUYUK HARFLERLE-- YAZILAN BASLIKLAR ICINDEKI GIRDILER DUZENLENMELIDIR. Kucuk Harflerle yazilan basliklar altinda islemler yapilir. ###

####################
### Kutuphaneler ###
#library(reader)

###################
### --AYARLAR-- ###
setwd("J:/Akademik/Araþtýrma Projeleri/2019 - BAP (Ortak)/Veri/KOI913 (K723b)")
dosyalar <- list.files(path = ".", pattern = "*LC.tbl", all.files = FALSE)
ciktiDosyasi <- "kepler723.tbl"
#veriTuru <- "LC"

for(i in 1:length(dosyalar)){

veri <- read.table(dosyalar[i], header=FALSE, sep = "", stringsAsFactors=FALSE)

}

##########################
### --GRAFIGI YAZDIR-- ###

#plot(veri[,2], veri[,3])

#########################
### --WRITING OUTPUT-- ##
m <- cbind(veri[,2], veri[,3])

write.table(m, file=ciktiDosyasi, col.names=FALSE, row.names=FALSE, quote=FALSE, append=FALSE, sep = "\t")

####################
### --KOD SONU-- ###
winDialog("ok", paste0("Kod Sonlandý!","\n\nBÝLGÝ: ",
  "\nÝþlenen Dosya Top.Sayýsý = ", length(dosyalar))) 
  #"\nAtýlan Satýr Top.Sayýsý = ", TopGecersizSayisi))
### --KOD SONU-- ###
####################