#########################################################################################
#                        -- KeplerVeriOKU.R - 08.08.2016 --                             #
# GIRDI:: NEA tarafindan 'Quarter' bazinda veri sayisi uzerinden siniflanmis            #
#         veri setleri: LC (llc_tbl) ve SC (slc_tbl).                                   #
# ISLEM:: Girdi dosyalarin basliklarini okur ve BKJD zamanlarini kulanarak veriyi       #
#         sadece BKJD, AKI, HATA sutunlarina ayirir.                                    #
# CIZIM:: Girdi dosyalarinin BKJD araliklarinda cizim yapar. (png)                      #
# CIKTI:: Girdi dosyasi basliklarindan okunan bilgilerle dosya ismi olusturulur.        #
#         ISLEM ile ayrilan veriyi (tbl) ve grafigi (png) kaydeder.                     #
#         Q##[VERITURU][QTR-ARALIKLARI]_yildizadi                                       #
#########################################################################################

### --BUYUK HARFLERLE-- YAZILAN BASLIKLAR ICINDEKI GIRDILER DUZENLENMELIDIR. Kucuk Harflerle yazilan basliklar altinda islemler yapilir. ###

####################
### Kutuphaneler ###
library(reader)

###################
### --AYARLAR-- ###
setwd("J:/Akademik/Araþtýrma Projeleri/2019 - BAP (Ortak)/Veri/KOI0125/Kepler468SC")
dosyalar <- list.files(path = ".", pattern = "*slc_lc.tbl", all.files = FALSE)
hedefAdi <- "kepler468"
veriTuru <- "SC"

# Ekrana grafiði çizdirilecek sütun numaralarý plot( veri[,#]  vektörü içinde # yerine yazýlmalýdýr.
# OKUNACAK SÜTUNLAR
sutun <- c(1, 9, 10)

# OKUNACAK BASLIK (HEADER) ANAHTARLARI

QTR <- 'QUARTER'  #Quarter No
BASZ <- 'TSTART'  #Baslama Zamani
BITZ <- 'TSTOP'   #Bitis Zamani
Gecersiz <- 'null'  #Geçersiz olan satirlarin deðerleri
### --AYARLAR-- ###
###################

# EKRANA BILGI
length(dosyalar)
veriSayidosyasi <- paste0(hedefAdi,veriTuru,"-VeriSayisi.out")
if (file.exists(veriSayidosyasi)) file.remove(veriSayidosyasi)

for(i in 1:length(dosyalar)){

bilgiler <- readLines(dosyalar[i])  

veriBas <- grep('.* TIME',readLines(dosyalar[i])) #Verinin baþlayacaðý satýr tespit ediliyor. Bunun icin ilgili satira ozgun bir metin veya metin obegi girilmeli.
#gecersiz <- grep(Gecersiz,n.readLines(dosyalar[i],n=600,skip=(veriBas[length(veriBas)]+3)))

qtr <- sprintf("%02d",as.integer(gsub(".*= ","",bilgiler[grep(QTR,readLines(dosyalar[i]))])))
basz <- round(as.numeric(gsub(".*= ","",bilgiler[grep(BASZ,readLines(dosyalar[i]))])),0)
bitz <- round(as.numeric(gsub(".*= ","",bilgiler[grep(BITZ,readLines(dosyalar[i]))])),0)
ciktiAdi <- paste0("Q",qtr,veriTuru,basz,"-",bitz,"_",hedefAdi)
ciktiDosyasi <- paste0(ciktiAdi,".tbl")

veri <- read.table(dosyalar[i], header=FALSE, skip=(veriBas[length(veriBas)]+3), sep = "", stringsAsFactors=FALSE)
length(veri[,sutun[1]])

##########################
### --GRAFIGI YAZDIR-- ###
# pdf(), png(), postscript(): saydamligi desteklemez: pdf'i eps'ye cevir.
yazdir <- 1 
if(yazdir){
png(paste0(ciktiAdi,".png"))#, width = 40, height = 30)
# En altta dev.off komutunu kapatmali; bu nedenle 'yazdir' degiseni ile kontrol ediliyor.
}
### --GRAFIGI YAZDIR-- ###
##########################
 
plot(veri[,sutun[1]], veri[,sutun[2]], main=ciktiAdi, 
  xlab=trimws(unlist(strsplit(bilgiler[veriBas],"|",fixed=T)))[sutun[1]+1], 
  ylab=trimws(unlist(strsplit(bilgiler[veriBas],"|",fixed=T)))[sutun[2]+1]
)

if(yazdir){
dev.off() }

# WRITING OUTPUT
  #library(stringr)  str_trim(veri[j,sutun[k]], side="both")             
  sutunlar <- ''  # alinacak sutunlar degiskeni bos tanimlaniyor.
  GecersizSatir = c() # atilacak satirlarin numaralarinin tutuldugu vektor tanimlaniyor.
  
  for(k in 1:length(sutun)) {    
  sutunlar <- paste0(sutunlar,"veri[,",sutun[k],"]") 
  if(k != length(sutun)) {sutunlar <- paste0(sutunlar,", ")}

    GecersizSayaci <- 0        
    for(h in 1:length(veri[,sutun[1]])){ 
      if(veri[h,sutun[k]] == Gecersiz) { 
        GecersizSatir <- c(GecersizSatir ,h)
        GecersizSayaci <- GecersizSayaci + 1 }}
   # TopGecersizSayisi <- TopGecersizSayisi + GecersizSayaci  
  }
  veri <- veri[-sort(unique(GecersizSatir)), ] # atilacak satirlar cikariliyor.
  
	m  <- eval(parse(text = paste("cbind(", sutunlar, ")")))
  #if(qtr == )qtrBazli <- TRUE
	write.table(m, file=ciktiDosyasi, col.names=FALSE, row.names=FALSE, quote=FALSE, append=FALSE, sep = "\t")

# Kaydedilen her dosyanin veri sayisi yaziliyor.
  write(paste0("Q",qtr,veriTuru,"\t",length(m)), file=veriSayidosyasi, append=T, sep="\n")
 
} # Dosyalar dongu sonu: for()
                           
####################
### --KOD SONU-- ###
winDialog("ok", paste0("Kod Sonlandý!","\n\nBÝLGÝ: ",
  "\nÝþlenen Dosya Top.Sayýsý = ", length(dosyalar))) 
  #"\nAtýlan Satýr Top.Sayýsý = ", TopGecersizSayisi))
### --KOD SONU-- ###
####################
