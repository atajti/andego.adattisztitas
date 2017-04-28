# library(data.table)
# library(DBI)
# library(RMySQL)
# library(data.table)
# library(stringr)
# # setwd("C:/Users/hajdul/Desktop/Andego-wheeler-cim")
# setwd("/media/munka/Új kötet/ANDEGO/andego.adattisztitas/R")
# adatok <- fread("pelda.csv",header = "auto", sep="auto",encoding="UTF-8")
# varosok = fread("varosok.csv",header = "auto", sep="auto",encoding="UTF-8")

# ##########################KONSTANSOK
             

# STREET2 <- c("MARCIUS 15"="MARCIUS15",
#              "MARC. 15"="MARCIUS15",
#              "MAJUS 1"="MAJUS1",
#              "MAJ. 1"="MAJUS1",
#              "AUGUSZTUS 20"="AUGUSZTUS20",
#              "AUG. 20"="AUGUSZTUS20",
#              "OKT. 6"="OKTOBER6",
#              "OKTOBER 6."="OKTOBER6",
#              "OKTOBER 6"="OKTOBER6",
#              "OKTOBER 23"="OKTOBER23",
#              "OKT. 23"="OKTOBER23",
#              "OKTOBER HUSZONHARMADIKA"="OKTOBER23",
#              "ALSO HATAR"="ALSOHATAR",
#              "ALSO KIKOTO"="ALSOKIKOTO",
#              "ALSO NYOMAS"="ALSONYOMAS",
#              "ALSO PAPIRGYAR"="ALSOPAPIRGYAR",
#              "ALSO SAS"="ALSOSAS",
#              "ALSO SVABHEGYI"="ALSOSVABHEGYI",
#              "ALSO TOROKVESZ"="ALSOTOROKVESZ",
#              "ALSO VOLGY"="ALSOVOLGY",
#              "ALSO ZOLDMALI"="ALSOZOLDMALI",
#              "II. JANOS"="II.JANOS",
#              "II. RAKOCZI"="II.RAKOCZI",
#              "I. LASZLO"="I.LASZLO",
#              "IV. LASZLO"="IV.LASZLO",
#              "III. BELA"="III.BELA",
#              "I. ISTVAN"="I.ISTVAN",
#              "IV. BELA"="IV.BELA",
#              "KIS DIOFA"="KISDIOFA",
#              "KIS FUVAROS"="KISFUVAROS",
#              "KIS KAROSHID"="KISKAROSHID",
#              "KIS KOBANYA"="KISKOBANYA",
#              "KIS NYIRFA"="KISNYIRFA",
#              "KIS RAKOS"="KISRAKOS",
#              "KIS ROKUS"="KISROKUS",
#              "KIS SALETROM"="KISSALETROM",
#              "KIS SORGYAR"="KISSORGYAR",
#              "KIS STACIO"="KISSTACIO",
#              "KULSO ARPAD"="KULSOARPAD",
#              "KULSO BACSAI"="KULSOBACSAI",
#              "KULSO CSABAI"="KULSOCSABAI",
#              "KULSO FOTI"="KULSOFOTI",
#              "KULSO LETAI"="KULSOLETAI",
#              "KULSO MESTER"="KULSOMESTER",
#              "KULSO SAGI"="KULSOSAGI",
#              "KULSO SAMSONI"="KULSOSAMSONI",
#              "KULSO SZILAGYI"="KULSOSZILAGYI",
#              "KULSO VASUT"="KULSOVASUT",
#              "KULSO VESZPREMI"="KULSOVESZPREMI",
#              "KULSO VOROSMARTY"="KULSOVOROSMARTY",
#              "MAGYAR ASSZONYOK"="MAGYARASSZONYOK",
#              "MAGYAR JAKOBINUSOK"="MAGYARJAKOBINUSOK",
#              "MAGYAR LAJOS"="MAGYARLAJOS",
#              "MAGYAR LASZLO"="MAGYARLASZLO",
#              "MAGYAR NOBEL-DIJASOK"="MAGYARNOBELDIJASOK",
#              "MAGYAR SZARNYAK"="MAGYARSZARNYAK",
#              "MAGYAR TUDOSOK"="MAGYARTUDOSOK",
#              "MARIA KIRALYNE"="MARIAKIRALYNE",
#              "MARIA TEREZIA"="MARIATEREZIA",
#              "NAGY BALOGH"="NAGYBALOGH",
#              "NAGY DIOFA"="NAGYDIOFA",
#              "NAGY FERENC"="NAGYFERENC",
#              "NAGY FUVAROS"="NAGYFUVAROS",
#              "NAGY GYORI"="NAGYGYORI",
#              "NAGY IGNAC"="NAGYIGNAC",
#              "NAGY IMRE"="NAGYIMRE",
#              "NAGY JENO"="NAGYJENO",
#              "NAGY KAROSHID"="NAGYKAROSHID",
#              "NAGY KEREKHEGY"="NAGYKEREKHEGY",
#              "NAGY KIRALY"="NAGYKIRALY",
#              "NAGY KOBANYA"="NAGYKOBANYA",
#              "NAGY LAJOS"="NAGYLAJOS",
#              "NAGY LASZLO"="NAGYLASZLO",
#              "NAGY MIHALY"="NAGYMIHALY",
#              "NAGY PAL"="NAGYPAL",
#              "NAGY SANDOR"="NAGYSANDOR",
#              "NAGY TEMPLOM"="NAGYTEMPLOM",
#              "SZABO BELA"="SZABOBELA",
#              "SZABO DEZSO"="SZABODEZSO",
#              "SZABO ERVIN"="SZABOERVIN",
#              "SZABO ILONKA"="SZABOILONKA",
#              "SZABO ISTVAN"="SZABOISTVAN",
#              "SZABO JANOS"="SZABOJANOS",
#              "SZABO JOZSEF"="SZABOJOZSEF",
#              "SZABO KALMAN"="SZABOKALMAN",
#              "SZABO KAROLY"="SZABOKAROLY",
#              "SZABO LORINC"="SZABOLORINC",
#              "SZABO PAL"="SZABOPAL",
#              "SZENT ADALBERT"="SZENTADALBERT",
#              "SZENT ANNA"="SZENTANNA",
#              "SZENT ANTAL"="SZENTANTAL",
#              "SZENT BENEDEK"="SZENTBENEDEK",
#              "SZENT BERTALAN"="SZENTBERTALAN",
#              "SZENT BORBALA"="SZENTBORBALA",
#              "SZENT CSALAD"="SZENTCSALAD",
#              "SZENT ERZSEBET"="SZENTERZSEBET",
#              "SZENT FERENC"="SZENTFERENC",
#              "SZENT FLORIAN"="SZENTFLORIAN",
#              "SZENT GELLERT"="SZENTGELLERT",
#              "SZENT GYORGY"="SZENTGYORGY",
#              "SZENT IMRE"="SZENTIMRE",
#              "SZENT ISTVAN"="SZENTISTVAN",
#              "SZENT JANOS"="SZENTJANOS",
#              "SZENT JOZSEF"="SZENTJOZSEF",
#              "SZENT KERESZT"="SZENTKERESZT",
#              "SZENT KORONA"="SZENTKORONA",
#              "SZENT KRISTOF"="SZENTKRISTOF",
#              "SZENT LAJOS"="SZENTLAJOS",
#              "SZENT LASZLO"="SZENTLASZLO",
#              "SZENT LORINC"="SZENTLORINC",
#              "SZENT MIHALY"="SZENTMIHALY",
#              "SZENT MIKLOS"="SZENTMIKLOS",
#              "SZENT MOR"="SZENTMOR",
#              "SZENT ORBAN"="SZENTORBAN",
#              "SZENT ROKUS"="SZENTROKUS",
#              "SZENT SEBESTYEN"="SZENTSEBESTYEN")

# ##########################SEGÉDFÜGGVÉNYEK

# #karakterkódolást beállítja (ékezetes karakterek cseréje)
# unAccent <- function(x){
#   x=gsub("á","a",x)
#   x=gsub("é","e",x)
#   x=gsub("í","i",x)
#   x=gsub("ó","o",x)
#   x=gsub("ú","u",x)
#   x=gsub("ű","u",x)
#   x=gsub("ő","o",x)
#   x=gsub("ö","o",x)
#   x=gsub("ü","u",x)
  
#   x=gsub("Á","A",x)
#   x=gsub("É","E",x)
#   x=gsub("Í","I",x)
#   x=gsub("Ó","O",x)
#   x=gsub("Ú","U",x)
#   x=gsub("Ű","U",x)
#   x=gsub("Ő","O",x)
#   x=gsub("Ö","O",x)
#   x=gsub("Ü","U",x)
#   return(x)
# }
# #a konstansokban tárolt elemek cseréjére van használva
# constReplacer <- function(regkif,csere,min){
#   return(gsub(regkif,csere,min))
# }
# #visszaadja az első 4 jegyű számot
# toZipCode <- function(x){
#   return(str_extract(x,"\\b[0-9]{4}\\b"))
# }
# #csak a betűk maradhatnak (a számokat kicseréli üres karakterre)
# toAlphaAndDigits <- function(x){
#   return(gsub("[0-9]","",x))
# }
# #a pontokat kiszedi a paraméterből
# removeAllDot <- function(x){
#   return(gsub("\\.", "", x))
# }
# #eltünteti a whitespaceket benne van a trimmelés is
# removeSpaces <- function(x){
#   return(trimws(gsub("[ \n\t\r]+", "", x)))
# }

# #speciális karaktereket tünteti el a sztringből
# removeSpecials <- function(x){
#   return(gsub("[^[:alnum:][:blank:]]", "", x))
# }


# #########################################
# #az irányítószám kiszedésére használatos
# clean_zip <- function(x){
#   return(removeAllDot(toZipCode(unAccent(toupper(x)))))
# }
# #a városnév tisztítására használatos
# clean_city <- function(x){
#   return(removeSpecials(removeAllDot(toupper(unAccent(x)))))
# }

# #az utca kitalálása a maradékból
# clean_street <- function(x){
#   x <- unAccent(x)
#   x <- toupper(x)
#   x <- trimws(x)
#   #kitöröljük az irányítószámot a stringből
#   x <- sub("\\b[0-9]{4}\\b","",x)
#   #a házszám a következő szám lesz
#   hnumber <- str_extract(x,"\\d+")
#   #ha nem találtuk meg ne fűzzünk hozzá NA-t a címhez
#   if(is.na(hnumber)){
#     hnumber=""
#   }
#   #a STREET2 -ben található key-eket cseréli a value-ra
#   for(i in seq_along(STREET2)){
#     value <- STREET2[i]
#     key <- names(value)
#     value <- unname(value)
#     x<-gsub(key,value,x)
#   }
  
  
#   x <- removeSpecials(x);
#   #1 vagy több whitespaceket kicseréli ; karakterre
#   x <- gsub("\\s{1,}",";",x)
#   #splitelem az így kapott stringet aztán a 3. helyen lévő szó lesz az utca
#   delimited <- strsplit(x,";")
#   street <- delimited[[1]][3]
  
#   return(paste(street,hnumber))
# }

# #cím normalizáló
# address_normalizer <- function(x){
#   irszam=clean_zip(x)
#   varos=clean_city(varosok[varosok$postal==irszam]$City[1])
#   street=clean_street(x)
#   return(paste(irszam,varos,street))
# }

# # tesztelő függvény ami paraméterként várja hogy mennyi rekordon tesztelje az adatokból. Teljes: nrow(adatok)
# tester <- function(x){
#   osszes=x
#   jo=0
#   #nrow(adatok)
#   for(i in 1:x){
#     aktualisparam = adatok[i]$value
#     sajatnormalizer = address_normalizer(adatok[i]$value)
    
#     if(sajatnormalizer==adatok[i]$normalized_value) {
#       jo=jo+1
#     }else{
#       #kiírjuk azokat amik nem egyeztek
#       print(sajatnormalizer)
#       print(adatok[i]$normalized_value)
#     }
#   }
#   szazalek=(jo/osszes)*100
#   return(szazalek)
# }

