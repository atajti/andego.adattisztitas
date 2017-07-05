#' Utcanevek tisztítása
#'
#' Megtisztítja a kapott irányítószámokat potenciális problémáktól.
#'
#' @param x karakter vektor
#' @param alternative unAccent használatát szabályzó logikai érték
#'
#' Az \code{unAccent} alapvetően az \code{iconv} függvényt használja.
#'   Ez az operácoós rendszer kódolását veszi alapul, így ha
#'   \code{Encoding(x)) nem ugyan az, mint ami az operációs rendszerből
#'   következik, hibás eredményt adhat; erre az esetre használható
#'   az \code{alternative} flag, lásd \code{\link{unAccent}}
#'
#' @section TODO:
#' Jelenleg a gyakori hibás utcanevek a kódba vannak égetve,
#'   ezt később érdemes volna modulárisan cserélhetővé tenni.
#'
#' @return
#' \code{x}-szelel azonos hosszú karakter vektor
#'
#' @author
#' Hajdú László
#'
#' @examples
#' cleanStreet(" 3120 Dorgalfalva, Kossuth utca 10.")
#'
#' @export
#' @importFrom stringr str_extract


cleanStreet <- function(x, alternative=FALSE){

  STREET2 <- c("MARCIUS 15"="MARCIUS15",
               "MARC. 15"="MARCIUS15",
               "MAJUS 1"="MAJUS1",
               "MAJ. 1"="MAJUS1",
               "AUGUSZTUS 20"="AUGUSZTUS20",
               "AUG. 20"="AUGUSZTUS20",
               "OKT. 6"="OKTOBER6",
               "OKTOBER 6."="OKTOBER6",
               "OKTOBER 6"="OKTOBER6",
               "OKTOBER 23"="OKTOBER23",
               "OKT. 23"="OKTOBER23",
               "OKTOBER HUSZONHARMADIKA"="OKTOBER23",
               "ALSO HATAR"="ALSOHATAR",
               "ALSO KIKOTO"="ALSOKIKOTO",
               "ALSO NYOMAS"="ALSONYOMAS",
               "ALSO PAPIRGYAR"="ALSOPAPIRGYAR",
               "ALSO SAS"="ALSOSAS",
               "ALSO SVABHEGYI"="ALSOSVABHEGYI",
               "ALSO TOROKVESZ"="ALSOTOROKVESZ",
               "ALSO VOLGY"="ALSOVOLGY",
               "ALSO ZOLDMALI"="ALSOZOLDMALI",
               "II. JANOS"="II.JANOS",
               "II. RAKOCZI"="II.RAKOCZI",
               "I. LASZLO"="I.LASZLO",
               "IV. LASZLO"="IV.LASZLO",
               "III. BELA"="III.BELA",
               "I. ISTVAN"="I.ISTVAN",
               "IV. BELA"="IV.BELA",
               "KIS DIOFA"="KISDIOFA",
               "KIS FUVAROS"="KISFUVAROS",
               "KIS KAROSHID"="KISKAROSHID",
               "KIS KOBANYA"="KISKOBANYA",
               "KIS NYIRFA"="KISNYIRFA",
               "KIS RAKOS"="KISRAKOS",
               "KIS ROKUS"="KISROKUS",
               "KIS SALETROM"="KISSALETROM",
               "KIS SORGYAR"="KISSORGYAR",
               "KIS STACIO"="KISSTACIO",
               "KULSO ARPAD"="KULSOARPAD",
               "KULSO BACSAI"="KULSOBACSAI",
               "KULSO CSABAI"="KULSOCSABAI",
               "KULSO FOTI"="KULSOFOTI",
               "KULSO LETAI"="KULSOLETAI",
               "KULSO MESTER"="KULSOMESTER",
               "KULSO SAGI"="KULSOSAGI",
               "KULSO SAMSONI"="KULSOSAMSONI",
               "KULSO SZILAGYI"="KULSOSZILAGYI",
               "KULSO VASUT"="KULSOVASUT",
               "KULSO VESZPREMI"="KULSOVESZPREMI",
               "KULSO VOROSMARTY"="KULSOVOROSMARTY",
               "MAGYAR ASSZONYOK"="MAGYARASSZONYOK",
               "MAGYAR JAKOBINUSOK"="MAGYARJAKOBINUSOK",
               "MAGYAR LAJOS"="MAGYARLAJOS",
               "MAGYAR LASZLO"="MAGYARLASZLO",
               "MAGYAR NOBEL-DIJASOK"="MAGYARNOBELDIJASOK",
               "MAGYAR SZARNYAK"="MAGYARSZARNYAK",
               "MAGYAR TUDOSOK"="MAGYARTUDOSOK",
               "MARIA KIRALYNE"="MARIAKIRALYNE",
               "MARIA TEREZIA"="MARIATEREZIA",
               "NAGY BALOGH"="NAGYBALOGH",
               "NAGY DIOFA"="NAGYDIOFA",
               "NAGY FERENC"="NAGYFERENC",
               "NAGY FUVAROS"="NAGYFUVAROS",
               "NAGY GYORI"="NAGYGYORI",
               "NAGY IGNAC"="NAGYIGNAC",
               "NAGY IMRE"="NAGYIMRE",
               "NAGY JENO"="NAGYJENO",
               "NAGY KAROSHID"="NAGYKAROSHID",
               "NAGY KEREKHEGY"="NAGYKEREKHEGY",
               "NAGY KIRALY"="NAGYKIRALY",
               "NAGY KOBANYA"="NAGYKOBANYA",
               "NAGY LAJOS"="NAGYLAJOS",
               "NAGY LASZLO"="NAGYLASZLO",
               "NAGY MIHALY"="NAGYMIHALY",
               "NAGY PAL"="NAGYPAL",
               "NAGY SANDOR"="NAGYSANDOR",
               "NAGY TEMPLOM"="NAGYTEMPLOM",
               "SZABO BELA"="SZABOBELA",
               "SZABO DEZSO"="SZABODEZSO",
               "SZABO ERVIN"="SZABOERVIN",
               "SZABO ILONKA"="SZABOILONKA",
               "SZABO ISTVAN"="SZABOISTVAN",
               "SZABO JANOS"="SZABOJANOS",
               "SZABO JOZSEF"="SZABOJOZSEF",
               "SZABO KALMAN"="SZABOKALMAN",
               "SZABO KAROLY"="SZABOKAROLY",
               "SZABO LORINC"="SZABOLORINC",
               "SZABO PAL"="SZABOPAL",
               "SZENT ADALBERT"="SZENTADALBERT",
               "SZENT ANNA"="SZENTANNA",
               "SZENT ANTAL"="SZENTANTAL",
               "SZENT BENEDEK"="SZENTBENEDEK",
               "SZENT BERTALAN"="SZENTBERTALAN",
               "SZENT BORBALA"="SZENTBORBALA",
               "SZENT CSALAD"="SZENTCSALAD",
               "SZENT ERZSEBET"="SZENTERZSEBET",
               "SZENT FERENC"="SZENTFERENC",
               "SZENT FLORIAN"="SZENTFLORIAN",
               "SZENT GELLERT"="SZENTGELLERT",
               "SZENT GYORGY"="SZENTGYORGY",
               "SZENT IMRE"="SZENTIMRE",
               "SZENT ISTVAN"="SZENTISTVAN",
               "SZENT JANOS"="SZENTJANOS",
               "SZENT JOZSEF"="SZENTJOZSEF",
               "SZENT KERESZT"="SZENTKERESZT",
               "SZENT KORONA"="SZENTKORONA",
               "SZENT KRISTOF"="SZENTKRISTOF",
               "SZENT LAJOS"="SZENTLAJOS",
               "SZENT LASZLO"="SZENTLASZLO",
               "SZENT LORINC"="SZENTLORINC",
               "SZENT MIHALY"="SZENTMIHALY",
               "SZENT MIKLOS"="SZENTMIKLOS",
               "SZENT MOR"="SZENTMOR",
               "SZENT ORBAN"="SZENTORBAN",
               "SZENT ROKUS"="SZENTROKUS",
               "SZENT SEBESTYEN"="SZENTSEBESTYEN")

  #kitöröljük az irányítószámot a stringből
  x <- sub("\\b[0-9]{4}\\b", "",
           toupper(unAccent(x, alternative)))

  #a házszám a következő szám lesz
  hnumber <- str_extract(x,"\\d+")
  #ha nem találtuk meg ne fűzzünk hozzá NA-t a címhez
  hnumber[which(is.na(hnumber))] <- ""
  #a STREET2 -ben található key-eket cseréli a value-ra

  for(i in seq_along(STREET2)){
    value <- STREET2[i]
    key <- names(value)
    value <- unname(value)
    x<-gsub(names(STREET2[i]),
            STREET2[i],
            x)
  }
  
  
  #1 vagy több whitespaceket kicseréli ; karakterre
  x <- gsub("\\s{1,}", ";",
            removeSpecials(x))
  #splitelem az így kapott stringet aztán a 3. helyen lévő szó lesz az utca
  delimited <- strsplit(x, ";")
  street <- sapply(delimited, "[", 3)
  
  return(paste(street,hnumber, sep=" "))
}