#' Cégek keresése
#'
#' Nagybetűs, ékezetmentes nevekben keres gyakran előforduló elemeket
#'   a függvény, amik cégnévre utalnak
#'
#' @param chr_vect egyszerű tisztításon átesett karakter vektor
#'
#' @section TODO:
#' Javítani, finomítani a keresendő mintákat
#'
#' @author
#' András Tajti <atajti@andego.hu>
#'
#' @examples
#' cleanFirms(toupper(unAccent(c("Andego Tanácsadó Korlátolt felelősségű társaság",
#'                               "Magyar Telekom Nyrt",
#'                               "Tajti András"))))
#' @return
#' logikai vektor, TRUE ha cég, FALSE ha személy.
#'
#' @export

detectFirms <-
function(chr_vect){

  if(any(grepl("[a-z]", chr_vect))){
    chr_vect <- toupper(unAccent(chr_vect))
    # warning("Kisbetűk is vannak. Lehet hogy nem tisztítottak? Tedd be clean_names2-be.")
  }

  res <- grepl(paste("ZRT", "NYRT", "KFT", "BT", "RESZVENYTARSASAG",
    "SZOLGALTATO", "KORLATOLT FELELOSSEGU", "BETETI TARSASAG",
    "SZERVIZ", "ALAPITVANY", "TRANS", "[[:digit:]]", "[[:space:]]+AG$",
    "SRL",
    "UGYVED", "IRODA", "LIZING", "GMBH", "KERESKEDELMI", "NEMZETKOZI",
    "AUTO", "BANK", "EGYESULET", "SOLUTION", "LEASE", "ORSZAGOS",
    "HUNGARY", "PENZUGY", "AIRPORT", "INSURANCE", "MAGYARORSZAG", "CO KG",
    "PROMOTION", "LTD", "LIMITED", "MEGYEI JOGU", "ORBITAL", "FINANCE",
    "CITY", "LANDES", "SRO", "CSOMAG", "RECYCL", "PROFORMANCE",
    "KORNYEZET", "CITROEN", "LOGISTIC", "LOGISZTIK", "BKK", "BBS", "SERVICE",
    "LANDRATSAMT", "KORMANY", "ISKOLA", "KORHAZ", "RENDELO",
    "NEDERLAND", "[[_space:]]+SA$", "VERBANDSGEMEINDEVERWALTUNG",
    "SZOVETKEZET", " RT", "OKTATO", "CLUB", "KLUB", "PLC",
    "COMPANY", "ASSISTANCE", "REGIERUNGSPRASIDIUM", "NEMZETI", " EV$",
    "EGYENI VALLALKOZO", "GMB", "PRAT SK PROVIDNA", "NV GUSBRECHTS",
    "MUNICIPALITY", "AOK RHEINLAND", "EGYHHAZ", "GESELLSCHAFT",
    "SUZUKI", "MAZDA", "OPEL", "SZINKRON CAR", "[[:space:]]+CO$",
    "LANDESAMT",
    "RENDORSEG", "SNOP CZ AS", "LANDESSTRASSENBAUBEHORDE", "FAHRZEUG",
    "JASZ.NAGYKUN.SZOLNOK", "SP[[:space:]]?Z[[:space:]]?OO", "WHATWHERE",
    "L V M", "SPA",
    "STATION", "STUDIO", "FINGO NV", "CSOPORT", "XVIII", "FEUERWEHR",
    "GLS", " RENT$", "DISTRIBUTION CZ$", "KOZPONT", " DOO$", "IMMOBIL",
    "ALLGEMEINE", "CORPORATION", "[[:space:]]+CORP$", "TVM.*BV$",
    "ORGANISATION",
    "INTERNATIONAL", "TOLNA.MEGYE", "MINISTERE", "SHELL", "AUSTRIA", 
    "STADTVERWALTUNG", "MAGYARORSZAGI", "SZOLGALAT", "KAROSSZERIA",
    "TAM ES SZOLG KP", "INTEZMENY", "UZEMELTETO", "PARTNER", "LEASING",
    "BEHANDELING", "DIRECT", "VERKEHR", "GROUP", "MGT$", "[[:space:]]AS$",
    "ZARTKORU", "MBH", "STREDISKO SAV", "THW GST", "SZESZIPARI BERENDEZ",
    "LKW", "TUZOLTOSAG", "TOURIST", "ASSOCIATION", "SZERVEZO",
    "KOZVETITO", "LIMITED", "GTLS", "RAIFFEISEN", "EXPRESS", "JAVITO",
    "KGM", "EMASZ", "FORD", "SZENTENDRE", "DES KANTONS",
    "HOLZVERARBEITUNG", "KOZTARSASAG", "NAGYKOVETSEG", "KOZSEG",
    "BURO ALTAL KIJELOLT", "[[:space:]]+KG$", "[[:space:]]+SCA$",
    "INDUSTR", "HONDA", "STADT PASSAU", "[[:space:]]KHT$", "TRAVEL",
    "POLICIJSKA", "POLICIE", "GODOLLO", "SNC", "MBVD", "BAZ MEGYE",
    "YAMAHA", "GAZDASAG", "MUNKA", "KOZOSSEG", "VALLALAT", "VILLAMOSSAG",
    "TECHNIKA", "CENTRAL", "MUVEK", "ELEKTRO",
    sep="|"), chr_vect)
    
    res[is.na(chr_vect)] <- NA
    
    return(res)

}
