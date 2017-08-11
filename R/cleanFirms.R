#' Cégnevek tisztítása
#'
#' A cégneveket egységesíti, pl.: NyRT -> RT
#'
#' @param x karakter vektor
#'
#' @section TODO:
#' Javítani, finomítani a keresendő mintákat
#'
#' @author
#' András Tajti <atajti@andego.hu>
#'
#' @examples
#' cleanFirms(c("Andego Tanácsadó Korlátolt felelősségű társaság",
#'              "Magyar Telekom Nyrt"))
#'
#' @export

cleanFirms <- function(x){

  # ha nem nagybetűs, sanszos hogy nincs tisztítva:
  if(any(sapply(!(x == toupper(x)), isTRUE))){
    x <- toupper(unAccent(x))
    # stop("Kisbetűs szövegrészletet találtam. Tisztítva van a név?")
  }

  # '&amp; sokszor előkerül:
  clean_firm_vect <- x
  clean_firm_vect <- gsub("&AMP;", "&", clean_firm_vect, fixed=TRUE)

  # KFT
  for(pattern in c("KORLATOLT FELELOSSEGU TARSASAG",
                   "KORLATOLT FELELOSSEGU")){
    clean_firm_vect[grepl(pattern,
                          clean_firm_vect)] <- gsub(pattern,
                                                    "KFT",
                                                    clean_firm_vect[grepl(pattern,
                                                            clean_firm_vect)])
  }

  # BT
  for(pattern in c("BETETI TARSASAG")){
    clean_firm_vect[grepl(pattern,
                          clean_firm_vect)] <- gsub(pattern,
                                                    "BT",
                                                    clean_firm_vect[grepl(pattern,
                                                            clean_firm_vect)])
  }

  # KHT
  for(pattern in c("KOZHASZNU TARSASAG")){
    clean_firm_vect[grepl(pattern,
                          clean_firm_vect)] <- gsub(pattern,
                                                    "KHT",
                                                    clean_firm_vect[grepl(pattern,
                                                            clean_firm_vect)])
  }

  # RT
  for(pattern in c("ZRT", "NYRT",
                   "ZARTKORUEN MUKODO RESZVENYTARSASAG",
                   "NYILVANOSAN MUKODO RESZVENYTARSASAG",
                   "RESZVENYTARSASAG",
                   "ZARTKORUEN MUKODO RESZTVENYTAR",
                   "ZARTKORUEN MUKODO RT")){
    clean_firm_vect[grepl(pattern,
                          clean_firm_vect)] <- gsub(pattern,
                                                    "RT",
                                                    clean_firm_vect[grepl(pattern,
                                                            clean_firm_vect)])
  }

  if(any(grepl("RESZVENY|TARSASAG|KORLAT|BETET", clean_firm_vect))){
    warning(paste0("Lehet hogy maradt benne \u00E1talak\u00EDtand\u00F3. Els\u0151: ",
                   min(which(grepl("RESZVENY|TARSASAG|KORLAT|BETET",
                                   clean_firm_vect)))))
  }

  return(clean_firm_vect)

} 