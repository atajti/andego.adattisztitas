#' Cégnevek tisztítása
#'
#' Nagybetűs, ékezetmentes cégnevekben keres gyakran előforduló elemeket
#'   a függvény, amiket egységes alakra hoz
#'
#' @param x egyszerű tisztításon átesett karakter vektor
#'
#' @section TODO:
#' Javítani, finomítani a keresendő mintákat
#'
#' @author
#' András Tajti <atajti@andego.hu>
#'
#' @examples
#' cleanFirms(toupper(unAccent(c("Andego Tanácsadó Korlátolt felelősségű társaság",
#'                               "Magyar Telekom Nyrt"))))
#'
#' @export

cleanFirms <- function(x){

  # ha nem nagybetűs, sanszos hogy nincs tisztítva:
  if(any(sapply(!(x == toupper(x)), isTRUE))){
    stop("Kisbetűs szövegrészletet találtam. Tisztítva van a név?")
  }

  clean_firm_vect <- x

  # KFT
  for(pattern in c("KORLATOLT FELELOSSEGU TARSASAG",
                   "KORLATOLT FELELOSSEGU")){
    clean_firm_vect[grepl(pattern,
                          clean_firm_vect)] <- gsub(pattern,
                                                    "KFT",
                                                    x[grepl(pattern,
                                                            clean_firm_vect)])
  }

  # BT
  for(pattern in c("BETETI TARSASAG")){
    clean_firm_vect[grepl(pattern,
                          clean_firm_vect)] <- gsub(pattern,
                                                    "BT",
                                                    x[grepl(pattern,
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
                                                    x[grepl(pattern,
                                                            clean_firm_vect)])
  }

  if(any(grepl("RESZVENY|TARSASAG|KORLAT|BETET", clean_firm_vect))){
    warning(paste0("Lehet hogy maradt benne \u00E1talak\u00EDtand\u00F3. Els\u0151: ",
                   min(which(grepl("RESZVENY|TARSASAG|KORLAT|BETET",
                                   clean_firm_vect)))))
  }

  return(clean_firm_vect)

} 