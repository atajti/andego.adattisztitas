#' Dátumtisztíítás
#'
#' Dátumok formátumának ellenőrzése. Erre lehet, hogy a \code{libridate}
#'   csomag eleve jobb.
#'
#' @param chr_vect karakter vektor
#' @param correct_yy_flag logikau érték, készámjegyű évszámok kezelésére.
#'   Alapvetően \code{TRUE}
#'
#' @details
#' A függvény megkeresi pár előre ismert formátumú dárumokat, de gőzöm
#'   sincs mit csinál velük.
#' A \code{correct_yy_flag} a két évszámból álló éveket kiegészíti:
#'   ha nagyobb, mint az aktuáis év utolsó két szánjegye, feltételezi,
#'   hogy a XX. századra vonatkozik.
#'
#' @return
#' \code{x}-szel megegyező hosszú Dátum vektor
#'
#' @author
#' Tajti András
#'
#' @examples
#' cleanDates("56-10-23")
#'
#' @export


cleanDates <- function(chr_vect, correct_yy_flag = TRUE){

  date_vect <- rep(NA_character_, length(chr_vect))

  YYYY_MM_DD <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}.*"
  YY_MM_DD <- "^[0-9]{2}-[0-9]{2}-[0-9]{2}.*"
  YYMMDD <- "^[0-9]{2}[0-9]{2}[0-9]{2}.*"
  YYYYMMDD <- "^[0-9]{4}[0-9]{2}[0-9]{2}.*"
  YYYY._MM._DD <- "^[0-9]{4}\\. [0-9]{2}\\. [0-9]{2}.*"
  YY._MM._DD <- "^[0-9]{2}\\. [0-9]{2}\\. [0-9]{2}.*"

  # kiszedem a betűket, azok tuti nem kellenek:
  chr_vect_mod <- gsub("[[:alpha:]]", "", chr_vect)

  # a két számjegyű évekkel kezdődők javítása:
  # Ha nagyobb mint jelenév, akkor 20.század.
  if(correct_yy_flag){
    correct_yy <- function(yy_date_chrs){
      yr <- substr(yy_date_chrs, 0, 2)
      prefix <- ifelse(as.numeric(yr) > substr(Sys.Date(), 3,4),
                       19,
                       20)
      corrected_yy <- paste0(prefix, yy_date_chrs)
      return(corrected_yy)
    }

    chr_vect_mod[grepl(YY_MM_DD,
                   chr_vect_mod)] <- correct_yy(chr_vect_mod[grepl(YY_MM_DD,
                                                           chr_vect_mod)])

    chr_vect_mod[grepl(YYMMDD,
                   chr_vect_mod)] <- correct_yy(chr_vect_mod[grepl(YYMMDD,
                                                           chr_vect_mod)])

    chr_vect_mod[grepl(YY._MM._DD,
                   chr_vect_mod)] <- correct_yy(chr_vect_mod[grepl(YY._MM._DD,
                                                           chr_vect_mod)])

  }

  
  # YYYY_MM_DD
  date_vect[grepl(YYYY_MM_DD,
                  chr_vect_mod)] <- str_extract(chr_vect_mod[grepl(YYYY_MM_DD,
                                                           chr_vect_mod)],
                                            substr(YYYY_MM_DD,
                                              0,
                                              nchar(YYYY_MM_DD) - 2))

  # YYYYMMDD
  corrected_yyyymmdd <- str_extract(chr_vect_mod[grepl(YYYYMMDD,
                                                           chr_vect_mod)],
                                            substr(YYYYMMDD,
                                              0,
                                              nchar(YYYYMMDD) - 2))
  corrected_yyyymmdd <- paste0(substr(corrected_yyyymmdd, 0, 4),
                               "-",
                               substr(corrected_yyyymmdd, 5, 6),
                               "-",
                               substr(corrected_yyyymmdd, 7, 8))

  date_vect[grepl(YYYYMMDD,
                  chr_vect_mod)] <- corrected_yyyymmdd

  # YYYY._MM._DD
  corrected_yyyy._mm._dd <- str_extract(chr_vect_mod[grepl(YYYY._MM._DD,
                                                               chr_vect_mod)],
                                                substr(YYYY._MM._DD,
                                                  0,
                                                  nchar(YYYY._MM._DD) - 2))
  corrected_yyyy._mm._dd <- paste0(substr(corrected_yyyy._mm._dd, 0, 4),
                                   "-",
                                   substr(corrected_yyyy._mm._dd, 7, 8),
                                   "-",
                                   substr(corrected_yyyy._mm._dd, 11, 12))

  date_vect[grepl(YYYY._MM._DD,
                  chr_vect_mod)] <- corrected_yyyy._mm._dd

  # ha van NA, aminek nem kéne annak lenni, akkor hibaüzenet
  if(length(setdiff(which(is.na(date_vect)),
                    which(is.na(chr_vect))))){
    warning(paste0("N\u00E8h\u00E0ny d\u00E0tumot nem siker\u00FClt \u00E8rtelmezni. Az els\u00F6: ",
                   min(setdiff(which(is.na(date_vect)),
                               which(is.na(chr_vect))))))
  }

  return(as.Date(date_vect))

}