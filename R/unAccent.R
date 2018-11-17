#' Ékezetek eltávolítása
#'
#' Az ékezetes karaktereket (pl á,é,í) ékezet nélkülire cseréli (a,e,i).
#'
#' @param x karakter vektor
#' @param alternative logikai konstans, alapvetően hamis. Ha igaz,
#'   akkor egy beépített cserekészlettel dolgozik kódolás helyett.
#'
#' @details
#' Az ékezet eltávolítását az \code{iconv(x, from="", to="ASCII//TRANSLIT")}
#'   függvény csinálja.
#' A \code{factor} változókat feldolgozás előtt karakterré alakítja
#'
#' @return
#'   \code{x}-szel egyező hosszú karakter vektor, ékezetek nélkül.
#'
#' @author
#' Hajdú László, Tajti András
#'
#' @examples
#' unAccent("Árvíztűrő tükörfúrógép")
#' unAccent("Árvíztűrő tükörfúrógép", TRUE)
#'
#' @encoding UTF-8
#' @export

unAccent <- function(x, alternative = FALSE){

  if(is.factor(x)){
    x <- as.character(x)
  }

  if(!alternative){
    encs <- if(is.character(x)){
              Encoding(x)
            } else {
              rep(NA_character_, length(x))
            }

    # if windows, set unknown to CP-*, otherwise to UTF-8
    if(Sys.info()[["sysname"]] == "Windows"){
      lang_id = strsplit(Sys.getlocale(), split=";")[[1]][1]
      if(is.na(suppressWarnings(as.numeric(lang_id)))){
        base_enc <- "windows-1250" # Hungarian locale as base
      } else {
        base_enc <- paste0("windows-",
                           substring(lang_id,
                                     nchar(lang_id)-3,
                                     nchar(lang_id)))
      }
    } else {
      base_enc <- "UTF-8"
    }

    encs[encs == "unknown" | is.na(encs)] <- base_enc
    iconved <- mapply(iconv,
                     from = encs,
                     x = x,
                     MoreArgs = list(to = "ASCII//TRANSLIT"))
    return(unname(iconved))

  } else {
    x <- gsub("\u00E0", "a", x)
    x <- gsub("\u00E1", "a", x)
    x <- gsub("\u00E8", "e", x)
    x <- gsub("\u00E9", "e", x)
    x <- gsub("\u00EC", "i", x)
    x <- gsub("\u00ED", "i", x)
    x <- gsub("\u00F2", "o", x)
    x <- gsub("\u00F3", "o", x)
    x <- gsub("\u00F9", "u", x)
    x <- gsub("\u00FA", "u", x)
    x <- gsub("\u00FC", "u", x)
    x <- gsub("\u0151", "o", x)
    x <- gsub("\u00F6", "o", x)
    x <- gsub("\u0171", "u", x)

    x <- gsub("\u00C0", "A", x)
    x <- gsub("\u00C1", "A", x)
    x <- gsub("\u00C8", "E", x)
    x <- gsub("\u00C9", "E", x)
    x <- gsub("\u00CC", "I", x)
    x <- gsub("\u00CD", "I", x)
    x <- gsub("\u00D2", "O", x)
    x <- gsub("\u00D3", "O", x)
    x <- gsub("\u00D9", "U", x)
    x <- gsub("\u00DA", "U", x)
    x <- gsub("\u0170", "U", x)
    x <- gsub("\u00D6", "O", x)
    x <- gsub("\u0150", "O", x)
    x <- gsub("\u00DC", "U", x)
    return(x)
  }
}
