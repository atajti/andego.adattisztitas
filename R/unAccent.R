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
#'
#' @return
#'   \code{x}-szel egyező hosszú karakter vektor, ékezetek nélkül.
#'
#' @author
#' Hajdú László
#'
#' @examples
#' unAccent("Árvíztűrő tükörfúrógép")
#' unAccent("Árvíztűrő tükörfúrógép", TRUE)
#'
#' @export

unAccent <- function(x, alternative=FALSE){
  if(!alternative){
    return(iconv(x, from="", to="ASCII//TRANSLIT"))
  } else {
    x <- gsub("á","a",x)
    x <- gsub("é","e",x)
    x <- gsub("í","i",x)
    x <- gsub("ó","o",x)
    x <- gsub("ú","u",x)
    x <- gsub("ű","u",x)
    x <- gsub("ő","o",x)
    x <- gsub("ö","o",x)
    x <- gsub("ü","u",x)

    x <- gsub("Á","A",x)
    x <- gsub("É","E",x)
    x <- gsub("Í","I",x)
    x <- gsub("Ó","O",x)
    x <- gsub("Ú","U",x)
    x <- gsub("Ű","U",x)
    x <- gsub("Ő","O",x)
    x <- gsub("Ö","O",x)
    x <- gsub("Ü","U",x)
    return(x)
  }
}