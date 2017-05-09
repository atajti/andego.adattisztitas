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
    x <- gsub("\u00E0","a",x)
    x <- gsub("\u00E1","a",x)
    x <- gsub("\u00E8","e",x)
    x <- gsub("\u00E9","e",x)
    x <- gsub("\u00EC","i",x)
    x <- gsub("\u00ED","i",x)
    x <- gsub("\u00F2","o",x)
    x <- gsub("\u00F3","o",x)
    x <- gsub("\u00F9","u",x)
    x <- gsub("\u00FA","u",x)
    x <- gsub("\u00FC","u",x)
    x <- gsub("\u0151","o",x)
    x <- gsub("\u00F6","o",x)
    x <- gsub("\u0171","u",x)

    x <- gsub("\u00C0","A",x)
    x <- gsub("\u00C1","A",x)
    x <- gsub("\u00C8","E",x)
    x <- gsub("\u00C9","E",x)
    x <- gsub("\u00CC","I",x)
    x <- gsub("\u00CD","I",x)
    x <- gsub("\u00D2","O",x)
    x <- gsub("\u00D3","O",x)
    x <- gsub("\u00D9","U",x)
    x <- gsub("\u00DA","U",x)
    x <- gsub("\u0170","U",x)
    x <- gsub("\u00D6","O",x)
    x <- gsub("\u0150","O",x)
    x <- gsub("\u00DC","U",x)
    return(x)
  }
}