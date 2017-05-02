#' Speciális karakterek eltűntetése
#' 
#' Csak betűk, számok és üres karaktereket tart meg, a többit kivágja.
#'
#' @param x karakter vektor
#'
#' @details
#' Az :alnum: és a :blank: osztályú karakterek maradnak csak meg.
#'
#' @return
#' \code{x}-szel megegyező hosszú karakter vektor
#'
#' @author
#' Hajdu László
#'
#' @examples
#' removeSpecials("Ꞁ ¤")
#'
#' @export

removeSpecials <- function(x){
  return(gsub("[^[:alnum:][:blank:]]", "", x))
}
