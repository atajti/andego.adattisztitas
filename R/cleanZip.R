#' Irányítószám tisztítása
#'
#' Megtisztítja a kapott irányítószámokat potenciális problémáktól.
#'
#' @param x karakter vektor
#' @param alternative logikai érték, ld. unAccent
#'
#' @return
#' \code{x}-nel azonos hosszú karakter vektor
#'
#' @author
#' Hajdú László
#'
#' @examples
#' cleanZip(" 3120 Dorgalfalva, Kossuth utca 10.")
#'
#' @encoding UTF-8
#' @export

cleanZip <- function(x, alternative = FALSE){
  return(substr(removeAllDot(toZipCode(unAccent(toupper(x),
                                                alternative))),
                0, 4))
}