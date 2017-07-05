#' Irányítószám tisztítása
#'
#' Megtisztítja a kapott irányítószámokat potenciális problémáktól.
#'
#' @param x karakter vektor
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
#' @export

cleanZip <- function(x, alternative=FALSE){
  return(removeAllDot(toZipCode(unAccent(toupper(x), alternative))))
}