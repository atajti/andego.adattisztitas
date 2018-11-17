#' Irányítószám tisztítása
#'
#' Megtisztítja a kapott városneveket potenciális problémáktól.
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



cleanCity <- function(x, alternative=FALSE){
  return(removeSpecials(removeAllDot(toupper(unAccent(x, alternative)))))
}