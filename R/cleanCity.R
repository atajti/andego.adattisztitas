#' Irányítószám tisztítása
#'
#' Megtisztítja a kapott városneveket potenciális problémáktól.
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



cleanCity <- function(x, alternative=FALSE){
  return(removeSpecials(removeAllDot(toupper(unAccent(x, alternative)))))
}