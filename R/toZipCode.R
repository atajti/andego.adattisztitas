#' Irányítószám kiszűrése
#' 
#' Az első 4 egymás utáni számot tartja meg.
#'
#' @param x karakter vektor
#'
#' @details
#' \code{str_extract(x,"\\b[0-9]{4}\\b")}
#'
#' @return
#' \code{x}-szel megegyező hosszú karakter vektor
#'
#' @author
#' Hajdu László
#'
#' @examples
#' cimek <- c("8000 Siófok, Ballagó utca 14/a",
#'            "8000 Siofk, Ballagó utca 14/a 3. em 31.")
#' toZipCode(cimek)
#'
#' @export

toZipCode <- function(x){
  return(str_extract(x, "\\b[0-9]{4,5}\\b"))
}