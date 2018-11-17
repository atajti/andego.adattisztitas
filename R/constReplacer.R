#' Konstansokban tárolt elemek cseréje
#'
#' Csak eszköz, valójában egy \code{gsub}-hívás.
#' 
#' @param regkif reguláris kifejezés, u.a. mint a \code{pattern}
#' @param csere cseresztring, u.a. mint a \code{replacement}
#' @param min a minta amiben keres, u.a. mint az \code{x}'
#'
#' @return
#' \code{min}-nel azonos hosszú karakter vektor
#'
#' @author
#' Hajdú László
#' @encoding UTF-8


constReplacer <- function(regkif,csere,min){
  return(gsub(regkif,csere,min))
}