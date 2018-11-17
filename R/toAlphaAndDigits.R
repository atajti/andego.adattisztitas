#' Számok és betűk megtartása
#' 
#' Minden karaktert töröl, ami nem szám, vagy nem betű (a betű
#'   \code{:alpha:} értelmében, ld. \code{?regex})
#'
#' @param x karakter vektor
#'
#' @details
#' \code{gsub("[0-9]","",x)}
#'
#' @section TODO:
#' A nem angolszász ABC betűit is békén hagyhatná...
#'
#' @return
#' \code{x}-szel megegyező hosszú karakter vektor
#'
#' @author
#' Hajdu László
#'
#' @examples
#' toAlphaAndDigits("1+4=öt")
#'
#' @encoding UTF-8
#' @export


toAlphaAndDigits <- function(x){
  return(gsub("[^[:alnum:]]", "", x))
}