#' Pontok eltűntetése
#' 
#' Az összes pontot törli a sztringekből
#'
#' @param x karakter vektor
#'
#' @details
#' \code{gsub("\\.", "", x)}
#'
#' @return
#' \code{x}-szel megegyező hosszú karakter vektor
#'
#' @author
#' Hajdu László
#'
#' @examples
#' removeAllDot("Az ellipszis argumentum: ...")
#'
#' @export

removeAllDot <- function(x){
  return(gsub("\\.", "", x))
}