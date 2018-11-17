#' Felesleges szóközök eltávolítása
#'
#' Ha több mint egy sortörés, tab, kocsivissza vagy szóköz jön egymás
#' után, egyetlen szóközre cseréli, illetve a szöveg eleji és végi
#' szóközöket is eltüntetni.
#'
#' @param x karakter vektor
#'
#' @details
#' \code{trimws(gsub("[ \n\t\r]+", "", x))}
#' 
#' @section TODO:
#' A cél az lenne, hogy minden whitespace-t csak egy space-re
#' cseréljünk?
#'
#' @return
#' \code{x}-szel megegyező hosszú karakter vektor
#'
#' @author
#' Hajdu László
#'
#' @examples
#' removeSpaces("Az ellipszis argumentum: ...")
#'
#' @encoding UTF-8
#' @export


removeSpaces <- function(x){
  return(trimws(gsub("[ \n\t\r]+", "", x)))
}
