#' Névtisztítás
#'
#' Cég- és személynevek egységes alakra hozása
#'
#' @param x karakter vektor, megtisztítandó személynevekkel
#'
#' @details
#' A függvény valójában csak egy wrapper, a \code{detectFirms} eredménye
#'   alapján eldönti, melyik névre alkalmazza a \code{cleanPersonNames}
#'   és melyikre a \code{cleanFirms} függvényt.
#'
#' @return
#' karakter vektor, a tisztított nevekkel
#'
#' @author
#' Tajti András <atajti@andego.hu>
#' 
#' @examples
#' cleanNames("Sándor József Benedek")
#' cleanNames("S. József Benedek")
#' cleanNames("Sándor J. Benedek")
#' cleanFirms(toupper(unAccent(c("Andego Tanácsadó Korlátolt felelősségű társaság",
#'                               "Magyar Telekom Nyrt"))))
#'
#' @export

cleanNames <- function(x){
  res <- character(length(x))
  firms <- detectFirms(x)
  res[!isTRUE(firms)] <- cleanPersonNames(x[!isTRUE(firms)])
  res[isTRUE(firms)] <- cleanFirms(x[isTRUE(firms)])
  return(res)
}
