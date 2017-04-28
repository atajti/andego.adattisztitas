#' Címek tisztítása
#'
#' Egy irányítószámból, településből, köztérből és házszámból álló
#'   címek egységes alakra hozása.
#'
#' @param x tisztítandó címeket tartalmazó vektor
#'
#' @return
#' sztring vektor, \code{x} minden elemére egy javított verzió
#'
#' @export

address_normalizer <- function(x){
  irszam=clean_zip(x)
  varos=clean_city(varosok[varosok$postal==irszam]$City[1])
  street=clean_street(x)
  return(paste(irszam,varos,street))
}
