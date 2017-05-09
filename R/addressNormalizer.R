#' Címek tisztítása
#'
#' Egy irányítószámból, településből, köztérből és házszámból álló
#'   címek egységes alakra hozása.
#'
#' @param x tisztítandó címeket tartalmazó vektor
#'
#' @return
#' karakter vektor, \code{x} minden elemére egy javított verzió
#'
#' @author
#' Hajdú László
#'
#' @examples
#' cimek <- c("8000 Siófok, Ballagó utca 14/a",
#'            "8000 Siofk, Ballagó utca 14/a 3. em 31.")
#' addressNormalizer(cimek)
#' @export

addressNormalizer <- function(x){
  irszam <- cleanZip(x)
  irsz_sorrend <- sapply(irszam,
                         function(irsz){
                           which(irsz == varosok$postal)
                         })
  varos <- cleanCity(varosok[irsz_sorrend, "City"])
  street <- cleanStreet(x)
  return(paste(irszam,varos,street))
}
