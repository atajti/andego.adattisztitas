#' Címek tisztítása
#'
#' Egy irányítószámból, településből, köztérből és házszámból álló
#'   címek egységes alakra hozása.
#'
#' @param x tisztítandó címeket tartalmazó vektor
#'
#'
#' @section TODO:
#' A városok jelenleg egy lista alapján irányítoszámokból jön létre.
#'   Mivel egy irányítoszám több elepüléshez tartozik, ez a módszer
#'   adatvesztéssel jár. A listát városnév-tisztításra kéne csak
#'   használni.
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
  varosok <- varosok[which(!duplicated(varosok$postal)),]
  irsz_sorrend <- sapply(irszam,
                         function(irsz){
                           which(irsz == varosok$postal)
                         })
  irsz_sorrend[(sapply(irsz_sorrend, length)==0)] <- NA # nem talált irszek NA lesznek
  varos <- cleanCity(unname(unlist(varosok[unlist(irsz_sorrend), "City"])))
  street <- cleanStreet(x)
  return(paste(irszam,varos,street))
}
