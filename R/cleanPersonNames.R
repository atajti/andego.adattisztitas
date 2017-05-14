#' Névtisztítás
#'
#' Személynevek egységes alapkra hozása
#'
#' @param x karakter vektor, megtisztítandó személynevekkel
#'
#' @details
#' Alapvetően vezetéknévre és keresztnévre bontjuk a neveket.
#' Ha van középs név, azt az első betűre redukáljuk (Kiss Sándor József
#'   -> Kiss S József'), kivétel, ha az első név egy vagy két betűből
#'   áll - ekkor a középső nevet teljesen meghagyjuk (N Nagy Nándor)
#'
#' @section TODO:
#' Egyrészt karaktereket és nem betűket különböztetünk meg, Szabó Sz
#'   Balázs -> Szabó S Balázs, másrészt római számokat is érdemes
#'   lesz figyelni később: II. Rákóczi Ferenc -> I Rákóczi Ferenc
#'
#' @return
#' karakter vektor, a tisztított nevekkel
#'
#' @author
#' Tajti András <atajti@andego.hu>
#' 
#' @examples
#' cleanPersonNames("Sándor József Benedek")
#' cleanPersonNames("S. József Benedek")
#' cleanPersonNames("Sándor J. Benedek")
#'
#' @export

cleanPersonNames <- function(x){
  x_split <- strsplit(gsub("[[:punct:]]", "",
                     removeSpecials(unAccent(toupper(trimws(removePrefixes(x)))))),
         split=" ")

  tiszta_x <- sapply(x_split, function(darabolt_nev){
    if(length(darabolt_nev)>2 & !(nchar(darabolt_nev[1])<3)){
      darabolt_nev[2] <- substr(darabolt_nev[2], 0, 1)
    }
    darabolt_nev})
  
  tiszta_x <- if(is.list(tiszta_x)){
                sapply(tiszta_x, paste0, collapse=" ")
              } else {
                paste0(tiszta_x, collapse=" ")
              }
  tiszta_x[is.na(x)] <- NA
  return(tiszta_x)
}

removePrefixes <- function(x){
  torlendok <- c("dr", "id", "ifj", "phd")
  return(gsub(paste0(torlendok, "[\\.| ]", collapse="|"),
              "",
              x,
              ignore.case=TRUE))
}
