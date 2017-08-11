#' Névtisztítás
#'
#' Személynevek egységes alapkra hozása
#'
#' @param x karakter vektor, megtisztítandó személynevekkel
#'
#' @details
#' A nevej első két tagja marad meg, kivétel, ha az első tag tisztított
#'   formában max 2 karakter hosszú (ld. példa), akkor az első három
#'   tag marad meg.
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
  if(!length(x)){
    return(character(0))
  }

  x_split <- strsplit(gsub("[[:punct:]]", "",
                     removeSpecials(unAccent(toupper(trimws(removePrefixes(x)))))),
         split=" ")

  tiszta_x <- lapply(x_split, function(darabolt_nev){
    if(length(darabolt_nev) > 2 & !(nchar(darabolt_nev[1]) < 3)){
      darabolt_nev <- darabolt_nev[1:2]
    } else if(length(darabolt_nev) > 2 & (nchar(darabolt_nev[1]) < 3)){
      darabolt_nev <- darabolt_nev[1:3]
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

removePrefixes <- function(x) {
  elo = "(dr|med|mvdr|mudr|phd|phdr|csc|dsc|rndr|judr|mgr|id|id\u0151sebb|ifj|ijf|ifjabb|\u00F6zv|\u00F6zvegy|kk|masmunkavallalo|igazgatosagi tag)"
  elo = paste("^", elo, "((\\.)|(\\s)+)|(\\s)+", elo, "((\\.)|(\\s)+)|(\\s)+", 
      elo, "$", sep = "")
  i = 0
  while (any(grepl(elo, x, ignore.case = T))) {
      i = i + 1
      x <- gsub(elo, " ", x, ignore.case = T)
  }
  return(stringr::str_trim(x))
}
