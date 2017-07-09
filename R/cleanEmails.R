#' E-mail címek tisztítása
#'
#' A nem megfelelő formátumú e-ail-címeket (amikben nem csak egy
#'   \code{@} és \code{.} van), \code{NA}-val helyettesíti.
#'
#' @param chr_vect karakter vektor
#'
#' @details
#' A függvény jelenleg egy \code{grepl}-hívásból áll. Találtunk duplikált
#'   is, ennek detektálása a két \code{@} és \code {.}, vagy hogy minden
#'   előfordulásának száma páros. Fel lehet később turbózni, hogy
#'   létező internetkapcsolat esetén ellenőrizze hogy létező top level
#'   domain-nel rendelkezik-e a cím.
#' 
#' @return
#' \code{x}-szel megegyező hosszú karakter vektor
#'
#' @author
#' Tomasovszky Álmos
#'
#' @examples
#' cleanEmails("vmi@email.cim")
#'
#' @export


cleanEmails <- function(chr_vect){

  cemail <- chr_vect
  cemail[!grepl(".*@.*\\.*", chr_vect)] <- NA

  return(cemail) 
}
