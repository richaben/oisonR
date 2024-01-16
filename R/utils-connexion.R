#' Test si les accès à l'API OISON sont bien renseignés
#'
#' @description
#' Fonction pour tester si les accès login/mdp sont bien stockés dans
#' l'environnement R (fichier `.Renviron`).
#'
#' @return Aucune valeur. Une erreur est renvoyée si les accès ne sont pas renseignés.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' check_acces_oison_api()
#' }

check_acces_oison_api <- function() {

  id <- Sys.getenv("OISON_ID")
  pwd <- Sys.getenv("OISON_PASSWORD")

  if (id == "" | pwd == "") {

    stop("Impossible de lire vos acc\u00e8s \u00e0 l\'application OISON. Lancer la fonction ",
         "`usethis::edit_r_environ()` pour stocker votre login/mdp sous la forme ",
         "\'OISON_ID = *****\' et \'OISON_PASSWORD = *****\'.", call. = FALSE)
  } else {

    cli::cli_alert_info(cli::col_br_green("Connexion API possible !"))

  }

  invisible(NULL)
}

#' Test si les paramètres à la BDD OISON en SQL sont bien renseignés
#'
#' @description
#' Fonction pour tester si les paramètres pour l'accès à la BDD OISON en SQL
#' sont bien stockés dans l'environnement R (fichier `.Renviron`).
#'
#' @return Aucune valeur. Une erreur est renvoyée si les paramètres ne sont pas renseignés.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' check_acces_oison_sql()
#' }

check_acces_oison_sql <- function() {

  bd_name <- Sys.getenv("OISON_BDname")
  bd_hostname <- Sys.getenv("OISON_BDhostname")
  bd_port <- Sys.getenv("OISON_BDport")
  bd_uid <- Sys.getenv("OISON_BDuid")
  bd_pwd <- Sys.getenv("OISON_BDpwd")

  if (bd_name == "" | bd_hostname == "" | bd_port == "" | bd_uid == "" | bd_pwd == "") {

    stop("Param\u00e8tres de connexion \u00e0 la BDD SQL OISON non stock\u00e9s. Lancer la fonction ",
         "`usethis::edit_r_environ()` pour stocker ces informations ",
         "\'OISON_BDname\' ; \'OISON_BDhostname\' ; \'OISON_BDport\' ; \'OISON_BDuid\' et \'OISON_BDpwd\'.", call. = FALSE)
  } else {

    cli::cli_alert_info(cli::col_br_green("Connexion SQL possible !"))

  }

  invisible(NULL)
}
