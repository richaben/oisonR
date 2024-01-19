#' @noRd
#'

.onAttach <- function(libname, pkgname) {

  invisible(suppressPackageStartupMessages(
    sapply(c("jsonlite"),
           requireNamespace, quietly = TRUE)
  ))

  url_issues <- "https://github.com/richaben/oisonR/issues"

  welcome <- function() {
    cli::cli_h1(glue::glue("{cli::col_blue(\'Bienvenue dans oisonR\')} {cli::col_blue(utils::packageVersion(\'oisonR\'))}"))
    cli::cli_par()
    cli::cli_end()
    cli::cli_alert_info(cli::col_br_red("A noter avant de d\u00e9buter :"))
    cli::cli_ul(
      c("Une connexion internet est requise pour l\'acc\u00e8s aux donn\u00e9es;",
        "Selon le type d\'acc\u00e8s aux donn\u00e9es, les identifiants OISON (login/mdp)
        peuvent etre n\u00e9cessaires ou les param\u00e8tres de la base SQL;",
        "Un acc\u00e8s \u00e0 la base SQL requiert une connexion au r\u00e9seau de l\'OFB (ou VPN).")
      )

    cli::cli_par()
    cli::cli_alert_info(cli::col_br_magenta("Veuillez remonter les probl\u00e8mes ou id\u00e9es ici:"))
    cli::cli_text("{.url {url_issues}}")
    cli::cli_end()
  }


  if (!interactive()) return(invisible())

  welcome()

}
