#' Récupération des Codes Habitats à partir de la fiche taxon présente sur l'INPN
#'
#' @description
#' A partir d'un dataframe issu de la requête OISON, cette fonction permet de récupérer
#' les informations relatives aux Habitats présentés sur la fiche taxon de l'INPN.
#' Par exemple, pour le Rougegorge familier, les infos sont collectés
#' \url{https://inpn.mnhn.fr/espece/cd_nom/4001/tab/habitats}{sur sa fiche ici}.
#'
#' @param df un dataframe issu d'une requête taxon OISON avec à minima les 3 colonnes suivantes :
#'  - le nom vernaculaire
#'  - le nom scientifique
#'  - le code taxon
#' @param cdnom_col le nom de la colonne contenant le code taxon utilisé dans l'URL de l'INPN
#' @param nom_vern_col le nom de la colonne contenant le(s) nom(s) vernaculaire(s) à conserver
#' @param nom_sci_col le nom de la colonne contenant le(s) nom(s) scientifique(s) à conserver
#'
#' @return un dataframe avec les Codes Habitats pour chacun des taxons du tableau initial
#' @export
#'
#' @importFrom dplyr select everything mutate right_join n as_tibble
#' @importFrom furrr future_map_dfr
#' @importFrom progressr with_progress progressor
#' @importFrom purrr possibly
#' @importFrom rvest read_html html_element html_table
#'
#' @examples
#' \dontrun{
#' ## Requete sur quelques observations
#' data_oison <-
#'   oisonR::get_taxon_dpt(dpt_code = "27",
#'                         login = "john.doe@ofb.gouv.fr",
#'                         mdp = "mon_mdp",
#'                         date_min = "2023-01-01",
#'                         date_max = "2023-02-01",
#'                         collect_all = TRUE) %>%
#'   dplyr::select(cd_ref, nom_scientifique, nom_vernaculaire)
#'
#' ## Collecte des informations Codes Habitats
#' code_habitats <-
#'   imp_inpn_habitats_taxon(df = data_oison,
#'                           cdnom_col = cd_ref,
#'                           nom_sci_col = nom_scientifique,
#'                           nom_vern_col = nom_vernaculaire)
#'
#' }

imp_inpn_habitats_taxon <- function(df,
                                    cdnom_col,
                                    nom_vern_col,
                                    nom_sci_col){

  df <-
    df %>%
    dplyr::select({{nom_vern_col}},
                  {{nom_sci_col}},
                  {{cdnom_col}},
                  dplyr::everything()) %>%
    unique() %>%
    dplyr::mutate(url_esp_hab := paste0("https://inpn.mnhn.fr/espece/cd_nom/", {{cdnom_col}}, "/tab/habitats"))

  read_url_hab <- function(url_hab, df){

    p()

    tab_taxon <-
      rvest::read_html(url_hab) %>%
      rvest::html_element("#especes") %>%
      rvest::html_table() %>%
      dplyr::select(!!-c(1))

    df %>%
      dplyr::right_join(
        tab_taxon %>%
          dplyr::mutate(url_esp_hab = rep(url_hab, dplyr::n()))) %>%
      suppressMessages()
  }


  poss_readurl_hab <- purrr::possibly(.f = read_url_hab, otherwise = NULL)

  progressr::with_progress({

    p <- progressr::progressor(steps = length(df$url_esp_hab))

    furrr::future_map_dfr(df = df,
                          .x = df$url_esp_hab,
                          .f = poss_readurl_hab
    ) %>%
      dplyr::as_tibble()
  })

}
