#' Récupération des infos de répartition d'un taxon à l'échelle départementale
#'  selon l'INPN
#'
#' @description
#' La fonction permet de récupérer rapidement les informations, quand elles existent,
#' sur la répartition d'un taxon à l'échelle des départements sur le site de l'INPN.
#' Ces informations sont utilisées pour la validation des données du SINP. Plusieurs
#' statuts de présence/absence sont possibles (voir INPN pour plus d'infos).
#' Exemple de carte de répartition pour la Salamandre tachetée :
#' \href{https://inpn.mnhn.fr/espece/cd_nom/965096/tab/carte}{sur sa fiche ici}.
#'
#' @param cd_nom le code cd_nom vérifié du taxon
#' @param code_dpt le code insee du département
#'
#' @return un dataframe avec les informations de répartition pour le taxon
#' @export
#'
#' @importFrom dplyr as_tibble mutate
#' @importFrom glue glue
#' @importFrom rvest read_html html_elements html_text html_element html_table
#'
#' @examples
#' \dontrun{
#' ## Exemple pour la Salamandre tachetée sur le département du Calvados (14)
#' oisonR::add_inpn_repartition_taxon(cd_nom = 965096, code_dpt = 14)
#' }

add_inpn_repartition_taxon <- function(cd_nom, code_dpt){

  url_repartition <- glue::glue("https://inpn.mnhn.fr/espece/cd_nom/{cd_nom}/donnees/dept/{code_dpt}")

  species_name <-
    rvest::read_html(url_repartition) %>%
    rvest::html_elements("h2") %>%
    rvest::html_text() %>% .[[1]]

  dpt_name <-
    rvest::read_html(url_repartition) %>%
    rvest::html_elements("h2") %>%
    rvest::html_text() %>% .[[2]]

  rvest::read_html(url_repartition) %>%
    rvest::html_element("table") %>%
    rvest::html_table() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(cd_nom = {{cd_nom}},
                  code_dpt = {{code_dpt}},
                  nom_sci = species_name,
                  .before = 1)
}
