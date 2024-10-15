#' Ajoute une colonne contenant le type de geometrie pour les observations
#'
#' @param oison_df un dataframe de classe sf
#'
#' @return un dataframe avec une colonne `type_geometrie`
#' @export
#'
#' @importFrom dplyr mutate case_when
#' @importFrom sf st_is
#'
#' @examples
#' \dontrun{
#' oison_taxon <-
#'   oison_taxon %>%
#'   sf::st_as_sf() %>%
#'   add_type_geometrie()
#' }
#'

add_type_geometrie <- function(oison_df) {

  if (!inherits(oison_df, "sf"))
    stop("le dataframe n'est pas de classe sf")

  oison_df %>%
    dplyr::mutate(type_geometrie = dplyr::case_when(sf::st_is(., "POINT") ~ "POINT",
                                                    sf::st_is(. , "POLYGON") ~ "POLYGON",
                                                    sf::st_is(., "MULTILINESTRING") ~ "MULTILINESTRING",
                                                    sf::st_is(., "LINESTRING") ~ "LINESTRING",
                                                    TRUE ~ "EMPTY GEOMETRY")
                  )
}

