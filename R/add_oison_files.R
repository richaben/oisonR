#' Fonction pour ajouter les fichiers images/sons à une table OISON existante
#'
#' @description Permet d'ajouter à une 'table OISON' (taxons ou milieux) existantes les fichiers
#' images et sons présents dans la base de données.
#' La table OISON doit contenir la colonne \code{observation_id} pour permettre la jointure.
#'
#' @param oison_table nom de la table OISON à enrichir
#' @param conn nom de la base à utiliser
#' @param ... arguments supplémentaires
#'
#' @importFrom dbplyr in_schema
#' @importFrom dplyr left_join tbl collect
#'
#' @returns un dataframe avec les fichiers images et sons associés à chaque observation
#' @export
#'
#' @examples
#' \dontrun{
#' bdd_oison <- start_sql_connexion()
#'
#' df_taxon_oison_sql <- get_table_taxon_sql(conn = bd_oison)
#'
#' add_oison_files(df_taxon_oison_sql, bdd_oison)
#' }

add_oison_files <- function(oison_table, conn, ...) {

  if (missing(conn)) {
    stop("Connexion \u00e0 la base requise !")
  }

  if (!"observation_id" %in% colnames(oison_table)) {
    stop("La table OISON doit contenir la colonne 'observation_id' !")
  }

  oison_table %>%
    dplyr::left_join(
      dplyr::tbl(
        bdd_oison,
        dbplyr::in_schema("data", "file")
      ) %>%
        dplyr::filter(!is.na(observation_id)) %>%
        dplyr::select(observation_id, fichier_id = file_url) %>%
        dplyr::collect() %>%
        dplyr::bind_rows(
          dplyr::tbl(
            bdd_oison,
            dbplyr::in_schema("data", "file")
          ) %>%
            dplyr::filter(is.na(observation_id)) %>%
            dplyr::select(observation_id = sound_observation_id, fichier_id = file_url) %>%
            dplyr::collect()
        )
    )
}
