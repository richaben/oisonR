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
#' @importFrom dplyr left_join tbl collect rename
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
#' add_oison_img_files(df_taxon_oison_sql, bdd_oison)
#' }

add_oison_img_files <- function(oison_table, conn, ...) {

  if (missing(conn)) {
    stop("Connexion \u00e0 la base requise !")
  }

  if (!"observation_id" %in% colnames(oison_table)) {
    stop("La table OISON doit contenir la colonne 'observation_id' !")
  }

  oison_table %>%
    dplyr::left_join(
      dplyr::tbl(
        conn,
        dbplyr::in_schema("data", "file")
      ) %>%
        dplyr::collect()
    ) %>%
    dplyr::rename(fichier_img_id = file_url,
                  fichier_son_id = sound_observation_id)
}
