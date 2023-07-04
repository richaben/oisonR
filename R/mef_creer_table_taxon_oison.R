#' Fonction pour créer un tableau taxon issu de OISON
#'
#' Cette fonction permet de créer un tableau avec les observations \code{taxon} présentes dans la
#'  base OISON, à partir des tables importées par le dump de la base.
#'
#'  Nécessite au préalable d'importer le dump sql avec la fonction \code{imp_importer_dump_oison}.
#'
#' @return un dataframe avec les observations taxons.
#'  La colonne \code{geometry} spécifie le type de géométrie (point, lignes, polygones) pour l'observation,
#'  et doit être traitée avec précaution lors d'un export sous un autre format (.csv, ...).
#' @export
#'
#' @importFrom dplyr filter mutate rename left_join select recode as_tibble
#' @importFrom sf st_as_sfc
#'
#' @examples
#' \dontrun{
#' df_taxon_oison <- mef_creer_table_taxon_oison()
#' }
#'
mef_creer_table_taxon_oison <-
  function() {
    observation %>%
      dplyr::filter(type != 'milieu') %>%
      dplyr::rename(observation_id = id,
                    heure = `"time"`) %>%
      dplyr::left_join(taxon %>% dplyr::rename(taxon_code = code) %>% dplyr::select(-leaf)) %>%

      dplyr::left_join(users %>% dplyr::select(
        initiator_user_id = id,
        email,
        nom = name,
        prenom = first_name
      )) %>%

      dplyr::left_join(
        corine_biotope %>% dplyr::select(
          corine_biotope_id = id,
          corine_label = label,
          corine_code = code
        )
      ) %>%

      dplyr::select(-initiator_user_id,-corine_biotope_id) %>%

      dplyr::mutate(
        type_recherche_id = dplyr::recode(
          type_recherche_id,!!!setNames(type_recherche$label,
                                        type_recherche$id)
        ),
        contexte_recherche_id = dplyr::recode(
          contexte_recherche_id,!!!setNames(contexte_recherche$label,
                                            contexte_recherche$id)
        ),
        objectif_recherche_id = dplyr::recode(
          objectif_recherche_id,!!!setNames(objectif_recherche$label,
                                            objectif_recherche$id)
        ),
        status_id = dplyr::recode(status_id,!!!setNames(status$label,
                                                        status$id)),
        presence_id = dplyr::recode(
          presence_id,!!!setNames(taxon_presence$label,
                                  taxon_presence$id)
        )
      ) %>%
      left_join((
        recensement_taxon %>%
          dplyr::select(-id) %>%
          dplyr::rename(observation_id = observation_taxon_id) %>%
          dplyr::mutate(
            stade_developpement_id = dplyr::recode(
              stade_developpement_id,!!!setNames(
                taxon_stade_developpement$label,
                taxon_stade_developpement$id
              )
            ),
            vivant_trace_id = dplyr::recode(
              vivant_trace_id,!!!setNames(taxon_vivant_trace$label,
                                          taxon_vivant_trace$id)
            ),
            classe_nombre_individus_id = dplyr::recode(
              classe_nombre_individus_id,!!!setNames(classe_nombre_individus$label,
                                                     classe_nombre_individus$id)
            )
          )
      )) %>%
      dplyr::left_join(localisation %>%
                         dplyr::select(-id) %>%
                         dplyr::mutate(geometry = sf::st_as_sfc(
                           structure(as.list(localisation$geometry), class = "WKB"), EWKB = TRUE
                         ))) %>%
      dplyr::select(
        observation_id,
        observation_type = type,
        nom,
        prenom,
        email,
        date,
        heure,
        nom_vernaculaire = vernacular_name,
        nom_scientifique = name,
        presence = presence_id,
        status = status_id,
        cd_nom = taxon_code,
        regne,
        phylum,
        classe,
        ordre,
        famille,
        rang,
        groupe2_inpn,
        type_recherche = type_recherche_id,
        contexte_recherche = contexte_recherche_id,
        objectif_recherche = objectif_recherche_id,
        co_observateur,
        commentaire,
        stade_developpement = stade_developpement_id,
        vivant_trace = vivant_trace_id,
        nombre_individu,
        classe_nombre_individus = classe_nombre_individus_id,
        corine_label,
        corine_code,
        geometry,
        x_point,
        y_point,
        surface_station,
        longueur_troncon,
        uuid
      ) %>%
      dplyr::as_tibble()
  }
