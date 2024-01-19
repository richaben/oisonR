#' Fonction pour créer le tableau milieu OISON via connexion SQL
#'
#' @description Permet de créer une 'table milieu' à partir de la base SQL.
#' La table ainsi créée correspond par défaut à l'ensemble des observations sur
#' les milieux bancarisés dans OISON.
#'
#' @param conn nom de la base à utiliser.
#'
#' @return un dataframe avec les observations taxons
#' @export
#'
#' @importFrom dbplyr in_schema
#' @importFrom dplyr tbl filter rename left_join select collect mutate
#' @importFrom sf st_as_sfc
#'
#' @examples
#' \dontrun{
#' bdd_oison <- start_sql_connexion()
#'
#' df_milieu_oison_sql <- get_milieu_taxon_sql(conn = bd_oison)
#'
#' stop_sql_connexion(conn = bdd_oison)
#' }
#'
get_milieu_taxon_sql <-
  function(conn) {
    dplyr::tbl(conn, dbplyr::in_schema("data", "observation")) %>%
      dplyr::filter(type == 'milieu') %>%
      dplyr::rename(observation_id = id,
                    heure = time) %>%

      # join to users part by 'initiator_user_id'
      dplyr::left_join(
        dplyr::tbl(conn, dbplyr::in_schema("oison", "users")) %>%
          dplyr::select(
            initiator_user_id = id,
            email,
            nom = name,
            prenom = first_name
          )
      ) %>%
      # join to type_recherche
      dplyr::left_join(
        dplyr::tbl(conn, dbplyr::in_schema("referentiels", "type_recherche")) %>%
          dplyr::select(type_recherche_id = id,
                        type_recherche = label)
      ) %>%
      # join to contexte_recherche
      dplyr::left_join(
        dplyr::tbl(
          conn,
          dbplyr::in_schema("referentiels", "contexte_recherche")
        ) %>%
          dplyr::select(
            contexte_recherche_id = id,
            contexte_recherche = label
          )
      ) %>%

      # join to objectif_recherche
      dplyr::left_join(
        dplyr::tbl(
          conn,
          dbplyr::in_schema("referentiels", "objectif_recherche")
        ) %>%
          dplyr::select(
            objectif_recherche_id = id,
            objectif_recherche = label
          )
      ) %>%
      # join to status
      dplyr::left_join(
        dplyr::tbl(conn, dbplyr::in_schema("referentiels", "status")) %>%
          dplyr::select(status_id = id,
                        status = label)
      ) %>%
      # join to corine biotope part
      dplyr::left_join(
        dplyr::tbl(conn, dbplyr::in_schema("referentiels", "corine_biotope")) %>%
          dplyr::select(
            corine_biotope_id = id,
            corine_label = label,
            corine_code = code
          )
      )   %>%
      # join to impact_suppose / situation
      dplyr::left_join(
        dplyr::tbl(
          conn,
          dbplyr::in_schema("referentiels", "milieu_impact_suppose")
        ) %>%
          dplyr::select(impact_suppose_id = id,
                        impact_situation = label)
      ) %>%
      # join to perturbation
      dplyr::left_join(
        dplyr::tbl(
          conn,
          dbplyr::in_schema("referentiels", "milieu_perturbation")
        ) %>%
          dplyr::select(perturbation_id = id,
                        perturbation = label)
      ) %>%
      # join to milieu_impact
      dplyr::left_join((
        dplyr::tbl(
          conn,
          dbplyr::in_schema("data", "observation_milieu_code_impact")
        ) %>%
          dplyr::left_join(
            dplyr::tbl(
              conn,
              dbplyr::in_schema("referentiels", "milieu_code_impact")
            ) %>%
              dplyr::select(
                milieu_code_impact_id = id,
                milieu_impact = label
              )
          )
      )) %>%
      # join to activite
      dplyr::left_join(
        dplyr::tbl(conn, dbplyr::in_schema("referentiels", "milieu_activite")) %>%
          dplyr::select(activite_id = id,
                        activite = label)
      ) %>%
      # join to fonction observee / compartiment
      dplyr::left_join(
        dplyr::tbl(
          conn,
          dbplyr::in_schema("referentiels", "milieu_fonction_observee")
        ) %>%
          dplyr::select(
            fonction_observee_id = id,
            fonction_observee = label
          )
      ) %>%
      # join to chronicite
      dplyr::left_join(
        dplyr::tbl(
          conn,
          dbplyr::in_schema("referentiels", "milieu_chronicite")
        ) %>%
          dplyr::select(chronicite_id = id,
                        chronicite = label)
      ) %>%
      # join to localisation part
      dplyr::left_join(
        dplyr::tbl(conn, dbplyr::in_schema("data", "localisation")) %>%
          dplyr::select(
            localisation_id = id,
            observation_id,
            geometry,
            x_point,
            y_point,
            surface_station,
            longueur_troncon
          )
      ) %>%
      # selection columns
      dplyr::select(
        observation_id,
        observation_type = type,
        nom,
        prenom,
        email,
        date,
        heure,
        # nom_vernaculaire = vernacular_name,
        # nom_scientifique = name,
        # presence,
        status,
        # cd_nom = taxon_code,
        # regne,
        # phylum,
        # classe,
        # ordre,
        # famille,
        # rang,
        # groupe2_inpn,
        corine_label,
        corine_code,
        type_recherche,
        contexte_recherche,
        objectif_recherche,
        co_observateur,
        commentaire,
        # stade_developpement,
        # vivant_trace,
        # nombre_individu,
        # classe_nombre_individus,
        impact_situation,
        perturbation,
        milieu_impact,
        fonction_observee,
        chronicite,
        activite,
        geometry,
        x_point,
        y_point,
        surface_station,
        longueur_troncon,
        localisation_id,
        uuid
      ) %>%
      # test some rows
      dplyr::collect() %>%

      # transform geometry to WKT
      dplyr::mutate(geometry =
                      sf::st_as_sfc(structure(as.list(geometry), class = "WKB"), EWKB = TRUE))
  }


