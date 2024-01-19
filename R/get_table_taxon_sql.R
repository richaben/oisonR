#' Fonction pour créer le tableau taxon OISON via connexion SQL
#'
#' @description Permet de créer une 'table taxon' à partir de la base SQL.
#' La table ainsi créée correspond par défaut à l'ensemble des observations sur
#' les taxons bancarisés dans OISON.
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
#' df_taxon_oison_sql <- get_table_taxon_sql(conn = bd_oison)
#'
#' stop_sql_connexion(conn = bdd_oison)
#' }
#'
get_table_taxon_sql <-
  function(conn) {
    # get observation in data schema
    dplyr::tbl(conn, dbplyr::in_schema("data", "observation")) %>%
      dplyr::filter(type != 'milieu') %>%
      dplyr::rename(observation_id = id,
                    heure = time) %>%
      # join to referentiel taxon by 'taxon_code'
      dplyr::left_join(
        dplyr::tbl(conn, dbplyr::in_schema("referentiels", "taxon")) %>%
          dplyr::rename(taxon_code = code) %>%
          dplyr::select(-leaf)
      ) %>%
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
      # join to corine biotope part
      left_join(
        dplyr::tbl(conn, dbplyr::in_schema("referentiels", "corine_biotope")) %>%
          dplyr::select(
            corine_biotope_id = id,
            corine_label = label,
            corine_code = code
          )
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

      # join to status part
      dplyr::left_join(
        dplyr::tbl(conn, dbplyr::in_schema("referentiels", "status")) %>%
          dplyr::select(status_id = id,
                        status = label)
      ) %>%

      # join to taxon_presence
      dplyr::left_join(
        dplyr::tbl(conn, dbplyr::in_schema("referentiels", "taxon_presence")) %>%
          dplyr::select(presence_id = id,
                        presence = label)
      ) %>%

      # remove useless columns
      dplyr::select(
        -initiator_user_id,-corine_biotope_id,-type_recherche_id,-contexte_recherche_id,-objectif_recherche_id,-status_id,-presence_id
      ) %>%

      # join to recensement_taxon
      dplyr::left_join(
        dplyr::tbl(conn, dbplyr::in_schema("data", "recensement_taxon")) %>%
          dplyr::select(-id) %>%
          dplyr::rename(observation_id = observation_taxon_id) %>%
          # join to taxon_stade_developpement
          left_join(
            dplyr::tbl(
              conn,
              dbplyr::in_schema("referentiels", "taxon_stade_developpement")
            ) %>%
              dplyr::select(
                stade_developpement_id = id,
                stade_developpement = label
              )
          ) %>%
          # join to taxon_vivant_trace
          left_join(
            dplyr::tbl(
              conn,
              dbplyr::in_schema("referentiels", "taxon_vivant_trace")
            ) %>%
              dplyr::select(vivant_trace_id = id,
                            vivant_trace = label)

          ) %>%
          # join to classe_nombre_individus
          left_join(
            dplyr::tbl(
              conn,
              dbplyr::in_schema("referentiels", "classe_nombre_individus")
            ) %>%
              dplyr::select(
                classe_nombre_individus_id = id,
                classe_nombre_individus = label
              )
          ) %>%
          # remove useless columns
          dplyr::select(
            -stade_developpement_id,-vivant_trace_id,-classe_nombre_individus_id
          )
      ) %>%
      # final selection of columns
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
        presence,
        status,
        cd_nom = taxon_code,
        regne,
        phylum,
        classe,
        ordre,
        famille,
        rang,
        groupe2_inpn,
        type_recherche,
        contexte_recherche,
        objectif_recherche,
        co_observateur,
        commentaire,
        stade_developpement,
        vivant_trace,
        nombre_individu,
        classe_nombre_individus,
        corine_label,
        corine_code,
        observation_milieu_id,
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
