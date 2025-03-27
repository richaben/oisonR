#' Fonction pour créer le tableau taxon OISON via connexion SQL
#'
#' @description Permet de créer une 'table taxon' à partir de la base SQL.
#' Par défaut, la table créée correspond à l'ensemble des observations sur
#' les taxons bancarisés dans OISON.
#' Si un argument \code{geometrie} est fourni, la table créée correspond aux
#' observations sur les taxons sur la zone géographique spécifiée par la géométrie.
#'
#' @param conn nom de la base à utiliser
#' @param geometrie chaîne de caractères.
#'  Elle est utilisée pour la requête SQL pour
#' filtrer les données.
#'  La géométrie doit être sous la forme d'une chaîne de caractères
#' (ex. "POLYGON((x1 y1, x2 y2, x3 y3, x4 y4, x1 y1))".
#'  Sa projection doit être en RGF93 / Lambert-93 (EPSG=2154).
#' @param ... arguments supplémentaires
#'
#' @return un dataframe avec les observations taxons.
#' @export
#'
#' @importFrom dbplyr in_schema
#' @importFrom dplyr tbl filter rename left_join inner_join select collect mutate
#' @importFrom sf st_as_sfc
#'
#' @examples
#' \dontrun{
#' ### ------------- ###
#' ## Exemple 1. Export sans géométrie
#' bdd_oison <- start_sql_connexion()
#'
#' df_taxon_oison_sql <- get_table_taxon_sql(conn = bdd_oison)
#'
#' stop_sql_connexion(conn = bdd_oison)
#'
#' ### ------------- ###
#' ## Exemple 2. Export avec géométrie spécifiée
#' # a) créer une géométrie
#' # devtools::install_github("MaelTheuliere/COGiter")
#'
#' geom_normandie <-
#'   COGiter::regions_geo %>%
#'   dplyr::filter(REG == 28) %>%
#'   sf::st_as_sfc() %>%
#'   # convert to text
#'   sf::st_as_text()
#'
#' # b) faire la requête
#' bdd_oison <- start_sql_connexion()
#'
#' df_taxon_oison_sql_normandie <-
#' get_table_taxon_sql(conn = bdd_oison, geometrie = geom_normandie)
#'
#' stop_sql_connexion(conn = bdd_oison)
#'
#' }
get_table_taxon_sql <-
  function(conn, geometrie, ...) {

    { if (!missing(geometrie)) {

      req <- glue::glue('SRID=2154;{geometrie}')

      dplyr::tbl(conn, dbplyr::in_schema("oison_data", "localisation")) %>%
        dplyr::select(
          localisation_id = id,
          observation_id,
          geometry,
          x_point,
          y_point,
          surface_station,
          longueur_troncon
        ) %>%
        dplyr::filter(ST_Intersects(geometry, req)) } else {
          dplyr::tbl(conn, dbplyr::in_schema("oison_data", "localisation")) %>%
            dplyr::select(
              localisation_id = id,
              observation_id,
              geometry,
              x_point,
              y_point,
              surface_station,
              longueur_troncon
            )
        }
    } %>%
      dplyr::inner_join(
        dplyr::tbl(conn, dbplyr::in_schema("oison_data", "observation")) %>%
          dplyr::filter(type != 'milieu') %>%
          dplyr::rename(observation_id = id,
                        heure = time)) %>%
      #
      # join to referentiel taxon by 'taxon_code'
      dplyr::left_join(
        dplyr::tbl(conn, dbplyr::in_schema("oison_referentiels", "taxon")) %>%
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
      dplyr::left_join(
        dplyr::tbl(conn, dbplyr::in_schema("oison_referentiels", "corine_biotope")) %>%
          dplyr::select(
            corine_biotope_id = id,
            corine_label = label,
            corine_code = code
          )
      ) %>%
      # join to type_recherche
      dplyr::left_join(
        dplyr::tbl(conn, dbplyr::in_schema("oison_referentiels", "type_recherche")) %>%
          dplyr::select(type_recherche_id = id,
                        type_recherche = label)
      ) %>%

      # join to contexte_recherche
      dplyr::left_join(
        dplyr::tbl(
          conn,
          dbplyr::in_schema("oison_referentiels", "contexte_recherche")
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
          dbplyr::in_schema("oison_referentiels", "objectif_recherche")
        ) %>%
          dplyr::select(
            objectif_recherche_id = id,
            objectif_recherche = label
          )
      ) %>%

      # join to status part
      dplyr::left_join(
        dplyr::tbl(conn, dbplyr::in_schema("oison_referentiels", "status")) %>%
          dplyr::select(status_id = id,
                        status = label)
      ) %>%

      # join to taxon_presence
      dplyr::left_join(
        dplyr::tbl(conn, dbplyr::in_schema("oison_referentiels", "taxon_presence")) %>%
          dplyr::select(presence_id = id,
                        presence = label)
      ) %>%

      # remove useless columns
      dplyr::select(
        -initiator_user_id,-corine_biotope_id,-type_recherche_id,-contexte_recherche_id,-objectif_recherche_id,-status_id,-presence_id
      ) %>%

      # join to recensement_taxon
      dplyr::left_join(
        dplyr::tbl(conn, dbplyr::in_schema("oison_data", "recensement_taxon")) %>%
          dplyr::select(-id) %>%
          dplyr::rename(observation_id = observation_taxon_id) %>%
          # join to taxon_stade_developpement
          dplyr::left_join(
            dplyr::tbl(
              conn,
              dbplyr::in_schema("oison_referentiels", "taxon_stade_developpement")
            ) %>%
              dplyr::select(
                stade_developpement_id = id,
                stade_developpement = label
              )
          ) %>%
          # join to taxon_vivant_trace
          dplyr::left_join(
            dplyr::tbl(
              conn,
              dbplyr::in_schema("oison_referentiels", "taxon_vivant_trace")
            ) %>%
              dplyr::select(vivant_trace_id = id,
                            vivant_trace = label)

          ) %>%
          # join to classe_nombre_individus
          dplyr::left_join(
            dplyr::tbl(
              conn,
              dbplyr::in_schema("oison_referentiels", "classe_nombre_individus")
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
