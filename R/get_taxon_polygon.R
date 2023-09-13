#' Consulter et télécharger les données OISON sur une zone définie (type polygone)
#'
#' @param zone_selection un objet contenant le zone (polygon), au CRS RGF93 (epsg: 2154)
#' @param login login du compte OISON
#' @param mdp mot de passe associé au compte OISON
#' @param collect_all Booléen. Si FALSE (par défaut) consultation du nombre d'observation (FALSE).
#'  Si TRUE télécharge les données.
#' @param draw_zone Booléen. Si TRUE, lance l'outil de sélection de zones.
#'  Par défaut, FALSE.
#' @param date_min date minimale pour la période au format (\code{\%Y-\%m-\%d} ; cf. exemple '2022-01-01').
#'  NULL par défaut.
#' @param date_max date maximum pour la période au format (\code{\%Y-\%m-\%d} ; cf. exemple '2022-12-31').
#'  NULL par défaut.
#'
#' @return Si \code{collect_all}=FALSE, retourne uniquement un dataframe avec le nombre d'observations associé
#'  sur la zone géographique sélectionnée.
#'  Si \code{collect_all}=TRUE, retourne un dataframe avec les observations pour les taxons.
#'  Si \code{date_min} et \code{date_max} sont spécifiés, retourne un dataframe avec les valeurs sur la période.
#' @export
#'
#' @importFrom cli cli_abort cli_alert_warning cli_h1 cli_alert_danger cli_alert_success cli_progress_step cli_alert_info cli_warn
#' @importFrom dplyr mutate
#' @importFrom glue glue
#' @importFrom httr POST warn_for_status content add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom progress progress_bar
#' @importFrom purrr map_df
#'
#' @examples
#' \dontrun{
#' rm(selection_zone)
#'
#' ## Exemple 1) Collecter des données sur un polygone défini manuellement:
#' test_zone <-
#'   "POLYGON ((410677.9 6877209, 413275.2 6929083, 496961.8 6925506,
#' 495122.6 6873600, 410677.9 6877209))"
#'
#' get_taxon_polygon(zone_selection = test_zone,
#'                   login = "john.doe@ofb.gouv.fr",
#'                   mdp = "mon_mdp",
#'                   collect_all = F,
#'                   draw_zone = F)
#'
#' ## Exemple 2) Consulter des données sur un polygone tracé via l'outil de traçage:
#' get_taxon_polygon(login = "john.doe@ofb.gouv.fr",
#'                   mdp = "mon_mdp",
#'                   collect_all = F,
#'                   draw_zone = T)
#'
#' ## Exemple 3) Collecter des données sur un polygone tracé en même temps avec l'outil de traçage:
#' get_taxon_polygon(login = "john.doe@ofb.gouv.fr",
#'                   mdp = "mon_mdp",
#'                   collect_all = T,
#'                   draw_zone = T)
#'
#' ## Exemple 4) Consulter des données sur un polygone déjà sauvegardé dans l'environnement:
#' get_taxon_polygon(login = "john.doe@ofb.gouv.fr",
#'                   mdp = "mon_mdp",
#'                   collect_all = F,
#'                   draw_zone = F)
#'
#' ## Exemple 5) Consulter des données sur un polygone connu et
#' ## défini manuellement sur une période précise:
#'
#' test_zone <-
#'   "POLYGON ((410677.9 6877209, 413275.2 6929083, 496961.8 6925506,
#' 495122.6 6873600, 410677.9 6877209))"
#'
#' get_taxon_polygon(zone_selection = test_zone,
#'                   login = "john.doe@ofb.gouv.fr",
#'                   mdp = "mon_mdp",
#'                   collect_all = F,
#'                   draw_zone = T,
#'                   date_min = '2022-01-01',
#'                   date_max = '2022-12-31')
#' }

get_taxon_polygon <-
  function(zone_selection,
           login,
           mdp,
           collect_all = FALSE,
           draw_zone = FALSE,
           date_min = NULL,
           date_max = NULL) {


    ## Verif. arguments
    if (missing(login) &
        missing(mdp))
      cli::cli_abort("Login et Mot de passe OISON requis !")

    ## Si date_min saisie et pas date_max -> date_max = date du jour
    if (!missing(date_min) & missing(date_max))
      date_max <- format(Sys.Date(), '%Y-%m-%d')

    if (missing(zone_selection) &
        exists("selection_zone") == TRUE & draw_zone == FALSE) {

      cli::cli_alert_warning(
        "Un object 'selection_zone' existe d\u00e9j\u00e0 dans l\'environnement.
                           Il sera utilis\u00e9 pour r\u00e9cup\u00e9rer les donn\u00e9es.
                           Relancez-la fonction 'outil_selection_zone()' pour une nouvelle s\u00e9lection."
      )

      zone_selection <- selection_zone

    }

    if (draw_zone == T) {
      outil_selection_zone()
      zone_selection <- selection_zone
    }

    cli::cli_h1(glue::glue("R\u00e9cup\u00e9ration des donn\u00e9es OISON sur Polygones"))

    ## Connexion url base de l'api
    url_base <- "https://api-oison.ofb.fr/login-check"

    login <- list("password" = mdp,
                  "username" = login)

    connexion <- httr::POST(url_base,
                            body = login,
                            encode = "json")
    ## Check HTTP Status
    httr::warn_for_status(connexion)

    if (connexion$status_code != 200) {
      cli::cli_alert_danger("Echec connexion - V\u00e9rifiez votre login/mdp !")
    } else {
      ## message si connexion OK !
      cli::cli_alert_success("Connexion OISON - OK !")

      ## Recup Token de la connexion
      token <-
        paste("Bearer", httr::content(connexion)$token, sep = " ")

      ## Headers pour requete
      headers = c(`Authorization` = token,
                  `Content-Type` = 'application/json;charset=UTF-8')

      ## Body de la pour requete

      if (!is.null(date_min) & !is.null(date_max)) {


      body_polygon <- '{
  "entityName":"App\\\\Entity\\\\Data\\\\Observation", "filters":[{
    "property":{
      "name":"geometry", "path":["localisation"]
      }, "filter":{
        "type":"value", "negation":false, "value":{
          "operator":"=", "value":{
            "type":"string",
            "value":"<<zone_selection>>"
          }
        }
      }
      }, {
  "property":{
    "name":"date", "path":[]
  }, "filter":{
    "type":"range", "negation":false, "valueRange":{
      "values":[{
        "type":"string", "value":"<<date_min>>"
      }, {
        "type":"string", "value":"<<date_max>>"
      }]
    }
  }
},{
        "property":{
          "name":"type", "path":[]
          }, "filter":{
            "type":"value", "value":{
              "operator":"=", "value":{
                "type":"string", "value":"taxon"
              }
            }
          }
          }],
          "first":<<min_req>>, "max":<<max_req>>
      }'
      } else {
        body_polygon <- '{
  "entityName":"App\\\\Entity\\\\Data\\\\Observation", "filters":[{
    "property":{
      "name":"geometry", "path":["localisation"]
      }, "filter":{
        "type":"value", "negation":false, "value":{
          "operator":"=", "value":{
            "type":"string",
            "value":"<<zone_selection>>"
          }
        }
      }
      }, {
        "property":{
          "name":"type", "path":[]
          }, "filter":{
            "type":"value", "value":{
              "operator":"=", "value":{
                "type":"string", "value":"taxon"
              }
            }
          }
          }],
          "first":<<min_req>>, "max":<<max_req>>
      }'
}

      ## requete courte en obs. pour récupérer le nb d'observations total

      min_req <- 1
      max_req <- 5

      response <-
        httr::POST(
          url = 'https://api-oison.ofb.fr/queries/execute-data',
          httr::add_headers(.headers = headers),
          body = glue::glue(body_polygon, .open = "<<", .close = ">>")
        )

      cli::cli_progress_step("Collecte donn\u00e9es en cours...", msg_done = "Fini !")

      totalcount <-
        httr::content(response, "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON() %>%
        .$totalCount

      if (collect_all == F) {
        cli::cli_alert_info("Total observations en base : {totalcount}.")
        data.frame(zone_selection = zone_selection, total_obs = totalcount)
      }
      else if (totalcount == 0) {
        cli::cli_warn("Pas de donn\u00e9es \u00e0 t\u00e9l\u00e9charger.")
      }
      else {

        ## Recup donnees limitees (< 2500 obs difficile)
        ## -> utilise la fonction `req_min_max` pour creer min/max des valeurs de requete

        if (!is.null(date_min) & !is.null(date_max)) {

        tabCount_req <-
          req_min_max(totalcount) %>%
          dplyr::mutate(date_min = rep(date_min, n()),
                        date_max = rep(date_max, n())) %>%
          dplyr::mutate(data_body_req = glue::glue(body_polygon, .open = "<<", .close = ">>"))

        cli::cli_alert_info(
          "Total observations en base : {totalcount} (du {date_min} au {date_max}); Nombre de requ\u00eate \u00e0 suivre : {nrow(tabCount_req)}"
        )

        } else {
          tabCount_req <-
            req_min_max(totalcount) %>%
            dplyr::mutate(data_body_req = glue::glue(body_polygon, .open = "<<", .close = ">>"))

          cli::cli_alert_info(
            "Total observations en base : {totalcount} ; Nombre de requ\u00eate \u00e0 suivre : {nrow(tabCount_req)}"
          )
        }

        # Ajout barre de progression
        pb <-
          progress::progress_bar$new(total = nrow(tabCount_req), force = TRUE)

        # dataframe mise en forme
        tabCount_req$data_body_req %>%
          purrr::map_df(function(.x) {
            pb$tick()
            httr::POST(url = 'https://api-oison.ofb.fr/queries/execute-data',
                       httr::add_headers(.headers = headers),
                       body = .x) %>%
              httr::content('text', encoding = 'UTF-8') %>%
              jsonlite::fromJSON() %>%
              extract_requete()
          })
      }
    }
  }
