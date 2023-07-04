#' Consulter et télécharger les données sur les taxons de la bdd OISON pour une région administrative
#'
#' @param region_code Code INSEE de la région (ex. 28 pour la Normandie, ...)
#' @param login login du compte OISON
#' @param mdp mot de passe associé au compte OISON
#' @param collect_all Booléen. Si FALSE (par défaut) consultation du nombre d'observation.
#'  Si TRUE, télécharge les données pour les taxons.
#' @param date_min date minimale pour la période au format (\code{\%Y-\%m-\%d} ; cf. exemple '2022-01-01').
#'  NULL par défaut.
#' @param date_max date maximum pour la période au format (\code{\%Y-\%m-\%d} ; cf. exemple '2022-12-31').
#'  NULL par défaut.
#'
#' @return Si \code{collect_all}=FALSE, retourne un dataframe indiquant le nombre d'observations sur la région choisie.
#'  Si \code{collect_all}=TRUE, retourne un dataframe avec le détail des observations taxons.
#'  Si \code{date_min} et \code{date_max} sont spécifiés, retourne un dataframe avec les valeurs sur la période.
#' @export
#'
#' @importFrom cli cli_abort cli_h1 cli_alert_danger cli_alert_success cli_progress_step cli_alert_info cli_warn
#' @importFrom dplyr mutate
#' @importFrom glue glue
#' @importFrom httr POST warn_for_status content add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom progress progress_bar
#' @importFrom purrr map_df
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' # -> Exemple pour consulter rapidement le nombre de données dans la région
#' # Ile-de-France (code région = 11)
#'
#' get_taxon_region(region_code = "11",
#'                  login = "john.doe@ofb.gouv.fr",
#'                  mdp = "mon_mdp")
#'
#' # -> Exemple pour télécharger les données dans
#' # la région Île-de-France (code région 11)
#'
#' obs_regIDF <-
#'   get_taxon_region(region_code = "11",
#'                    login = "john.doe@ofb.gouv.fr",
#'                    mdp = "mon_mdp",
#'                    collect_all = T)
#'
#' # -> Exemple pour télécharger les données dans la région
#' # Île-de-France (code région 11) sur la période 2022
#'
#' obs_regIDF_2022 <-
#'   get_taxon_region(login = "john.doe@ofb.gouv.fr",
#'                    mdp = "mon_mdp",
#'                    region_code = "11",
#'                    date_min = '2022-01-01',
#'                    date_max = '2022-12-31',
#'                    collect_all = T)
#' }

get_taxon_region <- function(region_code,
                             login,
                             mdp,
                             collect_all = FALSE,
                             date_min = NULL,
                             date_max = NULL) {
  ## Verif. arguments
  if (missing(login) &
      missing(mdp))
    cli::cli_abort("Login et Mot de passe OISON requis !")

  ## Si date_min saisie et pas date_max -> date_max = date du jour
  if (!missing(date_min) & missing(date_max))
    date_max <- format(Sys.Date(), '%Y-%m-%d')

  cli::cli_h1(glue::glue(
    "R\u00e9cup\u00e9ration des donn\u00e9es OISON - region {region_code}"
  ))

  ## Connexion url base de l'api
  url_base <- "https://api-oison.ofb.fr/login-check"

  login <- list("_password" = mdp,
                "_username" = login)

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

    ## Body Json pour la requete
    if (!is.null(date_min) & !is.null(date_max)) {
      body_region  <-
        '{"entityName":"App\\\\Entity\\\\Data\\\\Observation",
      "filters":[
          {
      "property":{
        "name":"region",
        "path":[
          "localisation"
        ]
      },
      "filter":{
        "type":"value",
        "negation":false,
        "value":{
          "operator":"=",
          "value":{
            "type":"object",
            "value":{
              "code":"<<region_code>>"
            }
          }
        }
      }
    },{
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
"first":<<min_req>>, "max":<<max_req>>}'
    } else {
      ## Body simple pour la requete

      body_region <- '{
  "entityName":"App\\\\Entity\\\\Data\\\\Observation",
  "filters":[
    {
      "property":{
        "name":"region",
        "path":[
          "localisation"
        ]
      },
      "filter":{
        "type":"value",
        "negation":false,
        "value":{
          "operator":"=",
          "value":{
            "type":"object",
            "value":{
              "code":"<<region_code>>"
            }
          }
        }
      }
    },
    {
      "property":{
        "name":"type",
        "path":[

        ]
      },
      "filter":{
        "type":"value",
        "value":{
          "operator":"=",
          "value":{
            "type":"string",
            "value":"taxon"
          }
        }
      }
    }
  ],
  "first":<<min_req>>, "max":<<max_req>>
    }'
    }

    ## requete courte pour récupérer le nb d'observations total (et séquencer par la suite les requêtes)

    min_req <- 1
    max_req <- 5

    response <-
      httr::POST(
        url = 'https://api-oison.ofb.fr/queries/execute-data',
        httr::add_headers(.headers = headers),
        body = glue::glue(body_region, .open = "<<", .close = ">>")
      )

    cli::cli_progress_step("Collecte donn\u00e9es en cours...", msg_done = "Fini !")

    totalcount <-
      httr::content(response, "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON() %>%
      .$totalCount

    if (collect_all == F) {
      cli::cli_alert_info("Total observations en base : {totalcount}.")
      data.frame(code_region = region_code, total_obs = totalcount)
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
        dplyr::mutate(data_body_req = glue::glue(body_region, .open = "<<", .close = ">>"))

      cli::cli_alert_info(
        "Total observations en base : {totalcount} (du {date_min} au {date_max}); Nombre de requ\u00eate \u00e0 suivre : {nrow(tabCount_req)}"
      )

      } else {
        tabCount_req <-
          req_min_max(totalcount) %>%
          dplyr::mutate(data_body_req = glue::glue(body_region, .open = "<<", .close = ">>"))

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
        }) %>%
        tibble::as_tibble()
    }
  }
}


