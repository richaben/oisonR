#' Consulter et Télécharger les données OISON pour un département
#'
#' @param dpt_code code INSEE de département (ex. 27)
#' @param login login du compte
#' @param mdp mot de passe associé au compte
#' @param collect_all Booléen. Si FALSE (par défaut) consultation du nombre d'observation (FALSE), si TRUE télécharge les données.
#'
#' @return Si collect_all=FALSE, retourne un dataframe avec le code du département et le nombre d'observation dans celui-ci.
#' Si collect_all=T, retourne un dataframe avec les observations.
#' @export
#'
#' @importFrom cli cli_h1 cli_alert_danger cli_alert_success cli_progress_step cli_alert_info
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
#'
#' # -> consulter le nombre de données dans le département 27
#'
#' get_data_dpt(dpt_code = "27", login = "john.doe@ofb.gouv.fr", mdp = "mon_mdp")
#'
#' #' # -> télécharger les données dans le département 27
#'
#' obs_dpt27 <-
#' get_data_dpt(dpt_code = "27", login = "john.doe@ofb.gouv.fr", mdp = "mon_mdp", collect_all = T)
#' }
#'

get_data_dpt <- function(dpt_code, login, mdp, collect_all = FALSE) {

  cli::cli_h1(glue::glue("R\u00e9cup\u00e9ration des donn\u00e9es OISON - dpt {dpt_code}"))

  ## Connexion url base de l'api
  url_base <- "https://api-oison.ofb.fr/login-check"

  login <- list(
    "_password" = mdp,
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
    token <- paste("Bearer", httr::content(connexion)$token, sep = " ")

    ## Headers pour requete
    headers = c(
      `Authorization` = token,
      `Content-Type` = 'application/json;charset=UTF-8'
    )

    ## Body de la pour requete
    body_dpt <- '{
  "entityName":"App\\\\Entity\\\\Data\\\\Observation",
  "filters":[
    {
      "property":{
        "name":"departement",
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
              "inseeDept":"<<dpt_code>>"
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

    ## requete courte en obs. pour récupérer le nb d'observations total

    min_req <- 1 ; max_req <- 5

    response <- httr::POST(url = 'https://api-oison.ofb.fr/queries/execute-data',
                           httr::add_headers(.headers=headers),
                           body = glue::glue(body_dpt, .open = "<<", .close = ">>"))

    cli::cli_progress_step("Collecte donn\u00e9es en cours...", msg_done = "Fini !")

    totalcount <-
      httr::content(response,"text", encoding = "UTF-8") %>%
      jsonlite::fromJSON() %>%
      .$totalCount

    if(collect_all == F) {
      cli::cli_alert_info("Total observations en base : {totalcount}.")
      data.frame(code_departement = dpt_code, total_obs = totalcount) }
    else {

    ## Recup donnees limitees (< 2500 obs)
    ## -> utilise la fonction pour creer min/max valeurs requete

    tabCount_req <-
      req_min_max(totalcount) %>%
      dplyr::mutate(data_body_req = glue::glue(body_dpt, .open = "<<", .close = ">>"))

    cli::cli_alert_info("Total observations en base : {totalcount} ; Nombre de requ\u00eate \u00e0 suivre : {nrow(tabCount_req)}")

    pb <- progress::progress_bar$new(total = nrow(tabCount_req), force = TRUE)



# data_oison_dpt <-
    tabCount_req$data_body_req %>%
      purrr::map_df(
        function(.x) {
          pb$tick()
          httr::POST(url = 'https://api-oison.ofb.fr/queries/execute-data',
                     httr::add_headers(.headers=headers), body = .x) %>%
            httr::content('text',encoding = 'UTF-8') %>%
            jsonlite::fromJSON() %>%
            extract_requete()
        }
      ) %>%
      tibble::as_tibble()
    }
  }
}
