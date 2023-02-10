#' Consulter et Télécharger les données OISON sur une zone définie
#'
#' @param zone_selection un objet contenant le zone (polygon), RGF93 (epsg: 2154)
#' @param login login du compte
#' @param mdp mot de passe associé au compte
#' @param collect_all Booléen. Si FALSE (par défaut) consultation du nombre d'observation (FALSE), si TRUE télécharge les données.
#' @param draw_zone Booléen. Si TRUE, lance l'outil de sélection de zones. Par défaut, FALSE.
#'
#' @return Si collect_all=FALSE, retourne un dataframe avec le polygone et le nombre d'observation dans celui-ci.
#' Si collect_all=T, retourne un dataframe avec les observations.
#' @export
#'
#' @importFrom cli cli_alert_warning cli_h1 cli_alert_danger cli_alert_success cli_progress_step cli_alert_info
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
#' ## -> collecte des donnees sur polygone defini manuellement
#'
#' test_zone <-
#' "POLYGON ((410677.9 6877209, 413275.2 6929083,
#' 496961.8 6925506, 495122.6 6873600, 410677.9 6877209))"
#'
#' get_data_polygon(zone_selection = test_zone,
#'                  login = "john.doe@ofb.gouv.fr", mdp = "mon_mdp",
#'                  collect_all = F, draw_zone = F)
#'
#' ## -> consulter des données sur polygone tracé en meme temps
#'
#' get_data_polygon(login = "john.doe@ofb.gouv.fr", mdp = "mon_mdp",
#'                  collect_all = F, draw_zone = T)
#'
#' ## -> collecte des données sur polygone tracé en meme temps
#'
#' get_data_polygon(login = "john.doe@ofb.gouv.fr", mdp = "mon_mdp",
#'                  collect_all = T, draw_zone = T)
#'
#' ## -> consulte des données sur polygon déjà dans l'environnement
#'
#' get_data_polygon(login = "john.doe@ofb.gouv.fr", mdp = "mon_mdp",
#'                  collect_all = F, draw_zone = F)
#'
#' }
#'

get_data_polygon <- function(zone_selection, login, mdp, collect_all = FALSE, draw_zone = FALSE){

  if(missing(zone_selection) & exists("selection_zone") == TRUE & draw_zone == FALSE){
    cli::cli_alert_warning("Un object 'selection_zone' existe d\u00e9j\u00e0 dans l\'environnement.
                           Il sera utilis\u00e9 pour r\u00e9cupérer les donn\u00e9es.
                           Relancez-la fonction 'outil_selection_zone()' pour une nouvelle s\u00e9lection.")

    zone_selection <- selection_zone

  }

  if(draw_zone == T){
    outil_selection_zone()
    zone_selection <- selection_zone
  }

  cli::cli_h1(glue::glue("R\u00e9cup\u00e9ration des donn\u00e9es OISON sur Polygones"))

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

    ## requete courte en obs. pour récupérer le nb d'observations total

    min_req <- 1 ; max_req <- 5

    response <- httr::POST(url = 'https://api-oison.ofb.fr/queries/execute-data',
                           httr::add_headers(.headers=headers),
                           body = glue::glue(body_polygon, .open = "<<", .close = ">>"))

    cli::cli_progress_step("Collecte donn\u00e9es en cours...", msg_done = "Fini !")

    totalcount <-
      httr::content(response,"text", encoding = "UTF-8") %>%
      jsonlite::fromJSON() %>%
      .$totalCount

    if(collect_all == F) {
      cli::cli_alert_info("Total observations en base : {totalcount}.")
      data.frame(zone_selection = zone_selection, total_obs = totalcount) }
    else if(totalcount == 0){
      cli::cli_warn("Pas de données à télécharger.") }
    else {

      ## Recup donnees limitees (< 2500 obs)
      ## -> utilise la fonction pour creer min/max valeurs requete

      tabCount_req <-
        req_min_max(totalcount) %>%
        dplyr::mutate(data_body_req = glue::glue(body_polygon, .open = "<<", .close = ">>"))

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
        )
    }
  }
}



