#' Récupération des mailles 10x10km sur l'INPN sur une zone shape
#'
#' @param df un dataframe issu de la requete OISON
#' @param shape un dataframe contenant une couche shape pour la zone souhaitée
#'
#' @return un dataframe avec les mailles 10x10km INPN
#' @export
#'
#' @importFrom dplyr select mutate slice n left_join as_tibble
#' @importFrom furrr future_map_dfr
#' @importFrom geojsonsf geojson_sf
#' @importFrom glue glue
#' @importFrom progressr with_progress progressor
#' @importFrom purrr possibly
#' @importFrom sf st_intersects
#'
#' @examples
#' \dontrun{
#'
#' data_oison <- get_data_dpt(dpt_code = "27",
#' login = "john.doe@ofb.gouv.fr", mdp = "mon_mdp")
#'
#' normandie_shp <-
#' readRDS(file = './regions_geo_normandie.rds') %>%
#' sf::st_transform(crs = 4326)
#'
#' maille_inpn <- add_inpn_maille_taxon(df = data_oison,  shape = normandie_shp)
#'
#' }
#'

add_inpn_maille_taxon <- function(df, shape){

  base_url_geometrie <- "https://odata-inpn.mnhn.fr/geometries/grids/taxon/{cd_ref}"

  oison_df <-
    df %>%
    dplyr::select(nom_vernaculaire, nom_scientifique, cd_ref) %>%
    unique() %>%
    dplyr::mutate(url_esp_maille = glue::glue(base_url_geometrie))

  read_url_maille <- function(url_geom_inpn, shape, df) {

    p()

    grille_10km2 <-
      url_geom_inpn %>%
      geojsonsf::geojson_sf()

    list_mailles <- sf::st_intersects(shape, grille_10km2)

      grille_10km2 %>%
        dplyr::slice(unlist(list_mailles)) %>%
        dplyr::mutate(url_esp_maille = rep(url_geom_inpn, dplyr::n())) %>%
        dplyr::left_join(df) %>%
        suppressMessages()
      }

  poss_readurl_maille <- purrr::possibly(.f = read_url_maille, otherwise = NULL)

  progressr::with_progress({
    p <- progressr::progressor(steps = length(oison_df$url_esp_maille))
    furrr::future_map_dfr(.x = oison_df$url_esp_maille,
                          .f=  read_url_maille , shape = shape,
                          df = oison_df, .progress = T) %>%
      dplyr::as_tibble()
  })
}
