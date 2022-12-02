imp_inpn_maille_taxon <- function(df, shape){

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
