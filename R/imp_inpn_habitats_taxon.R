imp_inpn_habitats_taxon <- function(df){

  base_url_habitat <- "https://inpn.mnhn.fr/espece/cd_nom/{cd_ref}/tab/habitats"

  oison_df <-
    df %>%
    dplyr::select(nom_vernaculaire, nom_scientifique, cd_ref) %>%
    unique() %>%
    dplyr::mutate(url_esp_hab = glue::glue(base_url_habitat))

  read_url_hab <- function(url_hab, df){

    p()

    tab_taxon <-
      rvest::read_html(url_hab) %>%
      rvest::html_element("#especes") %>%
      rvest::html_table() %>%
      dplyr::select(!!-c(1))

    df %>%
      dplyr::right_join(
        tab_taxon %>%
          dplyr::mutate(url_esp_hab = rep(url_hab, dplyr::n()))) %>%
      suppressMessages()
  }


  poss_readurl_hab <- purrr::possibly(.f = read_url_hab, otherwise = NULL)

  progressr::with_progress({

    p <- progressr::progressor(steps = length(oison_df$url_esp_hab))

    furrr::future_map_dfr(.f = poss_readurl_hab, df = oison_df, .x = oison_df$url_esp_hab) %>%
      dplyr::as_tibble()
  })

}

#
# rvest::read_html('https://inpn.mnhn.fr/espece/cd_nom/2895/tab/habitats') %>%
#   rvest::html_element("#especes") %>%
#   rvest::html_table() %>%
#   dplyr::select(!!-c(1))
