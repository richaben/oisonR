# Fonctions utiles

#' Tableau nombres min et max à requêter
#'
#' @noRd
#' @description  L'api semble limitée en nombre d'observation possible à récupérer.
#'     Nécessaire de récupérer en plusieurs fois.
#'     La fonction sert à générer les min et max à inclure les requêtes individuelles.
#'     Les min/max sont créer en fonction du nombre total d'observations, à pas de 1000 observations.
#'
#' @param totalcount Nombre total d'observation
#'
#' @return retourne un tableau avec les min_req et max_req
#'
req_min_max <- function(totalcount){
  if(totalcount > 1000){

    (count <- totalcount + (1000 - totalcount %% 1000))
    (C <- seq(0,count, by = 1000))
    rep(C,c(1,rep(2,length(C)-2),1)) %>%
      matrix(ncol=2,byrow=TRUE) %>%
      data.frame() %>%
      `colnames<-`(c("min_req", "max_req"))
  } else {

    return(data.frame(min_req = 0, max_req = totalcount))

  }
}

#' Unnest conditionnel
#'
#' @noRd
#' @param df data.frame du resultat de la requete
#' @param var nom de la variable a deplier
#'
#' @return retourne un data.frame avec l'ensemble des champs deplié
#'
#' @importFrom dplyr select
#' @importFrom tidyr unnest tidyr_legacy
#'

unnest_conditionnel <- function(df, var){
  if(var %in% names(df)){
    return(df %>% tidyr::unnest(tidyr::all_of(var), names_repair = tidyr::tidyr_legacy) %>%
             dplyr::select(id,
                           corineBiotope_id = id1,
                           corineBiotope_label = label,
                           corineBiotope_code = code,
                           corineBiotope_parentCode = parentCode))
  } else{
    df <- data.frame(id = df$id,
                     corineBiotope_id = rep(NA,dim(df)[1]),
                     corineBiotope_label = rep(NA,dim(df)[1]),
                     corineBiotope_code = rep(NA,dim(df)[1]),
                     corineBiotope_parentCode = rep(NA,dim(df)[1]))
    return(df)
  }
}

#' Pour traiter le contenu de la requete et la mettre en forme
#'
#' @noRd
#' @param requete_df resultats de la requete
#'
#' @return retourne un data.frame avec l'ensemble des champs
#'
#' @importFrom dplyr select bind_cols left_join mutate_if filter
#' @importFrom purrr pluck map
#' @importFrom tidyr unnest tidyr_legacy
#' @importFrom tidyselect any_of

extract_requete <- function(requete_df) {

  part_base <-
    requete_df$items %>%
    dplyr::select(tidyselect::any_of(c('id', 'uuid', 'type', 'date', 'time'))) %>%
    dplyr::bind_cols(
      ### taxon infos
      requete_df$items %>%
        purrr::pluck("taxon") %>%
        dplyr::select(nom_vernaculaire = vernacularName,
                      nom_scientifique = name,
                      cd_ref = code,
                      parentCode,
                      regne,
                      rang))

    ### presence infos
    presence <-
      requete_df$items %>%
      dplyr::select(any_of(c("presence")))

    if(ncol(presence) == 0){
      presence <-
        requete_df$items %>%
        dplyr::select(any_of(c("id","presence"))) %>%
        mutate(presence = rep("NA",nrow(requete_df$items)))
    }  else {
      presence <- requete_df$items %>%
        dplyr::select(any_of(c("id", "presence"))) %>%
        tidyr::unnest(c(presence),names_sep = '_',keep_empty = T) %>%
        dplyr::select(any_of(c("id", "presence_label"))) %>%
        dplyr::rename(presence = presence_label)
    }

    part_base <-
      left_join(part_base, presence) %>%
      suppressMessages() %>%

    ### Observateur infos
    dplyr::bind_cols(
      requete_df$items %>%
        purrr::pluck("initiatorUser") %>%
        dplyr::select(obs_nom = name,
                      obs_prenom = firstName,
                      obs_mail = username,
                      obs_id = id)) %>%

    # dplyr::bind_cols(requete_df$items %>%
    #                    purrr::pluck("commentaire",
    #                                 .default = data.frame(commentaire = rep(NA,length(requete_df$items$id))))) %>%

    ### statut data infos
    dplyr::bind_cols(
      requete_df$items %>%
        purrr::pluck("status",
                     .default = data.frame(label = rep(NA,length(requete_df$items$id)))) %>%
        dplyr::select(statut_obs = label)) %>%

    ### objectif recherche infos
    dplyr::bind_cols(
      requete_df$items %>%
        purrr::pluck("objectifRecherche",
                     .default = data.frame(label = rep(NA,length(requete_df$items$id)))) %>%
        dplyr::select(obj_recherche = label)
    ) %>%
    ### contexte recherche infos
    dplyr::bind_cols(
      requete_df$items %>%
        purrr::pluck("contexteRecherche",
                     .default = data.frame(label = rep(NA,length(requete_df$items$id)))) %>%
        dplyr::select(contexte_recherche = label)
    ) %>%

    ### type recherche infos
    dplyr::bind_cols(
      requete_df$items %>%
        purrr::pluck("typeRecherche",
                     .default = data.frame(label = rep(NA,length(requete_df$items$id)))) %>%
        dplyr::select(type_recherche = label)
    )

  ### partie commentaires
  commentaires <-
    requete_df$items %>%
    dplyr::select(any_of(c("commentaire")))

  if(ncol(commentaires) == 0){
    commentaires <- requete_df$items %>%
      dplyr::select(any_of(c("id")))%>%
      mutate(commentaire = rep("NA",nrow(requete_df$items)))
  } else {
    commentaires <-
      requete_df$items %>%
      dplyr::select(any_of(c("id","commentaire")))
  }

  part_base <- left_join(part_base,commentaires) %>%
    suppressMessages()

  ### recensements taxon infos
  recensementTaxon <-
    requete_df$items %>%
    dplyr::select(id, recensementsTaxon) %>%
    tidyr::unnest(keep_empty = T) %>%
    suppressMessages()

  if(nrow(recensementTaxon) != 0){
    cols_recensementsTaxon <- c(id = 'id', std_dvpt = 'label', nbInd = 'nombreIndividu')

    recensementTaxon <-
      requete_df$items %>%
      dplyr::select(id, recensementsTaxon) %>%
      tidyr::unnest(c(recensementsTaxon),names_repair = tidyr::tidyr_legacy, keep_empty = T) %>% # recensementsTaxon
      tidyr::unnest() %>%
      tidyr::unnest() %>%
      dplyr::select(tidyselect::any_of(c("id", cols_recensementsTaxon))) %>%
      suppressMessages()
  }

  ### infos localisation
  part_localisation <-
    requete_df$items %>%
    dplyr::select(id, localisation) %>%
    tidyr::unnest(c(localisation),names_repair = tidyr::tidyr_legacy, keep_empty = T) %>%
    dplyr::mutate_if(is.list, ~purrr::map(.,~data.frame(.))) %>%
    tidyr::unnest(c(commune),names_sep = '_',keep_empty = T) %>%
    tidyr::unnest(c(departement),names_sep = '_',keep_empty = T) %>%
    tidyr::unnest(c(region),names_sep = '_',keep_empty = T) %>%
    tidyr::unnest(c(xPoint,yPoint),names_sep = '_',keep_empty = T) %>%
    dplyr::filter(!grepl("Arrondissement", commune_nomComm)) %>%
    dplyr::select(tidyselect::any_of(c("id",
                                       "geometry",
                                       "commune_label",
                                       "commune_inseeComm",
                                       "commune_nomDept",
                                       "region_nom",
                                       'region_code',
                                       "xPoint",
                                       'yPoint',
                                       'longueurTroncon'))) %>%
    suppressMessages()

  requete_df$items %>%
    dplyr::select(tidyselect::any_of(c("id","corineBiotope"))) %>%
    unnest_conditionnel('corineBiotope')


  left_join(part_base,recensementTaxon) %>%
    left_join(part_localisation) %>%
    suppressMessages() %>%
    dplyr::mutate(cd_ref = as.numeric(cd_ref),
                  parentCode = as.numeric(parentCode))
}


###############################################################################
###############################################################################
###############################################################################

#' Export et mise en forme des resultats
#'
#' @noRd
#' @param resultats_req resultats de la requete
#'
#' @return retourne un data.frame avec l'ensemble des champs
#'
#' @importFrom dplyr select bind_cols rename left_join mutate_if filter
#' @importFrom purrr pluck map
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest tidyr_legacy
#'
# extract_resultats <- function(resultats_req) {
#
#   resultats_req$items %>%
#     #dplyr::select(any_of(id, uuid, type, date, heure = time)) %>%
#     dplyr::select(any_of(c('id', 'uuid', 'type', 'date', 'time'))) %>%
#     dplyr::bind_cols(
#       ### taxon infos
#       resultats_req$items %>%
#         purrr::pluck("taxon") %>%
#         dplyr::select(nom_vernaculaire = vernacularName,
#                       nom_scientifique = name,
#                       cd_nom = code,
#                       cd_nom_parent = parentCode,
#                       regne,
#                       rang)) %>%
#     ### presence infos
#     dplyr::bind_cols(
#       resultats_req$items %>%
#         purrr::pluck("presence") %>%
#         dplyr::select(type_presence = label)) %>%
#     ### Observateur infos
#     dplyr::bind_cols(
#       resultats_req$items %>%
#         purrr::pluck("initiatorUser") %>%
#         dplyr::select(obs_nom = name,
#                       obs_prenom = firstName,
#                       obs_mail = username,
#                       obs_id = id)) %>%
#     ### Co-Observateur infos
#     # dplyr::bind_cols(
#     #   resultats_req$items %>%
#     #     dplyr::select(coObservateur)) %>%
#     dplyr::bind_cols(resultats_req$items %>%
#                        purrr::pluck("coObservateur",
#                                     .default = data.frame(coObservateur = rep(NA,length(resultats_req$items$id)))) %>%
#                        tibble::as_tibble() %>%
#                        dplyr::rename(coObservateur = value)) %>%
#     ### commentaires infos
#     # dplyr::bind_cols(
#     #   resultats_req$items %>%
#     #     dplyr::select(commentaire)) %>%
#     dplyr::bind_cols(resultats_req$items %>%
#                        purrr::pluck("commentaire",
#                                     .default = data.frame(commentaire = rep(NA,length(resultats_req$items$id)))) %>%
#                        tibble::as_tibble() %>%
#                        dplyr::rename(commentaire = value)) %>%
#
#     ### statut data infos
#     dplyr::bind_cols(
#       resultats_req$items %>%
#         purrr::pluck("status",
#                      .default = data.frame(label = rep(NA,length(resultats_req$items$id)))) %>%
#         dplyr::select(statut_obs = label)) %>%
#     ### objectif recherche infos
#     dplyr::bind_cols(
#       resultats_req$items %>%
#         purrr::pluck("objectifRecherche",
#                      .default = data.frame(label = rep(NA,length(resultats_req$items$id)))) %>%
#         dplyr::select(obj_recherche = label)) %>%
#     ### contexte recherche infos
#     dplyr::bind_cols(
#       resultats_req$items %>%
#         purrr::pluck("contexteRecherche",
#                      .default = data.frame(label = rep(NA,length(resultats_req$items$id)))) %>%
#         dplyr::select(contexte_recherche = label)) %>%
#     ### type recherche infos
#     dplyr::bind_cols(
#       resultats_req$items %>%
#         purrr::pluck("typeRecherche",
#                      .default = data.frame(label = rep(NA,length(resultats_req$items$id)))) %>%
#         dplyr::select(type_recherche = label)) %>%
#     ### recensements taxon infos
#     dplyr::left_join(resultats_req$items %>%
#                        dplyr::select(id, recensementsTaxon) %>%
#                        tidyr::unnest(c(recensementsTaxon),
#                                      names_repair = tidyr::tidyr_legacy, keep_empty = T) %>%
#                        tidyr::unnest(cols = c(stadeDeveloppement, classeNombreIndividus, vivantTrace),
#                                      names_repair = tidyr::tidyr_legacy, keep_empty = T) %>%
#                        dplyr::select(id = id,
#                                      std_dvpt = label,
#                                      nbInd = nombreIndividu,
#                                      nbInd_class = label1) %>%
#                        suppressMessages()
#     ) %>%
#     ### localisation infos
#     dplyr::left_join(
#       resultats_req$items %>%
#         dplyr::select(id, localisation) %>%
#         tidyr::unnest(c(localisation),names_repair = tidyr::tidyr_legacy, keep_empty = T) %>%
#         dplyr::mutate_if(is.list, ~purrr::map(.,~data.frame(.))) %>%
#         tidyr::unnest(c(commune),names_sep = '_',keep_empty = T) %>%
#         tidyr::unnest(c(departement),names_sep = '_',keep_empty = T) %>%
#         tidyr::unnest(c(region),names_sep = '_',keep_empty = T) %>%
#         tidyr::unnest(c(znieff1,znieff2),names_sep = '_',keep_empty = T) %>%
#         tidyr::unnest(c(sic),names_sep = '_',keep_empty = T) %>%
#         tidyr::unnest(c(zps),names_sep = '_',keep_empty = T) %>%
#         tidyr::unnest(c(contextePiscicole),names_sep = '_',keep_empty = T) %>%
#         tidyr::unnest(c(masseEau),names_sep = '_',keep_empty = T) %>%
#         tidyr::unnest(c(xPoint,yPoint),names_sep = '_',keep_empty = T) %>%
#         dplyr::filter(!grepl("Arrondissement", commune_nomComm)) %>%
#         dplyr::select(-apb,-rnn,-id1)
#     ) %>%
#     ### localisation corineBiotope
#     # dplyr::left_join(
#     #   resultats_req$items %>%
#     #     dplyr::select(id, corineBiotope) %>%
#     #     tidyr::unnest(c(corineBiotope), names_repair = tidyr::tidyr_legacy, keep_empty = T) %>%
#     #     dplyr::select(id,
#     #                   corineBiotope_id = id1,
#     #                   corineBiotope_label = label,
#     #                   corineBiotope_code = code,
#     #                   corineBiotope_parentCode = parentCode)
#     # )
#   dplyr::left_join(
#     resultats_req$items %>%
#       dplyr::select(any_of(c("id","corineBiotope"))) %>%
#       unnest_conditionnel('corineBiotope')) %>%
#     suppressMessages()
#
# }


#' #' @importFrom progress progress_bar
#' collect_extract_requete <- function(df_tableCount_req) {
#'   pb <- progress::progress_bar$new(total = nrow(df_tableCount_req))
#'
#'   df_tableCount_req$data_body_req %>%
#'     purrr::map_df(
#'       function(.x) {
#'         pb$tick()
#'         httr::POST(url = 'https://api-oison.ofb.fr/queries/execute-data',
#'                    httr::add_headers(.headers=headers), body = .x) %>%
#'           httr::content('text',encoding = 'UTF-8') %>%
#'           jsonlite::fromJSON() %>%
#'           extract_resultats()
#'       }
#'     )
#' }
