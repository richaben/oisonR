#' Fonction pour importer le dump de la base OISON
#'
#' @param fichier_dump chemin vers l'objet .sql du dump
#'
#' @return L'ensemble des tables de la base (sauf tables 'mapping')
#' @export
#'
#' @importFrom dplyr mutate arrange filter group_by mutate_all pull
#' @importFrom glue glue
#' @importFrom purrr map reduce set_names
#' @importFrom stringi stri_detect_fixed
#' @importFrom stringr str_replace_all str_extract str_extract_all str_sub str_split
#' @importFrom utils type.convert
#' @importFrom progress progress_bar
#' @importFrom purrr map
#' @importFrom cli cli_h1 cli_progress_step
#'
#' @examples
#'
#' \dontrun{
#' imp_importer_dump_oison(fichier_dump = "./raw_data/PROD_oison.sql")
#' }
#'

imp_importer_dump_oison <- function(fichier_dump) {

  ### fonction pour lire le dump
  imp_lire_lignes_dump_oison <-
    function(fichier_dump) {
      # lecture brute du fichier texte
      readLines(fichier_dump, encoding = "UTF-8")
    }

  ### fonction pour extraire noms tables
  imp_extraire_noms_tables_oison <-
    function(lignes_dump) {
      options(stringsAsFactors = F)
      # NOMS DES TABLES
      # ------------------------------------------------------------
      # Les noms des tables sont repérés car ils suivent "CREATE TABLE"
      noms_tables <-
        lignes_dump[which(stringi::stri_detect_fixed(lignes_dump, "CREATE TABLE"))] %>%
        stringr::str_replace_all(pattern = "CREATE TABLE ", replacement = "") %>%
        stringr::str_replace_all(pattern = " \\(", replacement = "")
    }

  # ------------------------------------------------------------
  ### fonction pour trouver la ligne de fin de chaque table
  imp_trouver_index_fin <-
    function (vecteur_index_lignes_fin, ligne_debut) {
      which(vecteur_index_lignes_fin > ligne_debut) %>%
        min() %>%
        vecteur_index_lignes_fin[.]
    }

  ### fonction pour trouver la ligne de début de chaque table
  trouver_index_ligne_debut <-
    function(ligne, df_tables, lignes_dump)

    {
      pb$tick()
      prov <- list()
      prov[[1]] <- df_tables[ligne, "nom_table"]
      prov[[2]] <- df_tables[ligne, "ligne_debut"]
      prov[[3]] <-
        which(stringi::stri_detect_fixed(lignes_dump, prov[[2]]))

      prov %>% unlist()

    }

  # ------------------------------------------------------------
  # Extraction
  # ------------------------------------------------------------

  cli::cli_h1(glue::glue(
    "Extraction des tables du dump sql OISON"
  ))

  # déclaration de noms de variables pour éviter des warnings en compilant le package
  tables_a_extraire <-
    index_ligne_debut <- index_ligne_fin <- nom_table <- NULL

  # lecture brute du fichier texte

  cli::cli_progress_step("Lecture du dump...")

  lignes_dump <- imp_lire_lignes_dump_oison(fichier_dump)

  options(stringsAsFactors = F)

  # noms des tables
  cli::cli_progress_step("Lecture des noms de tables...")

  noms_tables <-
    imp_extraire_noms_tables_oison(lignes_dump = lignes_dump)

  # traitement sur les tables
  ## -> tables 'mapping' trop volumineuses, on enlève !
  noms_tables <-
    noms_tables[!grepl(x = noms_tables, pattern = "mapping")]

  ## -> recuperation du prefix des tables
  prefix_tables <-
    stringr::str_extract(noms_tables, pattern = '[a-z]*\\.') %>%
    gsub(x = .,
         pattern = "\\.",
         replacement = "")

  noms_tables <-
    gsub(noms_tables, pattern = "[a-z]*\\." , replacement = "")

  # Récupération des débuts de lignes
  cli::cli_progress_step("R\u00e9cup\u00e9ration des lignes de d\u00e9but pour chaque table...")

  # début de table
  lignes_debut <-
    glue::glue(
      "-- Data for Name: {noms_tables}; Type: TABLE DATA; Schema: {prefix_tables}; Owner: oison"
    )

  # On va constituer un dataframe avec tout le nécessaire pour spliter ensuite en tables
  caracteristiques_tables <-
    data.frame(nom_table = noms_tables, ligne_debut = lignes_debut)

  # creation d'une barre de progression
  pb <-
    progress::progress_bar$new(total = nrow(caracteristiques_tables), force = TRUE)
  # pour le début des tables

    caracteristiques_tables <-
      purrr::map(
        .x = 1:nrow(caracteristiques_tables),
        .f = trouver_index_ligne_debut,
        lignes_dump = lignes_dump,
        df_tables = caracteristiques_tables
      ) %>%
      purrr::reduce(rbind)  %>%
      as.data.frame()

  if (length(caracteristiques_tables) == 1) {
    caracteristiques_tables <- t(caracteristiques_tables)
  }

  caracteristiques_tables <- caracteristiques_tables %>%
    as.data.frame() %>%
    purrr::set_names(c("nom_table", "contenu_ligne_debut", "index_ligne_debut")) %>%
    dplyr::mutate(index_ligne_debut = as.integer(index_ligne_debut)) %>%
    dplyr::arrange(index_ligne_debut) %>%
    dplyr::filter(!is.na(index_ligne_debut))

  # recherche des numéros de lignes de fin de chaque table, repérées par "."
  vecteur_index_lignes_fin <-
    which(stringi::stri_detect_fixed(lignes_dump, "\\."))

  # application sur le dataframe
  caracteristiques_tables <- caracteristiques_tables %>%
    dplyr::group_by(nom_table) %>% # nécessaire car la fonction imp_trouver_index_fin n'accepte pas les vecteurs en entrée
    dplyr::mutate(
      index_ligne_fin = imp_trouver_index_fin(
        vecteur_index_lignes_fin = vecteur_index_lignes_fin,
        ligne_debut = index_ligne_debut
      )
    ) %>%
    dplyr::mutate(index_ligne_fin = index_ligne_fin - 1,
                  index_ligne_debut = index_ligne_debut + 3)

  # Fonction qui extrait les lignes d'une table, les sépare en colonnes puis nomme les colonnes
  extraire_table <-
    function(numero_table,
             lignes_dump,
             caracteristiques_tables)

    {
      pb$tick()

      index_ligne_debut <- caracteristiques_tables[numero_table, ] %>%
        dplyr::pull(index_ligne_debut)

      index_ligne_fin <- caracteristiques_tables[numero_table, ] %>%
        dplyr::pull(index_ligne_fin)

      nom_table <- caracteristiques_tables[numero_table, ] %>%
        dplyr::pull(nom_table)

      table <-
        lignes_dump[index_ligne_debut:index_ligne_fin] # sélection des lignes de la table

      # vecteur contenant les noms des colonnes
      noms_col <- lignes_dump[index_ligne_debut] %>%
        stringr::str_extract_all(pattern = "\\([^()]+\\)") %>% # extraction de ce qui est entre les parenthèses
        stringr::str_sub(start = 2, end = nchar(.) - 1) %>%  # suppression des parenthèses
        stringr::str_split(", ") %>% # scission
        unlist()

      # séparation des colonnes pour chaque ligne (séparateur tabulation "\t")
      table <- strsplit(table[2:length(table)], "\t", fixed = TRUE)

      # empilage, passage en dataframe et remplacement des valeurs manquantes "N" par des NA
      table <- do.call(rbind, table) %>%
        as.data.frame() %>%
        replace(., . == "\\N", NA) %>% # gestion des valeurs manquantes
        dplyr::mutate_all(type.convert) # permet de convertir chaque colonne en son format "natif" car jusque-là tt est en texte

      # renommage des colonnes
      # Il y a qq cas où il y a des colonnes avec des noms dans la requête mais sans données
      # donc qui sont omises du dump => pas possible de simplement faire names(table) <- noms_col
      names(table) <- noms_col[1:ncol(table)]

      # nommage de la table + export dans l'environnement global
      assign(x = nom_table,
             value = table,
             envir = globalenv())

    }

  # extraction des tables avec progress bar

  cli::cli_progress_step("R\u00e9cup\u00e9ration des lignes de fin pour chaque table...")

  pb <-
    progress::progress_bar$new(total = nrow(caracteristiques_tables), force = TRUE)

    purrr::map(
      .x = 1:nrow(caracteristiques_tables),
      .f = extraire_table,
      lignes_dump = lignes_dump,
      caracteristiques_tables = caracteristiques_tables
      )

  # suppression des objets non utiles pour ne conserver que les dataframes
  mes_df <-
    ls()[sapply(ls(), function(i)
      class(get(i))) == "data.frame"]

  rm(list = ls()[!ls() %in% mes_df])

}
