# Fonction pour créer le tableau milieu OISON via connexion SQL

Permet de créer une 'table milieu' à partir de la base SQL. Par défaut,
la table créée correspond à l'ensemble des observations sur les milieux
bancarisés dans OISON. Si un argument `geometrie` est fourni, la table
créée correspond aux observations sur les milieux sur la zone
géographique spécifiée par la géométrie.

## Usage

``` r
get_table_milieu_sql(conn, geometrie, ...)
```

## Arguments

- conn:

  nom de la base à utiliser.

- geometrie:

  chaîne de caractères. Elle est utilisée pour la requête SQL pour
  filtrer les données. La géométrie doit être sous la forme d'une chaîne
  de caractères (ex. "POLYGON((x1 y1, x2 y2, x3 y3, x4 y4, x1 y1))". Sa
  projection doit être en RGF93 / Lambert-93 (EPSG=2154).

- ...:

  arguments supplémentaires

## Value

un dataframe avec les observations milieu

## Examples

``` r
if (FALSE) { # \dontrun{
### ------------- ###
## Exemple 1. Export sans géométrie
bdd_oison <- start_sql_connexion()

df_milieu_oison_sql <- get_table_milieu_sql(conn = bdd_oison)

stop_sql_connexion(conn = bdd_oison)

### ------------- ###
## Exemple 2. Export avec géométrie spécifiée
# a) créer une géométrie
# devtools::install_github("MaelTheuliere/COGiter")

geom_normandie <-
  COGiter::regions_geo %>%
  dplyr::filter(REG == 28) %>%
  sf::st_as_sfc() %>%
  # convert to text
  sf::st_as_text()

# b) faire la requête
bdd_oison <- start_sql_connexion()

df_taxon_milieu_sql_normandie <-
get_table_milieu_sql(conn = bdd_oison, geometrie = geom_normandie)

stop_sql_connexion(conn = bdd_oison)
} # }
```
