# Ajoute une colonne contenant le type de geometrie pour les observations

Ajoute une colonne contenant le type de geometrie pour les observations

## Usage

``` r
add_type_geometrie(oison_df)
```

## Arguments

- oison_df:

  un dataframe de classe sf

## Value

un dataframe avec une colonne \`type_geometrie\`

## Examples

``` r
if (FALSE) { # \dontrun{
oison_taxon <-
  oison_taxon %>%
  sf::st_as_sf() %>%
  add_type_geometrie()
} # }
```
