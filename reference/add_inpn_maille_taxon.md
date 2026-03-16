# Récupération des mailles 10x10km sur l'INPN sur une zone shape

Récupération des mailles 10x10km sur l'INPN sur une zone shape

## Usage

``` r
add_inpn_maille_taxon(df, shape)
```

## Arguments

- df:

  un dataframe issu de la requete OISON

- shape:

  un dataframe contenant une couche shape pour la zone souhaitée

## Value

un dataframe avec les mailles 10x10km INPN

## Examples

``` r
if (FALSE) { # \dontrun{

data_oison <- get_data_dpt(dpt_code = "27",
login = "john.doe@ofb.gouv.fr", mdp = "mon_mdp")

normandie_shp <-
readRDS(file = './regions_geo_normandie.rds') %>%
sf::st_transform(crs = 4326)

maille_inpn <- add_inpn_maille_taxon(df = data_oison,  shape = normandie_shp)

} # }
```
