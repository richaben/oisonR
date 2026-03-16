# Récupération des infos de répartition d'un taxon à l'échelle départementale selon l'INPN

La fonction permet de récupérer rapidement les informations, quand elles
existent, sur la répartition d'un taxon à l'échelle des départements sur
le site de l'INPN. Ces informations sont utilisées pour la validation
des données du SINP. Plusieurs statuts de présence/absence sont
possibles (voir INPN pour plus d'infos). Exemple de carte de répartition
pour la Salamandre tachetée : [sur sa fiche
ici](https://inpn.mnhn.fr/espece/cd_nom/965096/tab/carte).

## Usage

``` r
add_inpn_repartition_taxon(cd_nom, code_dpt)
```

## Arguments

- cd_nom:

  le code cd_nom vérifié du taxon

- code_dpt:

  le code insee du département

## Value

un dataframe avec les informations de répartition pour le taxon

## Examples

``` r
if (FALSE) { # \dontrun{
## Exemple pour la Salamandre tachetée sur le département du Calvados (14)
oisonR::add_inpn_repartition_taxon(cd_nom = 965096, code_dpt = 14)

## Pour une liste de taxons et de départements
### 3 taxons
taxons <- c(965096, # Salamandre tachetée
           310, # Grenouille agile
           77949 # Couleuvre verte et jaune
           )
# 5 départements
departements <- c(14, 27, 50, 61, 76)

tidyr::expand_grid(taxons, departements) %>%
  purrr::pmap_dfr(., ~add_inpn_repartition_taxon(cd_nom = .x, code_dpt = .y))

} # }
```
