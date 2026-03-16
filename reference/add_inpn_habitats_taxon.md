# Récupération des Codes Habitats à partir de la fiche taxon présente sur l'INPN

A partir d'un dataframe issu de la requête OISON, cette fonction permet
de récupérer les informations relatives aux Habitats présentés sur la
fiche taxon de l'INPN. Par exemple, pour le Rougegorge familier, les
infos sont collectés [sur sa fiche
ici](https://inpn.mnhn.fr/espece/cd_nom/4001/tab/habitats).

## Usage

``` r
add_inpn_habitats_taxon(df, cdnom_col, nom_vern_col, nom_sci_col)
```

## Arguments

- df:

  un dataframe issu d'une requête taxon OISON avec à minima les 3
  colonnes suivantes : - le nom vernaculaire - le nom scientifique - le
  code taxon

- cdnom_col:

  le nom de la colonne contenant le code taxon utilisé dans l'URL de
  l'INPN

- nom_vern_col:

  le nom de la colonne contenant le(s) nom(s) vernaculaire(s) à
  conserver

- nom_sci_col:

  le nom de la colonne contenant le(s) nom(s) scientifique(s) à
  conserver

## Value

un dataframe avec les Codes Habitats pour chacun des taxons du tableau
initial

## Examples

``` r
if (FALSE) { # \dontrun{
## Requete sur quelques observations
data_oison <-
  oisonR::get_taxon_dpt(dpt_code = "27",
                        login = "john.doe@ofb.gouv.fr",
                        mdp = "mon_mdp",
                        date_min = "2023-01-01",
                        date_max = "2023-02-01",
                        collect_all = TRUE) %>%
  dplyr::select(cd_ref, nom_scientifique, nom_vernaculaire)

## Collecte des informations Codes Habitats
code_habitats <-
  add_inpn_habitats_taxon(df = data_oison,
                          cdnom_col = cd_ref,
                          nom_sci_col = nom_scientifique,
                          nom_vern_col = nom_vernaculaire)

} # }
```
