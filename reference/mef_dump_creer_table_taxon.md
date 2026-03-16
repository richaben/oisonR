# Fonction pour créer un tableau taxon issu d'un dump OISON

Cette fonction permet de créer un tableau avec les observations `taxon`
présentes dans la base OISON, à partir des tables importées par le dump
de la base.

## Usage

``` r
mef_dump_creer_table_taxon()
```

## Value

un dataframe avec les observations taxons. La colonne `geometry`
spécifie le type de géométrie (point, lignes, polygones) pour
l'observation, et doit être traitée avec précaution lors d'un export
sous un autre format (.csv, ...).

## Details

Nécessite au préalable d'importer le dump sql avec la fonction
`imp_importer_dump_oison`.

## Examples

``` r
if (FALSE) { # \dontrun{
df_taxon_oison <- mef_dump_creer_table_taxon()
} # }
```
