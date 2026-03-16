# Fonction pour importer un dump de la base OISON

Fonction pour importer un dump de la base OISON

## Usage

``` r
imp_importer_dump_oison(fichier_dump)
```

## Arguments

- fichier_dump:

  chemin vers l'objet .sql du dump

## Value

L'ensemble des tables de la base (sauf tables 'mapping')

## Examples

``` r
if (FALSE) { # \dontrun{
imp_importer_dump_oison(fichier_dump = "./raw_data/PROD_oison.sql")
} # }
```
