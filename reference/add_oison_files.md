# Fonction pour ajouter les fichiers images/sons à une table OISON existante

Permet d'ajouter à une 'table OISON' (taxons ou milieux) existantes les
fichiers images et sons présents dans la base de données. La table OISON
doit contenir la colonne `observation_id` pour permettre la jointure.

## Usage

``` r
add_oison_files(oison_table, conn, ...)
```

## Arguments

- oison_table:

  nom de la table OISON à enrichir

- conn:

  nom de la base à utiliser

- ...:

  arguments supplémentaires

## Value

un dataframe avec les fichiers images et sons associés à chaque
observation

## Examples

``` r
if (FALSE) { # \dontrun{
bdd_oison <- start_sql_connexion()

df_taxon_oison_sql <- get_table_taxon_sql(conn = bd_oison)

add_oison_files(df_taxon_oison_sql, bdd_oison)
} # }
```
