# Stoppe une connexion à OISON via la base de données SQL

Fonction stoppe la connexion à la base.

## Usage

``` r
stop_sql_connexion(conn)
```

## Arguments

- conn:

  nom de la base à stopper.

## Value

Aucune valeur.

## Examples

``` r
if (FALSE) { # \dontrun{
bdd_oison <- start_sql_connexion()

stop_sql_connexion(conn = bdd_oison)
} # }
```
