# Démarre une connexion à OISON via la base de données SQL

Fonction qui utilise les paramètres de connexion à la base stockés dans
l'environnement R (fichier \`.Renviron\`), et lance la connexion à la
base.

## Usage

``` r
start_sql_connexion()
```

## Value

Aucune valeur. Une erreur est renvoyée si la connexion est impossible.

## Examples

``` r
if (FALSE) { # \dontrun{
bdd_oison <- start_sql_connexion()
} # }
```
