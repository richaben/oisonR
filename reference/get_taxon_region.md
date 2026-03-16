# Consulter et télécharger les données sur les taxons de la bdd OISON pour une région administrative

Consulter et télécharger les données sur les taxons de la bdd OISON pour
une région administrative

## Usage

``` r
get_taxon_region(
  region_code,
  login,
  mdp,
  collect_all = FALSE,
  date_min = NULL,
  date_max = NULL
)
```

## Arguments

- region_code:

  Code INSEE de la région (ex. 28 pour la Normandie, ...)

- login:

  login du compte OISON

- mdp:

  mot de passe associé au compte OISON

- collect_all:

  Booléen. Si FALSE (par défaut) consultation du nombre d'observation.
  Si TRUE, télécharge les données pour les taxons.

- date_min:

  date minimale pour la période au format (`%Y-%m-%d` ; cf. exemple
  '2022-01-01'). NULL par défaut.

- date_max:

  date maximum pour la période au format (`%Y-%m-%d` ; cf. exemple
  '2022-12-31'). NULL par défaut.

## Value

Si `collect_all`=FALSE, retourne un dataframe indiquant le nombre
d'observations sur la région choisie. Si `collect_all`=TRUE, retourne un
dataframe avec le détail des observations taxons. Si `date_min` et
`date_max` sont spécifiés, retourne un dataframe avec les valeurs sur la
période.

## Examples

``` r
if (FALSE) { # \dontrun{
# -> Exemple pour consulter rapidement le nombre de données dans la région
# Ile-de-France (code région = 11)

get_taxon_region(region_code = "11",
                 login = "john.doe@ofb.gouv.fr",
                 mdp = "mon_mdp")

# -> Exemple pour télécharger les données dans
# la région Île-de-France (code région 11)

obs_regIDF <-
  get_taxon_region(region_code = "11",
                   login = "john.doe@ofb.gouv.fr",
                   mdp = "mon_mdp",
                   collect_all = T)

# -> Exemple pour télécharger les données dans la région
# Île-de-France (code région 11) sur la période 2022

obs_regIDF_2022 <-
  get_taxon_region(login = "john.doe@ofb.gouv.fr",
                   mdp = "mon_mdp",
                   region_code = "11",
                   date_min = '2022-01-01',
                   date_max = '2022-12-31',
                   collect_all = T)
} # }
```
