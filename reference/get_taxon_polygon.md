# Consulter et télécharger les données OISON sur une zone définie (type polygone)

Consulter et télécharger les données OISON sur une zone définie (type
polygone)

## Usage

``` r
get_taxon_polygon(
  zone_selection,
  login,
  mdp,
  collect_all = FALSE,
  draw_zone = FALSE,
  date_min = NULL,
  date_max = NULL
)
```

## Arguments

- zone_selection:

  un objet contenant le zone (polygon), au CRS RGF93 (epsg: 2154)

- login:

  login du compte OISON

- mdp:

  mot de passe associé au compte OISON

- collect_all:

  Booléen. Si FALSE (par défaut) consultation du nombre d'observation
  (FALSE). Si TRUE télécharge les données.

- draw_zone:

  Booléen. Si TRUE, lance l'outil de sélection de zones. Par défaut,
  FALSE.

- date_min:

  date minimale pour la période au format (`%Y-%m-%d` ; cf. exemple
  '2022-01-01'). NULL par défaut.

- date_max:

  date maximum pour la période au format (`%Y-%m-%d` ; cf. exemple
  '2022-12-31'). NULL par défaut.

## Value

Si `collect_all`=FALSE, retourne uniquement un dataframe avec le nombre
d'observations associé sur la zone géographique sélectionnée. Si
`collect_all`=TRUE, retourne un dataframe avec les observations pour les
taxons. Si `date_min` et `date_max` sont spécifiés, retourne un
dataframe avec les valeurs sur la période.

## Examples

``` r
if (FALSE) { # \dontrun{
rm(selection_zone)

## Exemple 1) Collecter des données sur un polygone défini manuellement:
test_zone <-
  "POLYGON ((410677.9 6877209, 413275.2 6929083, 496961.8 6925506,
495122.6 6873600, 410677.9 6877209))"

get_taxon_polygon(zone_selection = test_zone,
                  login = "john.doe@ofb.gouv.fr",
                  mdp = "mon_mdp",
                  collect_all = F,
                  draw_zone = F)

## Exemple 2) Consulter des données sur un polygone tracé via l'outil de traçage:
get_taxon_polygon(login = "john.doe@ofb.gouv.fr",
                  mdp = "mon_mdp",
                  collect_all = F,
                  draw_zone = T)

## Exemple 3) Collecter des données sur un polygone tracé en même temps avec l'outil de traçage:
get_taxon_polygon(login = "john.doe@ofb.gouv.fr",
                  mdp = "mon_mdp",
                  collect_all = T,
                  draw_zone = T)

## Exemple 4) Consulter des données sur un polygone déjà sauvegardé dans l'environnement:
get_taxon_polygon(login = "john.doe@ofb.gouv.fr",
                  mdp = "mon_mdp",
                  collect_all = F,
                  draw_zone = F)

## Exemple 5) Consulter des données sur un polygone connu et
## défini manuellement sur une période précise:

test_zone <-
  "POLYGON ((410677.9 6877209, 413275.2 6929083, 496961.8 6925506,
495122.6 6873600, 410677.9 6877209))"

get_taxon_polygon(zone_selection = test_zone,
                  login = "john.doe@ofb.gouv.fr",
                  mdp = "mon_mdp",
                  collect_all = F,
                  draw_zone = T,
                  date_min = '2022-01-01',
                  date_max = '2022-12-31')
} # }
```
