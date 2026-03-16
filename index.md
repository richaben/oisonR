# oisonR

[OISON](https://oison.ofb.fr/) est un outil de saisies naturalistes pour
les agents de l’[Office français de la biodiversité
(OFB)](https://www.ofb.gouv.fr/). Il permet de collecter des données
relatives aux espèces et aux milieux, qui sont majoritairement issues
d’observations opportunistes, fortuites ou protocolées. L’accès à
l’outil est restreint et nécessite un compte utilisateur (*création de
compte nécessaire*).

Ce package a été développé dans le but de faciliter l’accès aux données
bancarisées dans [OISON](https://oison.ofb.fr/), et ainsi permettre leur
ré-utilisation dans le cadre de projets de valorisation (*téléchargement
des données et mise en forme dans des formats exploitables*).

### 📦 Installation et chargement

La version de développement sur [GitHub](https://github.com/) (*le
package* `devtools` *est à installer si nécessaire*).

``` r
# install.packages("devtools")
devtools::install_github("richaben/oisonR", build_vignettes = TRUE)
```

Pour charger le package `oisonR` :

``` r
library(oisonR)
```

### 📚 Vignettes / Documentation

Quelques vignettes sont disponibles pour l’utilisation du package :

- [Bien démarrer avec le package
  `oisonR`](https://richaben.github.io/oisonR/articles/bien-demarrer-avec-oisonR.html)
- [Comment utiliser le package `oisonR`
  ?](https://richaben.github.io/oisonR/articles/comment-utiliser-oisonR.html)

### ✉️ Issues / Bugs

Pour signaler un bug, une erreur ou une suggestion, merci d’utiliser les
issues sur le [dépôt GitHub](https://github.com/richaben/oisonR/issues)
du package.
