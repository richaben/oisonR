
<!-- README.md is generated from README.Rmd. Please edit that file -->

# oisonR <a href="https://richaben.github.io/oisonR/"><img src="man/figures/logo.png" align="right" height="139" alt="oisonR website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/richaben/oisonR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/richaben/oisonR/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![pkgdown](https://github.com/richaben/oisonR/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/richaben/oisonR/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

[OISON](https://oison.ofb.fr/) est un outil de saisies naturalistes pour
les agents de l’[Office français de la biodiversité
(OFB)](https://www.ofb.gouv.fr/). Il permet de collecter des données
relatives aux espèces et aux milieux, qui sont majoritairement issues
d’observations opportunistes, fortuites ou protocolées. L’accès à
l’outil est restreint et nécessite un compte utilisateur (*création de
compte nécessaire avant connexion*).

Ce package a été développé pour faciliter l’accès aux données
bancarisées dans OISON, et doit permettre leur ré-utilisation dans le
cadre de projets de valorisation (téléchargement des données à partir de
différentes sources, et leur mise en forme dans des formats
exploitables).

## Installation

La version de développement sur [GitHub](https://github.com/) :

``` r
# install.packages("devtools")
devtools::install_github("richaben/oisonR")
```

## Exemple

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
library(oisonR)
```
