% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-connexion.R
\name{start_sql_connexion}
\alias{start_sql_connexion}
\title{Démarre une connexion à OISON via la base de données SQL}
\usage{
start_sql_connexion()
}
\value{
Aucune valeur. Une erreur est renvoyée si la connexion est impossible.
}
\description{
Fonction qui utilise les paramètres de connexion à la base stockés dans
l'environnement R (fichier `.Renviron`), et lance la connexion à la base.
}
\examples{
\dontrun{
bdd_oison <- start_sql_connexion()
}

}
