% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-connexion.R
\name{check_acces_oison_sql}
\alias{check_acces_oison_sql}
\title{Test si les paramètres à la BDD OISON en SQL sont bien renseignés}
\usage{
check_acces_oison_sql()
}
\value{
Aucune valeur. Une erreur est renvoyée si les paramètres ne sont pas renseignés.
}
\description{
Fonction pour tester si les paramètres pour l'accès à la BDD OISON en SQL
sont bien stockés dans l'environnement R (fichier `.Renviron`).
}
\examples{
\dontrun{
check_acces_oison_sql()
}
}
