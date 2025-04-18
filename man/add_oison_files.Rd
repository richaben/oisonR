% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_oison_files.R
\name{add_oison_files}
\alias{add_oison_files}
\title{Fonction pour ajouter les fichiers images/sons à une table OISON existante}
\usage{
add_oison_files(oison_table, conn, ...)
}
\arguments{
\item{oison_table}{nom de la table OISON à enrichir}

\item{conn}{nom de la base à utiliser}

\item{...}{arguments supplémentaires}
}
\value{
un dataframe avec les fichiers images et sons associés à chaque observation
}
\description{
Permet d'ajouter à une 'table OISON' (taxons ou milieux) existantes les fichiers
images et sons présents dans la base de données.
La table OISON doit contenir la colonne \code{observation_id} pour permettre la jointure.
}
\examples{
\dontrun{
bdd_oison <- start_sql_connexion()

df_taxon_oison_sql <- get_table_taxon_sql(conn = bd_oison)

add_oison_files(df_taxon_oison_sql, bdd_oison)
}
}
