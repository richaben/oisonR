% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_inpn_habitats_taxon.R
\name{add_inpn_habitats_taxon}
\alias{add_inpn_habitats_taxon}
\title{Récupération des Codes Habitats à partir de la fiche taxon présente sur l'INPN}
\usage{
add_inpn_habitats_taxon(df, cdnom_col, nom_vern_col, nom_sci_col)
}
\arguments{
\item{df}{un dataframe issu d'une requête taxon OISON avec à minima les 3 colonnes suivantes :
- le nom vernaculaire
- le nom scientifique
- le code taxon}

\item{cdnom_col}{le nom de la colonne contenant le code taxon utilisé dans l'URL de l'INPN}

\item{nom_vern_col}{le nom de la colonne contenant le(s) nom(s) vernaculaire(s) à conserver}

\item{nom_sci_col}{le nom de la colonne contenant le(s) nom(s) scientifique(s) à conserver}
}
\value{
un dataframe avec les Codes Habitats pour chacun des taxons du tableau initial
}
\description{
A partir d'un dataframe issu de la requête OISON, cette fonction permet de récupérer
les informations relatives aux Habitats présentés sur la fiche taxon de l'INPN.
Par exemple, pour le Rougegorge familier, les infos sont collectés
\href{https://inpn.mnhn.fr/espece/cd_nom/4001/tab/habitats}{sur sa fiche ici}.
}
\examples{
\dontrun{
## Requete sur quelques observations
data_oison <-
  oisonR::get_taxon_dpt(dpt_code = "27",
                        login = "john.doe@ofb.gouv.fr",
                        mdp = "mon_mdp",
                        date_min = "2023-01-01",
                        date_max = "2023-02-01",
                        collect_all = TRUE) \%>\%
  dplyr::select(cd_ref, nom_scientifique, nom_vernaculaire)

## Collecte des informations Codes Habitats
code_habitats <-
  add_inpn_habitats_taxon(df = data_oison,
                          cdnom_col = cd_ref,
                          nom_sci_col = nom_scientifique,
                          nom_vern_col = nom_vernaculaire)

}
}
