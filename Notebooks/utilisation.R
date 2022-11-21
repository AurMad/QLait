## installer le package
# remotes::install_github()

## chargement du package
library(QLait)

## import de fichiers Excel
# préciser l'emplacement du dossier dans lequel sont les fichiers
# ne pas mettre d'autres fichiers Excel dans ce dossier
path <- "C:/Users/../ccs_excel"

## utiliser la fonction import_cl_excel()
ctl <- import_cl_excel(path = path, ext = "xlsx")

## calcul d'indicateurs mensuels
ind_mois <- indic_mam_mois(ctl)

## calcul d'indicateurs mensuels avec bornes temporelles
ind_mois <- indic_mam_mois(ctl, date_debut = "2021-01-01", date_fin = "2022-01-01")


## calcul d'indicateurs annuels
ind_an <- indic_mam_periode(ctl)

