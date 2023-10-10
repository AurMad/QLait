path <- "C:/Users/amadouasse/Documents/enseignement/vet5/med_pop/2023_2024/2023_09_Groupe1/GAEC_2_ponts"

test <- import_cl_pdf(path = path)

tab_hist <- historique_vach(test)
write.csv2(tab_hist, "historique_cellules.csv")

tab_mois <- indic_mam_mois(test)
write.csv2(tab_mois, "indic_mois.csv")
