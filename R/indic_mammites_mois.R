#' Calcul des indicateurs mammites mensuels entre 2 dates
#'
#' @param ctrl_indiv Tableau de données contrôle laitier avec une ligne par vache-date de contrôle
#' @param date_debut Date de début de la période à prendre en compte (Format aaaa-mm-jj)
#' @param date_fin Date de fin de la période à prendre en compte (Format aaaa-mm-jj)
#'
#' @return
#' @export
#'
#' @examples
indic_mam_mois <- function(ctrl_indiv = data.frame(),
                           date_debut = NULL,
                           date_fin = NULL){


  ## vérification des nomes de colonnes
  nom_col <- c("date_ctrl", "num_vach", "nom_vach", "date_vel", "rang_lac",
               "j_lac", "lait", "ccs")

  col_chk <- match(nom_col, colnames(ctrl_indiv))
  if(length(which(is.na(col_chk))) > 0){

    paste("Noms de colonnes attendus :", nom_col)
    stop()

  }

  if(is.null(date_debut)) date_debut <- min(ctrl_indiv$date_ctrl)
  if(is.null(date_fin)) date_fin <- max(ctrl_indiv$date_ctrl)

  ## Ajout des données du contrôle précédent
  ctrl_indiv <- ctrl_indiv |>
    dplyr::arrange(num_vach, as.Date(date_ctrl)) |>
    dplyr::group_by(num_vach) |>
    dplyr::mutate(ccs_prev = dplyr::lag(ccs))|>
    dplyr::filter(date_ctrl >= as.Date(date_debut) &
                  date_ctrl <= as.Date(date_fin))

  ## On considère comme premiers contrôles ceux survenant avant 31 jours de lactation
  by_date <- ctrl_indiv |>
    dplyr::group_by(date_ctrl) |>
    dplyr::summarise(
    n_vach = length(unique(num_vach)),
    n_primi = length(unique(num_vach[rang_lac == 1])),
    n_ctl1 = length(unique(num_vach[j_lac < 31])),
    n_primi_ctl1 = length(unique(num_vach[rang_lac == 1 & j_lac < 31])),
    ccs_trp = round(sum(ccs[!is.na(ccs)] * lait[!is.na(ccs)]) / sum(lait[!is.na(ccs)]), 0),
    prev_ccs_bas = round(length(ccs[!is.na(ccs) & ccs < 300]) /
                           length(ccs[!is.na(ccs)]), 3),
    prev_ccs_bas_p1 = round(length(ccs[rang_lac == 1 & !is.na(ccs) & ccs < 300]) /
                              length(ccs[rang_lac == 1 & !is.na(ccs)]), 3),
    incid_lac = round(length(ccs[j_lac >= 31 & !is.na(ccs_prev) & ccs_prev < 300 & !is.na(ccs) & ccs >= 300]) /
                        length(ccs[j_lac >= 31 & !is.na(ccs_prev) & ccs_prev < 300 & !is.na(ccs)]), 3),
    persist_lac = round(length(ccs[j_lac >= 31 & !is.na(ccs_prev) & ccs_prev >= 300 & !is.na(ccs) & ccs >= 300]) /
                          length(ccs[j_lac >= 31 & !is.na(ccs_prev) & ccs_prev >= 300 & !is.na(ccs)]), 3),
    incid_taris = round(length(ccs[j_lac < 31 & !is.na(ccs_prev) & ccs_prev < 300 & !is.na(ccs) & ccs >= 300]) /
                          length(ccs[j_lac < 31 & !is.na(ccs_prev) & ccs_prev < 300 & !is.na(ccs)]), 3),
    gueri_taris = round(length(ccs[j_lac < 31 & !is.na(ccs_prev) & ccs_prev >= 300 & !is.na(ccs) & ccs < 300]) /
                          length(ccs[j_lac < 31 & !is.na(ccs_prev) & ccs_prev >= 300 & !is.na(ccs)]), 3),
    prev_primi_ctl1 = round(length(ccs[rang_lac ==1 & j_lac < 31 & ccs > 300]) /
                              length(ccs[rang_lac ==1 & j_lac < 31]), 3)
  )

  by_date

}
