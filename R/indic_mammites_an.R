#' Calcul des indicateurs mammites globaux entre 2 dates
#'
#' @param ctrl_indiv Tableau de données contrôle laitier avec une ligne par vache-date de contrôle
#' @param date_debut Date de début de la période à prendre en compte (Format aaaa-mm-jj)
#' @param date_fin Date de fin de la période à prendre en compte (Format aaaa-mm-jj)
#'
#' @return
#' @export
#'
#' @examples
indic_mam_periode <- function(ctrl_indiv = data.frame(),
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
  moy_an <- data.frame(
    periode = paste0(date_debut," au " , date_fin),
    n_vach = length(unique(ctrl_indiv$num_vach)),
    n_primi = length(unique(ctrl_indiv$num_vach[ctrl_indiv$rang_lac == 1])),
    n_ctl1 = length(unique(ctrl_indiv$num_vach[ctrl_indiv$j_lac >= 31])),
    n_primi_ctl1 = length(unique(ctrl_indiv$num_vach[ctrl_indiv$rang_lac == 1 & ctrl_indiv$j_lac >= 31])),
    ccs_trp = round(sum(ctrl_indiv$ccs * ctrl_indiv$lait) / sum(ctrl_indiv$lait), 0),
    prev_ccs_bas = round(length(ctrl_indiv$ccs[!is.na(ctrl_indiv$ccs) & ctrl_indiv$ccs < 300]) /
                           length(ctrl_indiv$ccs[!is.na(ctrl_indiv$ccs)]), 3),
    prev_ccs_bas_p1 = round(length(ctrl_indiv$ccs[ctrl_indiv$rang_lac == 1 & !is.na(ctrl_indiv$ccs) & ctrl_indiv$ccs < 300]) /
                              length(ctrl_indiv$ccs[ctrl_indiv$rang_lac == 1 & !is.na(ctrl_indiv$ccs)]), 3),
    incid_lac = round(length(ctrl_indiv$ccs[ctrl_indiv$j_lac < 31 & !is.na(ctrl_indiv$ccs_prev) & ctrl_indiv$ccs_prev < 300 & !is.na(ctrl_indiv$ccs) & ctrl_indiv$ccs >= 300]) /
                        length(ctrl_indiv$ccs[ctrl_indiv$j_lac < 31 & !is.na(ctrl_indiv$ccs_prev) & ctrl_indiv$ccs_prev < 300 & !is.na(ctrl_indiv$ccs)]), 3),
    persist_lac = round(length(ctrl_indiv$ccs[ctrl_indiv$j_lac < 31 & !is.na(ctrl_indiv$ccs_prev) & ctrl_indiv$ccs_prev >= 300 & !is.na(ctrl_indiv$ccs) & ctrl_indiv$ccs >= 300]) /
                          length(ctrl_indiv$ccs[ctrl_indiv$j_lac < 31 & !is.na(ctrl_indiv$ccs_prev) & ctrl_indiv$ccs_prev >= 300 & !is.na(ctrl_indiv$ccs)]), 3),
    incid_taris = round(length(ctrl_indiv$ccs[ctrl_indiv$j_lac >= 31 & !is.na(ctrl_indiv$ccs_prev) & ctrl_indiv$ccs_prev < 300 & !is.na(ctrl_indiv$ccs) & ctrl_indiv$ccs >= 300]) /
                          length(ctrl_indiv$ccs[ctrl_indiv$j_lac >= 31 & !is.na(ctrl_indiv$ccs_prev) & ctrl_indiv$ccs_prev < 300 & !is.na(ctrl_indiv$ccs)]), 3),
    gueri_taris = round(length(ctrl_indiv$ccs[ctrl_indiv$j_lac < 31 & !is.na(ctrl_indiv$ccs_prev) & ctrl_indiv$ccs_prev >= 300 & !is.na(ctrl_indiv$ccs) & ctrl_indiv$ccs < 300]) /
                          length(ctrl_indiv$ccs[ctrl_indiv$j_lac < 31 & !is.na(ctrl_indiv$ccs_prev) & ctrl_indiv$ccs_prev >= 300 & !is.na(ctrl_indiv$ccs)]), 3),
    prev_primi_ctl1 = round(length(ctrl_indiv$ccs[ctrl_indiv$rang_lac ==1 & ctrl_indiv$j_lac >= 31 & ctrl_indiv$ccs > 300]) /
                              length(ctrl_indiv$ccs[ctrl_indiv$rang_lac ==1 & ctrl_indiv$j_lac >= 31]), 3))

  moy_an

}
