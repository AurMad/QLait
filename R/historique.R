historique_vach <- function(cl){

  cl_wide <- cl |>
    dplyr::select(num_vach, nom_vach, date_ctrl, ccs) |>
    tidyr::pivot_wider(
      names_from = date_ctrl,
      values_from = ccs
    ) |>
    dplyr::arrange(num_vach)

  return(cl_wide)

}
