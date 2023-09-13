## Liste des fichiers à concaténer
## A partir du dossier source
## et de l'extension des fichiers
list_pdf_files <- function(path = getwd(), ext = "pdf"){

  if(ext != "pdf") stop("Valeur inattendue pour l'argument ext")
  ## liste des fichiers Excel
  ls_files <- list.files(path)[grep(paste0("\\.", ext), list.files(path))]
  ## fichiers temporaires supprimés de la liste
  tmp_files <- grep("\\$", ls_files)
  if(length(tmp_files) > 0) ls_files <- ls_files[-grep("\\$", ls_files)]

  ## retourne une liste avec le chemin du dossier et la liste des fichiers
  list_files <- list(
    path = paste0(path, "/"),
    files = ls_files
  )

  return(list_files)

}


#' Import de tous les fichiers Excel d'un dossier et agrégation dans un seul tableau
#'
#' @param path Emplacement du dossier
#'
#' @return
#' @export
#'
#' @examples
import_cl_pdf <- function(path = getwd(), donnees_manquantes = FALSE){

  ext <- "pdf"
  ## chargement de la liste des fichiers
  list_files <- list_pdf_files(path, ext)
  ls_files <- paste0(list_files$path, list_files$files)

  message(paste(length(list_files$files)), " fichiers chargés.\n")
  message(paste0(list_files$files, "\n"))

  ## jeu de données final
  cl <- data.frame()

  j <- 1
  print(ls_files[j])

  ## boucle qui parcourt tous les fichiers pdf
  for(j in 1:length(ls_files)){

  ## chargement du fichier
  tbv_o <- pdftools::pdf_text(ls_files[j]) |>
    readr::read_lines()

  for(i in 1:20){
    this_row <- tbv_o[i]
    if(isTRUE(grep("Nom", this_row) == 1)){

      pos_start_end <- stringr::str_locate_all(this_row, "[:space:]{2,}")[[1]]
      break()

    }
  }

  ## identification des lignes qui contiennent des données
  ## hypothèse que chaque ligne avec des données contient un nombre minimum de caractères
  # nombre de caractères : lg_row
  ## hypothèse que chaque ligne de données contient un certain nombre de chiffres
  # nombre de chiffres : n_digits
  lg_row  <- rep(0, length(tbv_o))
  n_digits <- rep(0, length(tbv_o))

  for(i in 1:length(tbv_o)){

    lg_row[i]   <- nchar(tbv_o[i])
    n_digits[i] <- stringr::str_count(tbv_o[i], "[:digit:]")

  }

  # barplot(table(lg_row))
  # barplot(table(n_digits))
  # plot(x = n_digits, y = lg_row)
  # cbind(lg_row, n_digits)

  ## sélection d'un tableau avec des données vaches probables
  tbv_1 <- tbv_o[which(lg_row >= 100 & n_digits > 8)]

  ## position pour séparer les différentes colonnes dans le tableau de données
  pos <- as.integer(apply(pos_start_end, 1, mean))

  pos_i <- c(1, pos)
  pos_f <- c(pos, max(lg_row))

  ## vecteur de noms de colonnes initial
  c_names <- stringr::str_sub_all(this_row, start = pos_i, end = pos_f)[[1]]
  c_names <- stringr::str_replace_all(c_names, "[:space:]", "")

  ## reconstitution du tableau de données
  tbv_2 <- vector("list", length(tbv_1))

  tbv <- data.frame()

  for(i in 1:length(tbv_1)){

    tbv_2[[i]] <- unlist(stringr::str_sub_all(tbv_1[i], start = pos_i, end = pos_f))
    tbv <- rbind(tbv, tbv_2[[i]])

  }
  ## suppression des espaces superflus
  for(i in 1:ncol(tbv)){

    tbv[,i] <- stringr::str_replace_all(tbv[, i], "[:space:]", "")

  }

  tbv <- tbv[tbv[, 1] != "", ]

  ## colonnes renommées
  colnames(tbv) <- c_names

  cl_j <- tbv[, c(1, 2, 14, 4, 15, 6, 9)]
  colnames(cl_j) <- c("num_vach", "nom_vach", "date_vel", "rang_lac",
                    "j_lac", "lait", "ccs")

  cl_j$num_vach <- as.integer(cl_j$num_vach)
  cl_j$date_vel <- as.Date(cl_j$date_vel, "%d/%m/%y")
  cl_j$rang_lac <- as.integer(cl_j$rang_lac)
  cl_j$j_lac <- as.integer(cl_j$j_lac)
  cl_j$lait <- as.numeric(stringr::str_replace_all(cl_j$lait, ",", "\\."))
  cl_j$ccs <- as.integer(cl_j$ccs)
  cl_j$date_ctrl <- rep(NA)
  cette_date <- cl_j[!is.na(cl_j$lait), ][1, c("date_vel", "j_lac")]

  cette_date <- cette_date[,1] + cette_date[,2]
  cl_j$date_ctrl <- cette_date

  cl <- rbind(cl, cl_j)

  }

  cl <- cl[!is.na(cl$lait) & cl$lait > 0, ]
  cl <- cl |>
    dplyr::arrange(cl$date_ctrl, cl$num_vach)

  return(cl)
}

