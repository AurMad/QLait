## Liste des fichiers à concaténer
## A partir du dossier source
## et de l'extension des fichiers
list_xl_files <- function(path = getwd(), ext = "xlsx"){

  if(!ext %in% c("xls", "xlsx")) stop("Valeur inattendue pour l'argument ext")
  ## liste des fichiers Excel
  ls_files <- list.files(path)[grep(ext, list.files(path))]
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


## Test des noms de colonnes des fichiers Excel
## file Nom du fichier à tester
## sheet Feuille
test_header_xl <- function(file, sheet = 1){

  row_header <- 0
  ## contenu de la première ligne du fichier Excel
  row1 <- suppressMessages(as.vector(readxl::read_excel(file, sheet = sheet, col_names = FALSE, n_max = 1)))
  ## noms de colonnes recherchés
  hr_names <- c("N°", "Nom", "Vêlage", "N°\r\nLac.", "Durée", "Lait kg", "Leuco mil./ ml")
  ## identification des noms de colonnes
  col_match <- match(hr_names, row1)

  ## Boucle jusqu'à ce que les noms de colonnes recherchés soient touvés. Stop à 5 lignes
  while(!(length(col_match) == 7 & length(which(is.na(col_match))) == 0) & row_header < 4){

    row_header <- row_header + 1
    row1 <- suppressMessages(as.character(as.vector(readxl::read_excel(file, sheet = sheet, col_names = FALSE, n_max = 1, skip = row_header))))
    col_match <- match(hr_names, row1)

  }

  if(row_header < 4){

    xl_header <- list(
      header_names = row1,
      row_header_names = row_header

    )


  } else {

    stop("Colonnes recherchées nom trouvées dans le fichier")

  }

  xl_header

}

## Vérification des noms de colonnes
check_names_cl <- function(header_names){

  ## Noms de colonnes attendus
  hr_names <- c("N°", "Nom", "Vêlage", "N°\r\nLac.", "Durée", "Lait kg", "Leuco mil./ ml")

  ## Vérification de la présence des colonnes recherchées
  name_chk <- match(hr_names, header_names)

  if(length(which(is.na(name_chk))) > 0){

  print("Les noms de colonnes suivants sont attendus et manquants :")
  print(hr_names[which(is.na(name_chk))])
  stop()

  }

  col_order <- match(hr_names, header_names)
 }

#' Import de tous les fichiers Excel d'un dossier et agrégation dans un seul tableau
#'
#' @param path Emplacement du dossier
#' @param ext Extension des fichiers (xls ou xlsx)
#'
#' @return
#' @export
#'
#' @examples
import_cl_excel <- function(path = getwd(), ext = "xlsx", donnees_manquantes = FALSE){

  ## chargement de la liste des fichiers
  list_files <- list_xl_files(path, ext)
  ls_files <- paste0(list_files$path, list_files$files)

  message(paste(length(list_files$files)), " fichiers chargés.\n")
  message(paste0(list_files$files, "\n"))

  hr_names <- c("N°", "Nom", "Vêlage", "N°\r\nLac.", "Durée", "Lait kg", "Leuco mil./ ml")

  ## création du tableau final
  cl <- data.frame(
    num_vach = integer(),
    nom_vach = character(),
    date_vel = character(),
    rang_lac = integer(),
    j_lac = integer(),
    lait = double(),
    ccs = double(),
    date_ctrl = character()
  )

  cl$date_vel  <- as.Date(cl$date_vel)
  cl$date_ctrl <- as.Date(cl$date_ctrl)

  ## création d'un tableau pour les données manquantes
  cl_mis <- cl
  cl_mis$typ_erreur <- character()

  ## fichiers chargés un par un
  for(i in 1:length(ls_files)){

  ## Nombre de feuilles
  n_sheets <- length(readxl::excel_sheets(ls_files[i]))

  for(j in 1:n_sheets){

    ## identification de la colonne qui contient les noms de colonnes
    xl_hd <- test_header_xl(ls_files[i], sheet = j)

    ## vérification des noms de colonnes
    col_order <- check_names_cl(xl_hd$header_names)

    ## chargement des données
    ## chargement du premier fichier
    mois_i <- readxl::read_excel(ls_files[i], sheet = j, skip = xl_hd$row_header_names)
    mois_i <- mois_i[, col_order]

    ## changement des noms de colonnes
    colnames(mois_i) <- colnames(cl)

    ## changements des types des colonnes
    mois_i$num_vach  <- as.integer(mois_i$num_vach)
    mois_i$nom_vach  <- as.character(mois_i$nom_vach)
    mois_i$date_vel  <- as.Date(mois_i$date_vel)
    mois_i$rang_lac  <- as.integer(mois_i$rang_lac)
    mois_i$j_lac     <- as.integer(mois_i$j_lac)
    mois_i$lait      <- as.double(mois_i$lait)
    mois_i$ccs       <- as.double(mois_i$ccs)
    mois_i$date_ctrl <- as.Date("1900-01-01")

    ## supression des lignes avec des données manquantes
    # dates de vêlages manquantes
    vel_mis <- which(is.na(mois_i$date_vel))

    if(length(vel_mis) > 0){

      mis <- mois_i[vel_mis, ]
      mois_i <- mois_i[-vel_mis, ]
      mis$typ_erreur <- "Date vêlage"

      cl_mis <- rbind(cl_mis, mis)
      rm(list = c("mis", "vel_mis"))

    }

    # quantités de lait ou cellules manquantes
    lait_ccs_mis <- which(is.na(mois_i$lait) | is.na(mois_i$ccs))

    if(length(lait_ccs_mis) > 0){

      mis <- mois_i[lait_ccs_mis, ]
      mois_i <- mois_i[-lait_ccs_mis, ]

      mis$typ_erreur <- "Lait/CCS manquants"

      cl_mis <- rbind(cl_mis, mis)
      rm(list = c("mis", "lait_ccs_mis"))

    }

    ## ajout de la date de contrôle
    mois_i$date_ctrl <- mois_i$date_vel + mois_i$j_lac

    ## la date la plus fréquente est attribuée à toutes les vaches
    tab_dates_ctl <- table(mois_i$date_ctrl)
    mois_i$date_ctrl <- as.Date(names(tab_dates_ctl)[tab_dates_ctl == max(tab_dates_ctl)])

    cl <- rbind(cl, mois_i)
    rm(mois_i)

  }
  }

  cl <- cl[, c("date_ctrl", "num_vach", "nom_vach", "date_vel",
               "rang_lac", "j_lac", "lait", "ccs")]

  cl <- cl[order(cl$date_ctrl, cl$num_vach),]

  if(donnees_manquantes == FALSE){

    return(cl)

    } else {

    cl <- list(
      cl = cl,
      manq = cl_mis
    )

   }

}
