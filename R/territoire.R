#' Permet d'établir un périmètre afin d'en retenir ensuite des objets
#'
#' Cette fonction permet d'extraire sous forme spatiale (sf) un territoire à une échelle donnée afin de le réutiliser pour des croisements spatiaux
#' @name territoire
#' @param echelle Échelle spatiale de la synthèse (commune, canton, communauté de communes, département, région, PNR, entité GEMAPI, PGSZH, bassin versant, polygone autre)
#' @param territoire territoire concerné. Unique ou sous la forme d'un vecteur (c("Villerserine", "Villers-Robert") par exemple)
#' @import DBI
#' @import ecotools
#' @import RPostgreSQL
#' @import sf
#' @import tidyverse
#' @export
#' @examples
#' territoire(data)
#' territoire("EPCI", "200071595")
#' territoire("Commune", c("Villerserine", "Villers-Robert"))
#' territoire("Commune", c("39568", "39571"))
#' territoire("Commune", c("Villerserine", "39571"))
#' territoire("Milieu", c("Lac de Chalain", "Bonlieu"))
#' territoire(echelle = "GEMAPI", territoire = "Contrat HVAO")

##### TODO LIST #####
# Ajout de l'échelon cantonal
# Ajout de l'échelon ComCom
# Problème avec par exemple Poligny -> Poligny + Vaux-sur-Poligny
#####################

territoire <- function(
  echelle = c("Commune", "Canton", "EPCI", "Département", "Région", "PNR", "GEMAPI", "PGSZH", "Bassin",  "Polygone"),
  territoire = NA_character_
)
{
  
  #### Évaluation des choix ####
  echelle <- match.arg(echelle)
  
  #### Contexte ####
  territoire <- territoire |> formatage_parametre()
  if(any(territoire |> is.na())) stop("Aucun territoire précisé")
  utilisateur <- session_utilisateur()

  #### Communes #####
  if(echelle == "Commune"){
    stop("Traitement des communes à développer")
    
    # dbD <- BDD.ouverture("Data")
    # communes <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.topographie_communes WHERE (tpcomm_departement_insee = '39');")
    # # DBI::dbDisconnect(dbD)
    #   #territoire <- select.list(sort(communes$tpcomm_commune_libelle), multiple = T) # Liste directement dans R, avec sélection par numéro
    #   # territoire <- tcltk::tk_select.list(sort(communes$tpcomm_commune_libelle), multiple = T) # Pour avoir un menu qui s'ouvre
    # }
    # sortie <- communes %>% mutate(nometcode = paste0(tpcomm_commune_libelle, " - ", tpcomm_commune_insee)) %>% filter(grepl(paste(territoire,collapse="|"), nometcode)) %>% select(-nometcode)
    # if(nrow(sortie) == 0){warning("Aucun résultat")}
  }
  
  #### Cantons ####
  if(echelle == "Canton"){
    stop("Traitement des cantons à développer en les ajoutant aux communes")
    # dbD <- BDD.ouverture("Data")
    # communes <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.topographie_communes WHERE (tpcomm_departement_insee = '39');")
    # DBI::dbDisconnect(dbD)
    # if(Liste == F){
    #   if(class(territoire) != "character"){stop("Liste de territoires pas au bon format")}
    # }
    # if(Liste == T){
    #   #territoire <- select.list(sort(communes$tpcomm_commune_libelle), multiple = T) # Liste directement dans R, avec sélection par numéro
    #   territoire <- tcltk::tk_select.list(sort(communes$tpcomm_commune_libelle), multiple = T) # Pour avoir un menu qui s'ouvre
    # }
    # sortie <- communes %>% mutate(nometcode = paste0(tpcomm_commune_libelle, " - ", tpcomm_commune_insee)) %>% filter(grepl(paste(territoire,collapse="|"), nometcode)) %>% select(-nometcode)
    # if(nrow(sortie) == 0){warning("Aucun résultat")}
  }
  
  #### ComCom ####
  if(echelle == "EPCI"){
    stop("Traitement des EPCI à développer en les ajoutant aux communes")
    # dbD <- BDD.ouverture("Data")
    # communes <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.topographie_communes WHERE (tpcomm_departement_insee = '39');")
    # DBI::dbDisconnect(dbD)
    # if(Liste == F){
    #   if(class(territoire) != "character"){stop("Liste de territoires pas au bon format")}
    # }
    # if(Liste == T){
    #   #territoire <- select.list(sort(communes$tpcomm_commune_libelle), multiple = T) # Liste directement dans R, avec sélection par numéro
    #   territoire <- tcltk::tk_select.list(sort(communes$tpcomm_commune_libelle), multiple = T) # Pour avoir un menu qui s'ouvre
    # }
    # sortie <- communes %>% mutate(nometcode = paste0(tpcomm_commune_libelle, " - ", tpcomm_commune_insee)) %>% filter(grepl(paste(territoire,collapse="|"), nometcode)) %>% select(-nometcode)
    # if(nrow(sortie) == 0){warning("Aucun résultat")}
  }
  
  #### Département ####
  if(echelle == "Département"){
    warning("Filtrage des départements à ajouter")
    sortie <- 
      departements() # %>% 
      # filter()
    if(nrow(sortie) == 0){warning("Aucun résultat")}
  }
  
  #### Région ####
  if(echelle == "Région"){
    sortie <- 
      region_bfc()
    if(nrow(sortie) == 0){warning("Aucun résultat")}
  }
  
  #### PNR ####
  if(echelle == "PNR"){
    db_r <- bdd.connexion("referentiels", "193.108.54.245", 5432, session_utilisateur(), keyring::key_get("referentiels", session_utilisateur()))
    motifs <- paste0("%", territoire, "%")
    sortie <- 
      sf::st_read(db_r, query = glue_sql("SELECT * FROM wfs.vm_pnr
     WHERE nom_site ILIKE ANY (ARRAY[{motifs*}]);", .con = db_r))
    if(nrow(sortie) == 0){warning("Aucun résultat")}
    dbDisconnect(db_r)
  }
  
  #### GEMAPI ####
  if(echelle == "GEMAPI"){
    stop("Traitement des entités GEMAPI à développer")

    dbD <- BDD.ouverture("Data")
    mo_territoires <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.gestion_moterritoires WHERE (gestmoterr_activite = 'TRUE');")
    DBI::dbDisconnect(dbD)
    if(Liste == F){
      if(class(territoire) != "character"){stop("Liste de territoires pas au bon format")}
    }
    if(Liste == T){
      #territoire <- select.list(sort(mo_territoires$gestmoterr_intitule), multiple = T) # Liste directement dans R, avec sélection par numéro
      # territoire <- tcltk::tk_select.list(sort(mo_territoires$gestmoterr_intitule), multiple = T) # Pour avoir un menu qui s'ouvre
      stop("Commande à remplacer pour supprimer la dépendance à tcltk::tk_select.list")
    }
    sortie <- mo_territoires %>% filter(grepl(paste(territoire, collapse="|"), gestmoterr_intitule))
    if(nrow(sortie) == 0){warning("Aucun résultat")}
  }

  #### PGSZH ####
  if(echelle == "PGSZH"){
    sortie <- 
      glue("C:/Users/{utilisateur}/Nextcloud/Pole Milieux humides BFC/Interne_Pole_MH_BFC/Pole MH BFC - Strategie actions/Accompagnement_PGS/pgszh-bfc_2026.shp") |> 
      read_sf() |> 
      filter(grepl(paste(territoire, collapse = "|"), NOM))
    if(nrow(sortie) == 0){warning("Aucun résultat")}
  }
  
  #### Polygone ####
  if(echelle == "Polygone"){
    stop("Traitement des polygones à développer")
    
    # Localisation du fichier #
    sortie <- sf::st_read(adresse.switch(territoire))
  }
  
  ## sortie des résultats ##
  return(sortie)
  
} # Fin de la fonction
