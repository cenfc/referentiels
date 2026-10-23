#' Obtention des départements (SIG + surface)
#'
#' Cette fonction permet de charger les données de départements FC
#' @name departements
#' @param utilisateur Nom d'utilisateur de la base de données
#' @import cenbfc
#' @import dplyr
#' @import keyring
#' @import sf
#' @export
#' @examples
#' departements <- departements("utilisateur")

departements <- function(
    utilisateur = NA_character_
    )
{
  
  #### Collecte des données ####
  bd_r <- bdd.connexion("referentiels", "193.108.54.245", 5432, utilisateur, keyring::key_get("referentiels", utilisateur))
  communes <- st_read(bd_r, query = "select * from wfs.vm_communes_bfc;") # Tjs prendre cette vue matérialisée de référence
  
  #### Calcul ####
  departements <- 
    communes %>%
    group_by(insee_dep) %>%
    summarise(
      #   n_communes = n(),
      .groups = "drop"
    ) %>%
    st_make_valid() |> 
    mutate(superficie = st_area(geom)) |> 
    mutate(superficie = units::set_units(superficie, "km2")) |> 
    relocate(superficie, .after = insee_dep)
  
  #### Sortie ####
  return(departements)
  
} # Fin de la fonction
