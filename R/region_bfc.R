#' Obtention de la Région (SIG + surface)
#'
#' Cette fonction permet de charger les données de la région BFC
#' @name region_bfc
#' @param utilisateur Nom d'utilisateur de la base de données
#' @import sf
#' @export
#' @examples
#' region_bfc <- region_bfc("utilisateur")

region_bfc <- function(
    utilisateur = NA_character_
    )
{
  
  #### Collecte des données ####
  region_bfc <- departements(utilisateur) |> st_union()
  
  #### Sortie ####
  return(region_bfc)
  
} # Fin de la fonction
