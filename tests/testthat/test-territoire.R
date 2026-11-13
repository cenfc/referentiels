library(cenbfc)
library(referentiels)
library(testthat)

test_that("territoire absence de valeur", {
  "" |> 
    formatage_parametre() |>
    territoire(territoire = _) |>
    expect_error("Aucun territoire précisé")
})

test_that("territoire valeur sans résultat", {
  "ededed" |> 
    formatage_parametre() |>
    territoire(echelle = "Commune", territoire = _) |>
    expect_error("Traitement des communes à développer") # temporaire le temps de développer les communes
    # expect_warning("Aucun résultat") # Pour plus tard
  
  "ededed" |> 
    formatage_parametre() |>
    territoire(echelle = "PNR", territoire = _) |>
    expect_warning("Aucun résultat")
})

test_that("territoire valeur avec résultat(s)", {
  "Jura" |> 
    formatage_parametre() |>
    territoire(echelle = "PNR", territoire = _) |>
    expect_shape(nrow = 1)
  
  c("Jura", "Doubs") |> 
    # formatage_parametre() |>
    territoire(echelle = "PNR", territoire = _) |>
    expect_shape(nrow = 2)
})

