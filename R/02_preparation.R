# 02_preparation.R
# Assemble les fichiers téléchargés, ajoute le prix du gaz (TTF, mensuel) et les
# variables de calendrier, puis sauvegarde le tout dans data/donnees_preparees.rds.
#
# Prérequis : data/gaz_acer.csv (fichier ACER des prix du gaz, voir README.md).

library(dplyr)
library(lubridate)
library(readr)
library(purrr)

lire_dossier <- function(dossier) {
  list.files(dossier, pattern = "\\.csv$", full.names = TRUE) |>
    map_dfr(read_csv, show_col_types = FALSE)
}

# --- Prix du gaz (TTF, EUR/MWh, mensuel) ---------------------------------------
# Le fichier ACER se lit en une seule colonne : chaque ligne est un texte du type
#   01.01.2019,"21.69","9.29","24.43"   (date, TTF, Henry Hub, JKM)
fichier_gaz <- "data/gaz_acer.csv"
if (!file.exists(fichier_gaz)) {
  stop("Fichier ", fichier_gaz, " introuvable : voir README.md, section 'Reproduire'.")
}
brut  <- read.csv(fichier_gaz)
texte <- gsub("\"", "", brut[[1]])
parts <- strsplit(texte, ",")

gaz_m <- data.frame(
  date        = as.Date(sapply(parts, `[`, 1), format = "%d.%m.%Y"),
  ttf_eur_mwh = as.numeric(sapply(parts, `[`, 2))
)
stopifnot(!anyNA(gaz_m$date), !anyNA(gaz_m$ttf_eur_mwh))
message("Gaz : de ", min(gaz_m$date), " à ", max(gaz_m$date))

# --- Prix, production et demande réalisées (France et Allemagne-Luxembourg) ----
zones <- lire_dossier("data/zone_mois") |>
  distinct(zone, datetime, .keep_all = TRUE)

preparer <- function(df) {
  df |>
    mutate(
      local      = with_tz(datetime, "Europe/Paris"),
      heure      = factor(hour(local)),
      mois       = month(local),
      annee      = year(local),
      saison     = case_when(mois %in% c(12, 1, 2) ~ "hiver",
                             mois %in% 3:5         ~ "printemps",
                             mois %in% 6:8         ~ "ete",
                             TRUE                  ~ "automne"),
      eolien_gw  = eolien_mw / 1000,
      solaire_gw = solaire_mw / 1000,
      conso_gw   = conso_mw / 1000,
      mois_debut = as.Date(format(local, "%Y-%m-01"))
    ) |>
    left_join(gaz_m, by = c("mois_debut" = "date")) |>
    filter(!is.na(ttf_eur_mwh), !is.na(conso_gw))
}

fr_p <- zones |> filter(zone == "FR")    |> preparer()
de_p <- zones |> filter(zone == "DE_LU") |> preparer()
message("Heures : FR = ", nrow(fr_p), ", DE_LU = ", nrow(de_p))

# --- Prévisions de la veille (France) ------------------------------------------
prev <- lire_dossier("data/prev_mois") |>
  distinct(datetime, .keep_all = TRUE) |>
  mutate(eolien_prev_gw  = eolien_prev_mw / 1000,
         solaire_prev_gw = solaire_prev_mw / 1000,
         conso_prev_gw   = conso_prev_mw / 1000)

saveRDS(list(fr = fr_p, de = de_p, gaz_m = gaz_m, prev = prev),
        "data/donnees_preparees.rds")
message("Données préparées enregistrées dans data/donnees_preparees.rds")