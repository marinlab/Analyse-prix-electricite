# 01_telechargement.R
# Télécharge les données ENTSO-E, un fichier CSV par zone et par mois :
#   - data/zone_mois/ : prix day-ahead, production éolienne et solaire réalisée,
#                       demande réalisée (France et Allemagne-Luxembourg)
#   - data/prev_mois/ : prévisions de la veille (éolien, solaire, demande), France
# Les mois déjà téléchargés sont sautés : on peut relancer le script après une erreur.
#
# Prérequis : un jeton ENTSO-E dans la variable d'environnement ENTSOE_PAT
# (fichier ~/.Renviron, jamais dans le code). Voir README.md.

library(entsoeapi)
library(dplyr)
library(lubridate)
library(purrr)
library(readr)

if (nchar(Sys.getenv("ENTSOE_PAT")) == 0) {
  stop("Variable ENTSOE_PAT introuvable : voir README.md, section 'Reproduire'.")
}

ZONES <- c(FR = "10YFR-RTE------C", DE_LU = "10Y1001A1001A82H")
MOIS  <- seq(ymd("2024-01-01", tz = "CET"), by = "month", length.out = 24)

dir.create("data/zone_mois", recursive = TRUE, showWarnings = FALSE)
dir.create("data/prev_mois", recursive = TRUE, showWarnings = FALSE)

# --- Outil : télécharger avec nouvelles tentatives et sauvegarde ---------------
avec_reprise <- function(fichier, fun, essais = 4) {
  if (file.exists(fichier)) return(invisible(TRUE))   # mois déjà téléchargé
  for (i in seq_len(essais)) {
    res <- tryCatch(fun(), error = function(e) NULL)
    if (!is.null(res) && nrow(res) > 0) {
      write_csv(res, fichier)
      return(invisible(TRUE))
    }
    Sys.sleep(10 * i)   # pause de plus en plus longue avant de réessayer
  }
  message("Échec définitif : ", fichier)
  invisible(FALSE)
}

# --- Un mois, une zone : prix, production réalisée, demande réalisée ----------
# Toutes les séries sont ramenées au pas horaire (moyenne des quarts d'heure).
telecharger_zone_mois <- function(debut, eic, zone) {
  fin <- debut + months(1)
  
  prix <- energy_prices(eic = eic, period_start = debut, period_end = fin) |>
    transmute(datetime = floor_date(ts_point_dt_start, "hour"),
              p = ts_point_price_amount) |>
    group_by(datetime) |>
    summarise(prix_eur_mwh = mean(p), .groups = "drop")
  
  gen <- gen_per_prod_type(eic = eic, period_start = debut, period_end = fin,
                           gen_type = NULL, tidy_output = TRUE) |>
    filter(ts_mkt_psr_type %in% c("B16", "B18", "B19")) |>   # solaire, éolien mer, éolien terre
    mutate(datetime = floor_date(ts_point_dt_start, "hour"),
           filiere  = if_else(ts_mkt_psr_type == "B16", "solaire", "eolien")) |>
    group_by(datetime, ts_mkt_psr_type, filiere) |>
    summarise(mw = mean(ts_point_quantity), .groups = "drop") |>
    group_by(datetime) |>
    summarise(solaire_mw = sum(mw[filiere == "solaire"]),
              eolien_mw  = sum(mw[filiere == "eolien"]), .groups = "drop")
  
  conso <- load_actual_total(eic = eic, period_start = debut, period_end = fin,
                             tidy_output = TRUE) |>
    mutate(datetime = floor_date(ts_point_dt_start, "hour")) |>
    group_by(datetime) |>
    summarise(conso_mw = mean(ts_point_quantity), .groups = "drop")
  
  prix |>
    inner_join(gen,   by = "datetime") |>
    inner_join(conso, by = "datetime") |>
    mutate(zone = zone)
}

# --- Un mois, France : prévisions de la veille (process_type A01 = day ahead) --
telecharger_prev_mois <- function(debut, eic = ZONES[["FR"]]) {
  fin <- debut + months(1)
  
  enr <- gen_wind_solar_forecasts(eic = eic, period_start = debut, period_end = fin,
                                  process_type = "A01", tidy_output = TRUE) |>
    mutate(datetime = floor_date(ts_point_dt_start, "hour"),
           filiere  = if_else(ts_mkt_psr_type == "B16", "solaire", "eolien")) |>
    group_by(datetime, ts_mkt_psr_type, filiere) |>
    summarise(mw = mean(ts_point_quantity), .groups = "drop") |>
    group_by(datetime) |>
    summarise(solaire_prev_mw = sum(mw[filiere == "solaire"]),
              eolien_prev_mw  = sum(mw[filiere == "eolien"]), .groups = "drop")
  
  conso <- load_day_ahead_total_forecast(eic = eic, period_start = debut,
                                         period_end = fin, tidy_output = TRUE) |>
    mutate(datetime = floor_date(ts_point_dt_start, "hour")) |>
    group_by(datetime) |>
    summarise(conso_prev_mw = mean(ts_point_quantity), .groups = "drop")
  
  inner_join(enr, conso, by = "datetime")
}

# --- Boucles de téléchargement --------------------------------------------------
for (z in names(ZONES)) {
  for (i in seq_along(MOIS)) {
    debut   <- MOIS[i]
    fichier <- file.path("data/zone_mois",
                         paste0(z, "_", format(debut, "%Y-%m"), ".csv"))
    avec_reprise(fichier, function() telecharger_zone_mois(debut, ZONES[[z]], z))
  }
}

for (i in seq_along(MOIS)) {
  debut   <- MOIS[i]
  fichier <- file.path("data/prev_mois", paste0("FR_", format(debut, "%Y-%m"), ".csv"))
  avec_reprise(fichier, function() telecharger_prev_mois(debut))
}

message("Fichiers zone-mois : ", length(list.files("data/zone_mois")), " (attendu : 48)")
message("Fichiers prévisions : ", length(list.files("data/prev_mois")), " (attendu : 24)")
