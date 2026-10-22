# 03_modele.R
# Analyse : (1) modèle explicatif France / Allemagne, (2) effet par saison (France),
# (3) heures à prix négatifs, (4) prévision hors échantillon (France), (5) figures.
# Les tableaux vont dans resultats/, les graphiques dans figures/.

library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(ggplot2)
library(sandwich)

dir.create("resultats", showWarnings = FALSE)
dir.create("figures",   showWarnings = FALSE)

dd    <- readRDS("data/donnees_preparees.rds")
fr_p  <- dd$fr
de_p  <- dd$de
gaz_m <- dd$gaz_m
prev  <- dd$prev

# =============================================================================
# 1. Modèle explicatif, par zone (prix ~ éolien + solaire + demande + gaz)
#    Erreurs standard regroupées par mois (le gaz n'a que 24 valeurs distinctes).
# =============================================================================
estimer <- function(df) {
  m    <- lm(prix_eur_mwh ~ eolien_gw + solaire_gw + conso_gw + ttf_eur_mwh +
               heure + saison, data = df)
  noms <- c("eolien_gw", "solaire_gw", "conso_gw", "ttf_eur_mwh")
  se   <- sqrt(diag(vcovCL(m, cluster = df$mois_debut)))[noms]
  cf   <- coef(m)[noms]
  tibble(zone = df$zone[1], variable = noms, n = nrow(df),
         coefficient = round(unname(cf), 2),
         t_mois      = round(unname(cf / se), 1),
         r2_ajuste   = round(summary(m)$adj.r.squared, 2))
}

res_zone <- bind_rows(estimer(fr_p), estimer(de_p))
print(res_zone)
write_csv(res_zone, "resultats/modele_explicatif_fr_de.csv")

# =============================================================================
# 2. Effet de l'éolien et du solaire par saison (France)
#    Erreurs standard de Newey-West (retard d'une semaine = 168 h).
# =============================================================================
res_saison <- fr_p |>
  group_split(saison) |>
  map_dfr(function(x) {
    m   <- lm(prix_eur_mwh ~ eolien_gw + solaire_gw + heure + factor(annee), data = x)
    se  <- sqrt(diag(NeweyWest(m, lag = 168)))[c("eolien_gw", "solaire_gw")]
    cf  <- coef(m)[c("eolien_gw", "solaire_gw")]
    tibble(saison = x$saison[1], n = nrow(x),
           eolien      = round(unname(cf["eolien_gw"]), 2),
           t_eolien    = round(unname(cf["eolien_gw"] / se["eolien_gw"]), 1),
           solaire     = round(unname(cf["solaire_gw"]), 2),
           t_solaire   = round(unname(cf["solaire_gw"] / se["solaire_gw"]), 1),
           r2_ajuste   = round(summary(m)$adj.r.squared, 2))
  })
print(res_saison)
write_csv(res_saison, "resultats/effet_par_saison_fr.csv")

# =============================================================================
# 3. Prix moyen et heures à prix négatifs, par zone, année et saison
# =============================================================================
neg <- bind_rows(fr_p, de_p) |>
  group_by(zone, annee, saison) |>
  summarise(heures = n(),
            prix_moyen = round(mean(prix_eur_mwh), 1),
            heures_negatives = sum(prix_eur_mwh < 0),
            .groups = "drop")
print(neg, n = Inf)
write_csv(neg, "resultats/heures_negatives.csv")

# =============================================================================
# 4. Prévision hors échantillon (France) : estimation sur 2024, test sur 2025
#    Le gaz utilisé est celui du mois PRÉCÉDENT (le prix du mois en cours n'est
#    pas connu à l'avance). Les retards de 24 h et 168 h sont calculés sur les
#    lignes : les quelques heures manquantes les décalent très légèrement.
# =============================================================================
gaz_prev <- gaz_m |>
  mutate(date = date %m+% months(1)) |>
  rename(ttf_prev = ttf_eur_mwh)

base <- fr_p |>
  left_join(gaz_prev, by = c("mois_debut" = "date")) |>
  arrange(datetime) |>
  mutate(prix_veille  = lag(prix_eur_mwh, 24),
         prix_semaine = lag(prix_eur_mwh, 168)) |>
  filter(!is.na(ttf_prev)) |>
  left_join(prev, by = "datetime")

vars <- c("prix_eur_mwh", "eolien_gw", "solaire_gw", "conso_gw", "ttf_prev",
          "eolien_prev_gw", "solaire_prev_gw", "conso_prev_gw",
          "prix_veille", "heure", "saison")
base <- base |> filter(if_all(all_of(vars), ~ !is.na(.x)))

train <- base |> filter(annee == 2024)
test  <- base |> filter(annee == 2025)

m_real  <- lm(prix_eur_mwh ~ eolien_gw + solaire_gw + conso_gw + ttf_prev +
                heure + saison, data = train)
m_prev1 <- lm(prix_eur_mwh ~ eolien_prev_gw + solaire_prev_gw + conso_prev_gw +
                ttf_prev + heure + saison, data = train)
m_prev2 <- lm(prix_eur_mwh ~ eolien_prev_gw + solaire_prev_gw + conso_prev_gw +
                ttf_prev + prix_veille + heure + saison, data = train)

y    <- test$prix_eur_mwh
mae  <- function(p) mean(abs(y - p))
rmse <- function(p) sqrt(mean((y - p)^2))

comparaison <- tibble(
  methode = c("Prix de la veille, même heure",
              "Prix de la semaine précédente, même heure",
              "Modèle, variables réalisées (avantagé)",
              "Modèle, prévisions de la veille",
              "Modèle, prévisions + prix de la veille"),
  MAE  = c(mae(test$prix_veille), mae(test$prix_semaine),
           mae(predict(m_real, test)), mae(predict(m_prev1, test)),
           mae(predict(m_prev2, test))),
  RMSE = c(rmse(test$prix_veille), rmse(test$prix_semaine),
           rmse(predict(m_real, test)), rmse(predict(m_prev1, test)),
           rmse(predict(m_prev2, test)))
) |>
  mutate(across(c(MAE, RMSE), ~ round(.x, 1)))

print(comparaison)
message("Heures du jeu de test (2025) : ", nrow(test))
write_csv(comparaison, "resultats/prevision_2025.csv")

test$pred <- predict(m_prev2, test)

erreur_saison <- test |>
  mutate(err = abs(prix_eur_mwh - pred)) |>
  group_by(saison) |>
  summarise(MAE = round(mean(err), 1), prix_moyen = round(mean(prix_eur_mwh), 1),
            .groups = "drop")
print(erreur_saison)
write_csv(erreur_saison, "resultats/erreur_par_saison_2025.csv")

qualite_eolien <- tibble(
  erreur_moyenne_gw = round(mean(abs(test$eolien_gw - test$eolien_prev_gw)), 2),
  correlation       = round(cor(test$eolien_gw, test$eolien_prev_gw), 2)
)
print(qualite_eolien)
write_csv(qualite_eolien, "resultats/qualite_prevision_eolien.csv")

# =============================================================================
# 5. Figures
# =============================================================================
theme_set(theme_minimal(base_size = 12))

# 5a. Prix moyen journalier, France et Allemagne
prix_jour <- bind_rows(fr_p, de_p) |>
  mutate(jour = as.Date(local)) |>
  group_by(zone, jour) |>
  summarise(prix = mean(prix_eur_mwh), .groups = "drop")

g1 <- ggplot(prix_jour, aes(jour, prix, colour = zone)) +
  geom_line(linewidth = 0.4) +
  labs(title = "Prix day-ahead moyen journalier, 2024-2025",
       x = NULL, y = "€/MWh", colour = "Zone")
ggsave("figures/01_prix_journaliers.png", g1, width = 9, height = 4.5, dpi = 150)

# 5b. Prix et production éolienne, par saison (France)
g2 <- ggplot(fr_p, aes(eolien_gw, prix_eur_mwh)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "lm", formula = y ~ x) +
  facet_wrap(~ saison) +
  labs(title = "Prix day-ahead et production éolienne, France, 2024-2025",
       x = "Éolien (GW)", y = "Prix (€/MWh)")
ggsave("figures/02_prix_vs_eolien_fr.png", g2, width = 9, height = 6, dpi = 150)

# 5c. Heures à prix négatifs
g3 <- ggplot(neg, aes(saison, heures_negatives, fill = factor(annee))) +
  geom_col(position = "dodge") +
  facet_wrap(~ zone) +
  labs(title = "Heures à prix day-ahead négatif", x = NULL,
       y = "Nombre d'heures", fill = "Année")
ggsave("figures/03_heures_negatives.png", g3, width = 9, height = 4.5, dpi = 150)

# 5d. Prix réalisé et prévu, deux semaines (une avec des pics, une calme)
semaines <- bind_rows(
  test |>
    filter(local >= ymd("2025-01-13", tz = "Europe/Paris"),
           local <  ymd("2025-01-20", tz = "Europe/Paris")) |>
    mutate(periode = "Semaine du 13 janvier 2025"),
  test |>
    filter(local >= ymd("2025-07-14", tz = "Europe/Paris"),
           local <  ymd("2025-07-21", tz = "Europe/Paris")) |>
    mutate(periode = "Semaine du 14 juillet 2025")
)

g4 <- ggplot(semaines, aes(local)) +
  geom_line(aes(y = prix_eur_mwh, colour = "Réalisé")) +
  geom_line(aes(y = pred, colour = "Prévu")) +
  facet_wrap(~ periode, scales = "free_x", ncol = 1) +
  labs(title = "Prix day-ahead réalisé et prévu, France",
       x = NULL, y = "€/MWh", colour = NULL)
ggsave("figures/04_realise_vs_prevu.png", g4, width = 9, height = 6, dpi = 150)

message("Terminé : tableaux dans resultats/, graphiques dans figures/")