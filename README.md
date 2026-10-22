# Prix day-ahead de l'électricité en France et en Allemagne (2024-2025)

Analyse sous R de ce qui explique, et de ce qui permet de prévoir, le prix horaire de l'électricité sur le marché day-ahead : production éolienne et solaire, demande, prix du gaz.

**Auteur** : Marin Labouyrie

## Question

1. Comment la production éolienne et solaire, la demande et le prix du gaz sont-elles associées au prix horaire de l'électricité en France et en Allemagne-Luxembourg ?
2. Un modèle simple, utilisant seulement l'information disponible la veille, prévoit-il mieux le prix de demain qu'un repère naïf (le prix de la veille à la même heure) ?

## Données

| Donnée | Source | Pas de temps |
|---|---|---|
| Prix day-ahead, production éolienne et solaire réalisée, demande réalisée, prévisions de la veille (éolien, solaire, demande) | ENTSO-E Transparency Platform, via le paquet R `entsoeapi` | Horaire (quarts d'heure ramenés à l'heure) |
| Prix du gaz TTF (EUR/MWh) | ACER (données S&P Global) | Mensuel |

Période : janvier 2024 à décembre 2025. Les données brutes ne sont pas incluses dans ce dépôt ; les conditions de réutilisation n'ont pas été vérifiées. Le script `01_telechargement.R` les retélécharge.

## Méthode

- **Modèle explicatif** (par zone) : régression linéaire du prix sur l'éolien, le solaire, la demande et le gaz (TTF), avec effets d'heure de la journée et de saison. Erreurs standard regroupées par mois, car le gaz n'a que 24 valeurs distinctes.
- **Effet par saison** (France) : régression du prix sur l'éolien et le solaire, avec effets d'heure et d'année, erreurs de Newey-West (retard de 168 h).
- **Prévision** (France) : modèle estimé sur 2024 et testé sur 2025, avec les prévisions de la veille et le gaz du mois précédent. Comparaison à deux repères naïfs (prix de la veille, prix de la semaine précédente, même heure). Mesures : erreur absolue moyenne (MAE) et racine de l'erreur quadratique moyenne (RMSE), en €/MWh.

## Résultats

Les chiffres ci-dessous proviennent de `resultats/`. Vérifiez-les après avoir relancé les scripts.

**Modèle explicatif, 2024-2025** (variation du prix en €/MWh pour 1 GW de plus, ou 1 €/MWh de gaz en plus ; entre parenthèses : rapport t avec erreurs regroupées par mois)

| | France | Allemagne-Luxembourg |
|---|---|---|
| Éolien | −4,2 (−12,7) | −2,6 (−8,3) |
| Solaire | −4,5 (−9,4) | −2,0 (−4,6) |
| Demande | +3,0 (10,3) | +3,1 (11,7) |
| Gaz (TTF, mensuel) | +1,4 (4,9) | +0,8 (1,5) |
| R² ajusté | 0,73 | 0,61 |

Le gaz n'a pas d'effet détectable pour l'Allemagne dans cette spécification ; avec seulement 24 valeurs mensuelles, cela ne prouve pas qu'il n'a pas d'influence.

**Effet par saison (France)** : variation du prix en €/MWh pour 1 GW de plus, avec effets d'heure et d'année, sans la demande ni le gaz ; entre parenthèses : rapport t (Newey-West, 168 h).

| Saison | Éolien | Solaire | R² ajusté |
|---|---|---|---|
| Automne | −4,5 (−8,3) | −6,8 (−6,6) | 0,50 |
| Hiver | −5,2 (−10,6) | −2,8 (−2,3) | 0,52 |
| Printemps | −2,2 (−3,4) | −4,8 (−4,6) | 0,31 |
| Été | −5,0 (−8,1) | 0,0 (0,0) | 0,50 |

L'effet du solaire est nul en été dans ce modèle. Une explication possible, non testée : la production solaire d'été a presque la même forme chaque jour, et les effets d'heure absorbent l'essentiel de son effet.

**Heures à prix négatifs** (sur environ 8 760 heures par an) : France, 352 heures en 2024 et 513 en 2025 ; Allemagne-Luxembourg, 340 en 2024 et 491 en 2025. Elles se concentrent au printemps et en été. Le prix moyen est plus élevé en Allemagne qu'en France dans chacune des huit saisons de la période. Détail : `resultats/heures_negatives.csv`.

**Prévision, France, test sur 2025** (8 756 heures)

| Méthode | MAE (€/MWh) | RMSE (€/MWh) |
|---|---|---|
| Prix de la veille, même heure | 20,1 | 28,8 |
| Prix de la semaine précédente, même heure | 31,1 | 42,3 |
| Modèle, variables réalisées (avantagé) | 18,4 | 23,3 |
| Modèle, prévisions de la veille | 18,1 | 22,9 |
| Modèle, prévisions + prix de la veille | 15,5 | 20,1 |

Le meilleur modèle réduit le MAE d'environ 23 % par rapport au prix de la veille, pour un prix moyen de 61 €/MWh en 2025. L'erreur est la plus faible en hiver (12,9 €/MWh) et la plus élevée en été (18,6 €/MWh). L'écart entre les deux modèles intermédiaires est trop petit pour conclure.

## Limites

- Les résultats décrivent des associations statistiques, pas des effets causaux.
- Seulement 24 mois, et un seul test de prévision (2025, France) sans intervalle de confiance. Le niveau des prix du gaz a baissé entre 2024 et 2025, ce qui peut dégrader la prévision.
- Le gaz est mensuel : le modèle ne mesure pas ses variations d'une heure à l'autre. Pour la prévision, le gaz du mois précédent est utilisé.
- Les échanges avec les pays voisins, la disponibilité du nucléaire et du charbon ne sont pas dans le modèle.
- Les prévisions de production et de demande portent un numéro de révision que je n'ai pas contrôlé : je ne sais pas si ce sont les versions publiées la veille.
- Le modèle de prévision est linéaire : il peut prévoir des prix négatifs sans plancher, et il rate une partie des pics.
- La prévision ne porte que sur la France ; l'Allemagne n'est traitée que pour l'explication.

## Reproduire

1. R (testé avec R 4.4.2) et les paquets :
```r
   install.packages(c("dplyr", "lubridate", "purrr", "readr", "ggplot2", "sandwich", "remotes"))
   remotes::install_github("krose/entsoeapi")   # version testée : 1.2.1
```
2. **Jeton ENTSO-E** : créez un compte sur la plateforme de transparence ENTSO-E, demandez l'accès à l'API, générez un jeton, puis placez-le dans `~/.Renviron` sur une ligne (sans guillemets) : `ENTSOE_PAT=votre_jeton`. Redémarrez R. Ne publiez jamais ce fichier.
3. **Prix du gaz** : téléchargez le CSV « Gas prices in EU, US, and Asia (EUR/MWh) » sur la plateforme de données de l'ACER, et enregistrez-le sous `data/gaz_acer.csv`.
4. Exécutez dans l'ordre, depuis la racine du projet :
```r
   source("R/01_telechargement.R")
   source("R/02_preparation.R")
   source("R/03_modele.R")
```
   Le téléchargement dure plusieurs minutes. S'il s'interrompt (le serveur ENTSO-E peut ne pas répondre), relancez-le : les mois déjà téléchargés sont sautés.

## Structure

```
R/
  01_telechargement.R   téléchargement ENTSO-E avec reprise automatique
  02_preparation.R      assemblage, prix du gaz, variables de calendrier
  03_modele.R           modèles, prévision, tableaux et graphiques
resultats/              tableaux (CSV)
figures/                graphiques (PNG)
```