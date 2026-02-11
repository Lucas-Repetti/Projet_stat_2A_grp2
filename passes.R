library(ggplot2)
library(dplyr)
library(tidyr)

# -----------------------------
# 1. Charger le demi-terrain
# -----------------------------
source("demi terrain.R")  # suppose que p_terrain est créé dans ce script

# -----------------------------
# 2. Exemple de données de passes
# -----------------------------
# Remplacer par ta base réelle
# Colonnes : X_DEPART, Y_DEPART, X_ARRIVEE, Y_ARRIVEE
#passes <- actions_clees_details %>%
#  select(
#    X_DEPART  = X_DERNIERE_PASSE,
#    Y_DEPART  = Y_DERNIERE_PASSE,
#    X_ARRIVEE = X_DERNIERE_PASSE_RECEPTION,
#    Y_ARRIVEE = Y_DERNIERE_PASSE_RECEPTION,
#    LB_RESULTAT_DETAIL
#  ) %>%
#  filter(
#    !is.na(X_DEPART), !is.na(Y_DEPART),
#    !is.na(X_ARRIVEE), !is.na(Y_ARRIVEE),
#    !is.na(LB_RESULTAT_DETAIL)
#  )

# -----------------------------
# 3. Définir les zones pour chaque passeur et receveur
# -----------------------------
# On utilise les zones déjà définies dans demi terrain.R
assign_zone <- function(x, y, zones_df) {
  z <- zones_df %>%
    filter(
      x >= x_min, x < x_max,
      y >= y_min, y < y_max
    ) %>%
    pull(zone)
  
  if (length(z) == 0) {
    return(NA_character_)
  } else {
    return(z[1])  # on prend la première zone si ambigu
  }
}


passes <- passes %>%
  rowwise() %>%
  mutate(
    ZONE_PASSEUR   = assign_zone(X_DEPART, Y_DEPART, zones),
    ZONE_RECEPTEUR = assign_zone(X_ARRIVEE, Y_ARRIVEE, zones)
  ) %>%
  ungroup() %>%
  filter(!is.na(ZONE_PASSEUR), !is.na(ZONE_RECEPTEUR))

# -----------------------------
# 4. Résumer les passes entre zones
# -----------------------------
passes_summary <- passes %>%
  group_by(LB_RESULTAT_DETAIL, ZONE_PASSEUR, ZONE_RECEPTEUR) %>%
  summarise(NB_PASSES = n(), .groups="drop") %>%
  left_join(zones %>% select(zone, x_bar, y_bar),
            by = c("ZONE_PASSEUR" = "zone")) %>%
  rename(x_start = x_bar, y_start = y_bar) %>%
  left_join(zones %>% select(zone, x_bar, y_bar),
            by = c("ZONE_RECEPTEUR" = "zone")) %>%
  rename(x_end = x_bar, y_end = y_bar) %>%
  mutate(width = scales::rescale(NB_PASSES, to = c(0.5, 3)))

# -----------------------------
# 5. Tracer les flèches sur le demi-terrain
# -----------------------------
p_terrain +
  geom_segment(data=passes_summary,
               aes(x=x_start, y=y_start, xend=x_end, yend=y_end, linewidth=width),
               arrow = arrow(length = unit(0.15, "cm"), type="closed"),
               color="red", alpha=0.7) +
  scale_linewidth_identity() +
  labs(title="Demi-terrain avec passes entre zones")

plots <- passes_summary %>%
  split(.$LB_RESULTAT_DETAIL) %>%
  lapply(function(df) {
    p_terrain +
      geom_segment(
        data = df,
        aes(x = x_start, y = y_start,
            xend = x_end, yend = y_end,
            linewidth = width),
        arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
        color = "red",
        alpha = 0.7
      ) +
      scale_linewidth_identity() +
      labs(title = paste("Passes entre zones –", unique(df$LB_RESULTAT_DETAIL)))
  })
for(p in plots) {
  print(p)
}

flux_par_issue <- passes %>%
  group_by(LB_RESULTAT_DETAIL, ZONE_PASSEUR, ZONE_RECEPTEUR) %>%
  summarise(NB_PASSES = n(), .groups = "drop") %>%
  group_by(LB_RESULTAT_DETAIL) %>%
  mutate(PCT_PAR_ISSUE = 100 * NB_PASSES / sum(NB_PASSES)) %>%
  ungroup() %>%
  arrange(LB_RESULTAT_DETAIL, desc(PCT_PAR_ISSUE))
flux_par_issue





flux_eff <- passes %>%
  filter(LB_RESULTAT_DETAIL %in% c("BUT", "ARRÊT", "HORS CADRE")) %>%
    group_by(ZONE_PASSEUR, ZONE_RECEPTEUR, LB_RESULTAT_DETAIL) %>%
  summarise(NB = n(), .groups = "drop") %>%
    pivot_wider(
    names_from  = LB_RESULTAT_DETAIL,
    values_from = NB,
    values_fill = 0
  ) %>%
  
  mutate(
    TIRS_TOTAL  = BUT + ARRÊT + `HORS CADRE`,
    TAUX_BUT    = BUT / TIRS_TOTAL,
    TAUX_ARRET  = ARRÊT / TIRS_TOTAL,
    FLUX        = paste(ZONE_PASSEUR, "→", ZONE_RECEPTEUR)
  ) %>%
  
  arrange(desc(TAUX_BUT))

flux_eff





# Graph du taux d'arrêt par flux
ggplot(flux_eff %>% arrange(TAUX_ARRET) %>% mutate(FLUX = factor(FLUX, levels = FLUX)),
       aes(x = FLUX, y = TAUX_ARRET)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Taux d’arrêt par flux de passe",
    x = "Flux de passe",
    y = "Taux d’arrêt"
  ) +
  theme_minimal()

ggplot(flux_eff %>% arrange(TAUX_BUT) %>% mutate(FLUX = factor(FLUX, levels = FLUX)),
       aes(x = FLUX, y = TAUX_BUT)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Taux de but par flux de passe",
    x = "Flux de passe",
    y = "Taux de but"
  ) +
  theme_minimal()  






df_match_area %>%
  mutate(
    t = row_number(),
    momentum_change = MOMENTUM_NUM != lag(MOMENTUM_NUM),
    pic_pos = ETAT_DE_FORME == 1.00,
    pic_neg = ETAT_DE_FORME == 0.00
  ) %>%
  ggplot(aes(x = t)) +
  
  # Scores
  geom_line(aes(y = NB_SCORE_DOMICILE, color = "France"), linewidth = 1) +
  geom_line(aes(y = NB_SCORE_EXTERIEUR, color = "Grèce"), linewidth = 1) +
  
  # Changements de momentum
  geom_vline(
    data = function(d) d %>% filter(momentum_change & !is.na(momentum_change)),
    aes(xintercept = t),
    linetype = "dashed",
    color = "black",
    alpha = 0.6
  ) +
  
  # Pic de momentum positif (ETAT_DE_FORME == 1)
  geom_vline(
    data = function(d) d %>% filter(pic_pos),
    aes(xintercept = t),
    color = "lightblue",
    linewidth = 1
  ) +
  
  # Pic de momentum négatif (ETAT_DE_FORME == 0)
  geom_vline(
    data = function(d) d %>% filter(pic_neg),
    aes(xintercept = t),
    color = "pink",
    linewidth = 1
  ) +
  
  scale_color_manual(
    name = "Équipe",
    values = c("France" = "blue", "Grèce" = "red")
  ) +
  
  labs(
    title = "Évolution des scores France – Grèce\nChangements et pics de momentum",
    x = "Temps",
    y = "Score"
  ) +
  
  theme_minimal()
