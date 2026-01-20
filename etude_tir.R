# Chargement des librairies
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringi)  # pour enlever les accents

# -----------------------------
# 1️⃣ Nettoyage + normalisation
# -----------------------------
actions_clees_details <- actions_clees_details %>%
  mutate(
    # conversion en numérique
    X_CAGE = as.numeric(gsub(",", ".", X_CAGE)),
    Y_CAGE = as.numeric(gsub(",", ".", Y_CAGE)),
    # normalisation des textes : minuscule + suppression des accents
    LB_SEQUENCE_TYPE = stri_trans_general(LB_SEQUENCE_TYPE, "Latin-ASCII") %>% tolower()
  ) %>%
  # suppression des lignes invalides
  filter(!is.na(X_CAGE), !is.na(Y_CAGE)) %>%
  # définition des zones
  mutate(
    zone_x = case_when(
      X_CAGE < 1/3 ~ "Petit filet gauche",
      X_CAGE < 2/3 ~ "Centre",
      TRUE         ~ "Petit filet droit"
    ),
    zone_y = case_when(
      Y_CAGE < 1/3 ~ "Bas",
      Y_CAGE < 2/3 ~ "Milieu",
      TRUE         ~ "Lucarne"
    ),
    ZONE_CAGE = paste(zone_y, zone_x)
  )

# -----------------------------
# 2️⃣ Graphiques un par un
# -----------------------------
types_sequence <- unique(actions_clees_details$LB_SEQUENCE_TYPE)

for (type in types_sequence) {
  
  p <- ggplot(
    actions_clees_details %>% filter(LB_SEQUENCE_TYPE == type),
    aes(x = X_CAGE, y = Y_CAGE, color = ZONE_CAGE)
  ) +
    
    # cage de hand
    geom_rect(
      xmin = 0, xmax = 1,
      ymin = 0, ymax = 1,
      fill = "grey95",
      color = "black",
      linewidth = 1
    ) +
    
    # lignes de zonage
    geom_vline(xintercept = c(1/3, 2/3), linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = c(1/3, 2/3), linetype = "dashed", color = "grey50") +
    
    # points des tirs
    geom_point(size = 2, alpha = 0.8) +
    
    coord_fixed(ratio = 2/3) +
    
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    
    labs(
      title = paste("Répartition des tirs –", type),
      color = "Zone de la cage",
      x = NULL,
      y = NULL
    ) +
    
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      legend.position = "right"
    )
  
  print(p)
}

# -----------------------------
# 3️⃣ Tableau des pourcentages
# -----------------------------
tableau_zones_pct <- actions_clees_details %>%
  count(LB_SEQUENCE_TYPE, ZONE_CAGE, name = "NB_TIRS") %>%
  group_by(LB_SEQUENCE_TYPE) %>%
  mutate(
    PCT_TIRS = NB_TIRS / sum(NB_TIRS) * 100
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = ZONE_CAGE,
    values_from = PCT_TIRS,
    values_fill = 0
  ) %>%
  arrange(LB_SEQUENCE_TYPE)

# Affichage du tableau
tableau_zones_pct
