data <- actions_clees_details
head(data)

library(dplyr)
library(ggplot2)
library(tidyr)


# Mise en place : actions avec dernière passe
passes <- data %>%
  filter(
    ID_JOUEUR_DERNIERE_PASSE != "",
    !is.na(ID_JOUEUR_DERNIERE_PASSE)
  ) %>%
  mutate(
    tir = LB_RESULTAT == "TIR",
    but = LB_RESULTAT_DETAIL == "BUT",
    faute = LB_RESULTAT == "FAUTE"
  )

print(
  passes %>%
    summarise(
      actions_avec_passe = n(),
      passeurs_distincts = n_distinct(ID_JOUEUR_DERNIERE_PASSE),
      tirs = sum(tir),
      buts = sum(but),
      taux_tir = mean(tir),
      taux_but = mean(but)
    )
)


# Différents types d’attaque avec dernière passe
passes_par_attaque <- passes %>%
  group_by(LB_SEQUENCE_TYPE) %>%
  summarise(
    actions = n(),
    tirs = sum(tir),
    buts = sum(but),
    taux_tir = mean(tir),
    taux_but = mean(but),
    .groups = "drop"
  ) %>%
  arrange(desc(actions))

print(passes_par_attaque)

ggplot(passes_par_attaque,
       aes(x = reorder(LB_SEQUENCE_TYPE, actions), y = actions)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Actions avec dernière passe par type d’attaque",
    x = "Type d’attaque",
    y = "Nombre d’actions"
  ) +
  theme_minimal()

# =====================================================
# Résultat des actions après dernière passe
# =====================================================
resultats_passe <- passes %>%
  count(LB_RESULTAT) %>%
  mutate(pct = n / sum(n))

print(resultats_passe)

ggplot(resultats_passe,
       aes(x = reorder(LB_RESULTAT, -n), y = pct)) +
  geom_col(fill = "purple") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Résultat des actions après dernière passe",
    x = "Résultat",
    y = "Pourcentage"
  ) +
  theme_minimal()

# =====================================================
# Détail des résultats des tirs après passe
# =====================================================
details_tirs_passe <- passes %>%
  filter(tir) %>%
  count(LB_RESULTAT_DETAIL) %>%
  mutate(pct = n / sum(n)) %>%
  arrange(desc(n))

print(details_tirs_passe)

ggplot(details_tirs_passe,
       aes(x = reorder(LB_RESULTAT_DETAIL, n), y = pct)) +
  geom_col(fill = "purple") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Détail des tirs après dernière passe",
    x = "Détail du tir",
    y = "Pourcentage"
  ) +
  theme_minimal()

# =====================================================
# Efficacité des attaques après dernière passe
# =====================================================
efficacite_passe <- passes %>%
  group_by(LB_SEQUENCE_TYPE) %>%
  summarise(
    actions = n(),
    tirs = sum(tir),
    buts = sum(but),
    taux_but = mean(but),
    .groups = "drop"
  ) %>%
  arrange(desc(taux_but))

print(efficacite_passe)

ggplot(efficacite_passe,
       aes(x = reorder(LB_SEQUENCE_TYPE, taux_but), y = taux_but)) +
  geom_col(fill = "purple") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Efficacité offensive après dernière passe",
    x = "Type d’attaque",
    y = "Taux de but"
  ) +
  theme_minimal()

# =====================================================
# Dernières passes et fautes provoquées
# =====================================================
fautes_passe <- passes %>%
  group_by(LB_SEQUENCE_TYPE) %>%
  summarise(
    actions = n(),
    fautes = sum(faute),
    fautes_par_action = mean(faute),
    .groups = "drop"
  )

print(fautes_passe)

ggplot(fautes_passe,
       aes(x = LB_SEQUENCE_TYPE, y = fautes_par_action)) +
  geom_col(fill = "purple") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Part des actions avec faute après dernière passe",
    x = "Type d’attaque",
    y = "% d’actions avec faute"
  ) +
  theme_minimal()


# =====================================================
# Préparation des données de passes spatialisées
# =====================================================

passes_xy <- passes %>%
  filter(
    X_DERNIERE_PASSE != "",
    Y_DERNIERE_PASSE != "",
    X_DERNIERE_PASSE_RECEPTION != "",
    Y_DERNIERE_PASSE_RECEPTION != ""
  ) %>%
  mutate(
    Xp = as.numeric(X_DERNIERE_PASSE),
    Yp = as.numeric(Y_DERNIERE_PASSE),
    Xr = as.numeric(X_DERNIERE_PASSE_RECEPTION),
    Yr = as.numeric(Y_DERNIERE_PASSE_RECEPTION)
  )

print(
  passes_xy %>%
    summarise(
      passes_spatialisees = n(),
      tirs = sum(tir),
      buts = sum(but),
      taux_tir = mean(tir),
      taux_but = mean(but)
    )
)

# =====================================================
# Zones de terrain (découpage simple)
# =====================================================

passes_xy <- passes_xy %>%
  mutate(
    zone_passe = case_when(
      Xp < 0.33 ~ "AR",
      Xp < 0.66 ~ "MI",
      TRUE ~ "AV"
    ),
    zone_reception = case_when(
      Xr < 0.33 ~ "AR",
      Xr < 0.66 ~ "MI",
      TRUE ~ "AV"
    )
  )

# =====================================================
# Statistiques par zone de passe
# =====================================================

zone_passe_stats <- passes_xy %>%
  group_by(zone_passe) %>%
  summarise(
    passes = n(),
    tirs = sum(tir),
    buts = sum(but),
    taux_tir = mean(tir),
    taux_but = mean(but),
    .groups = "drop"
  )

print(zone_passe_stats)

ggplot(zone_passe_stats,
       aes(x = zone_passe, y = taux_but)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Efficacité selon la zone de la dernière passe",
    x = "Zone de passe",
    y = "Taux de but"
  ) +
  theme_minimal()

# =====================================================
# Statistiques par zone de réception
# =====================================================

zone_recep_stats <- passes_xy %>%
  group_by(zone_reception) %>%
  summarise(
    receptions = n(),
    tirs = sum(tir),
    buts = sum(but),
    taux_tir = mean(tir),
    taux_but = mean(but),
    .groups = "drop"
  )

print(zone_recep_stats)

ggplot(zone_recep_stats,
       aes(x = zone_reception, y = taux_but)) +
  geom_col(fill = "purple") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Efficacité selon la zone de réception",
    x = "Zone de réception",
    y = "Taux de but"
  ) +
  theme_minimal()

# =====================================================
# Passe → Réception (flux AR / MI / AV)
# =====================================================

flux_passe <- passes_xy %>%
  count(zone_passe, zone_reception) %>%
  group_by(zone_passe) %>%
  mutate(pct = n / sum(n))

print(flux_passe)

ggplot(flux_passe,
       aes(x = zone_passe, y = pct, fill = zone_reception)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Flux des dernières passes (zone passe → réception)",
    x = "Zone de passe",
    y = "Répartition"
  ) +
  theme_minimal()

# =====================================================
# Cartographie des passes menant à un but
# =====================================================

ggplot(passes_xy %>% filter(but),
       aes(x = Xp, y = Yp, xend = Xr, yend = Yr)) +
  geom_segment(alpha = 0.15, color = "darkgreen") +
  labs(
    title = "Dernières passes menant à un but",
    x = "Largeur terrain",
    y = "Longueur terrain"
  ) +
  theme_minimal()

# =====================================================
# Efficacité spatiale par type d’attaque
# =====================================================

efficacite_zone_attaque <- passes_xy %>%
  group_by(LB_SEQUENCE_TYPE, zone_reception) %>%
  summarise(
    actions = n(),
    buts = sum(but),
    taux_but = mean(but),
    .groups = "drop"
  ) %>%
  arrange(desc(taux_but))

print(efficacite_zone_attaque)

# SYNTHESE :
# Les dernières passes sont impliquées dans une très grande majorité des situations offensives,
# avec plus de 80 % des actions menant à un tir et près d’une action sur deux aboutissant à un but.
# Les contre-attaques et phases de jeu rapides présentent une efficacité nettement supérieure
# aux attaques placées, bien que ces dernières concentrent l’essentiel du volume de passes.
# L’analyse spatiale montre que la zone de réception influence davantage la réussite que la zone de passe,
# les réceptions en zone avant étant légèrement plus efficaces.
# Globalement, la dernière passe apparaît comme un levier clé de création d’occasions de qualité.
