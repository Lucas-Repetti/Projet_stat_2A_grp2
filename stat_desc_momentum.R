library(dplyr)
library(ggplot2)


momentum_desc <- read.csv2("data/momentum_desc", sep = ",")


momentum_summary <- momentum_desc %>%
  mutate(
    momentum_pres = nb_momentums > 0,
    GAGNE = as.logical(GAGNE)           # convertir en TRUE/FALSE
  ) %>%
  group_by(momentum_pres) %>%
  summarise(
    taux_victoire = mean(GAGNE, na.rm = TRUE) * 100,  # % victoires
    ecart_points_moy = mean(ECART_POINTS, na.rm = TRUE), # écart moyen
    n_equipes = n()
  )

momentum_summary




# Régression linéaire simple
regression_momentum <- lm(ECART_POINTS ~ nb_momentums, data = momentum_desc)

# Résumé de la régression
summary(regression_momentum)



ggplot(momentum_desc, aes(x = nb_momentums, y = ECART_POINTS)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5, color = "steelblue") +  # points légèrement décalés pour mieux voir
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # droite de régression avec intervalle de confiance
  theme_minimal() +
  labs(
    title = "Relation entre le nombre de momentum et l'écart de points",
    x = "Nombre de momentum",
    y = "Écart de points"
  )




# Calculer le taux de victoire par nombre de momentum
momentum_victoire <- momentum_desc %>%
  group_by(nb_momentums) %>%
  summarise(
    taux_victoire = mean(GAGNE, na.rm = TRUE) * 100,
    n_equipes = n()
  ) %>%
  arrange(nb_momentums)

momentum_victoire

# Graphique : taux de victoire selon le nombre de momentum
ggplot(momentum_victoire, aes(x = nb_momentums, y = taux_victoire)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(taux_victoire, 1)), vjust = -0.5) +
  scale_x_continuous(breaks = 0:max(momentum_victoire$nb_momentums)) +
  ylim(0, 100) +
  theme_minimal() +
  labs(
    title = "Taux de victoire selon le nombre de momentum",
    x = "Nombre de momentum par équipe",
    y = "Taux de victoire (%)"
  )

# Optionnel : distribution du nombre de momentum
momentum_dist <- momentum_desc %>%
  count(nb_momentums) %>%
  mutate(freq = n / sum(n) * 100)

momentum_dist

ggplot(momentum_dist, aes(x = nb_momentums, y = freq)) +
  geom_col(fill = "orange") +
  geom_text(aes(label = round(freq, 1)), vjust = -0.5) +
  scale_x_continuous(breaks = 0:max(momentum_dist$nb_momentums)) +
  ylim(0, max(momentum_dist$freq) + 5) +
  theme_minimal() +
  labs(
    title = "Distribution du nombre de momentum par équipe",
    x = "Nombre de momentum",
    y = "Pourcentage d'équipes (%)"
  )



stats_equipes <- momentum_desc %>%
  group_by(MOMENTUM) %>%
  summarise(
    total_momentum = sum(nb_momentums, na.rm = TRUE),          
    nb_matchs = n(),                                           
    nb_victoires = sum(GAGNE, na.rm = TRUE),
    taux_victoire = nb_victoires / nb_matchs * 100,           
  ) %>%
  arrange(desc(total_momentum))

stats_equipes


regression_total <- lm(taux_victoire ~ total_momentum, data = stats_equipes)

# Résumé de la régression
summary(regression_total)



stats_equipes_FR <- stats_equipes %>%
  filter(grepl("^FR", MOMENTUM))  # ne garder que celles qui commencent par "FR"

# Régression linéaire : taux de victoire ~ total_momentum pour les équipes FR
regression_FR <- lm(taux_victoire ~ total_momentum, data = stats_equipes_FR)

# Résumé de la régression
summary(regression_FR)



