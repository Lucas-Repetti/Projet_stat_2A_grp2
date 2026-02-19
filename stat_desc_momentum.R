library(dplyr)
library(ggplot2)


momentum_desc <- read.csv2("data/momentum_desc", sep = ",")
classement_2021 <- tibble::tribble(
  ~MOMENTUM,              ~classement_final,
  "FR_PSGHB_75016",     1,   # Paris-SG
  "FR_HBCN_44200",      2,   # Nantes
  "FR_MHB_34000",       3,   # Montpellier
  "FR_LHB_87000",       4,   # Limoges
  "FR_CSMBHB_73000",    5,   # Chambéry
  "FR_TFHB_93290",      6,   # Tremblay
  "FR_FTHB_31000",      7,   # Toulouse
  "FR_SRVHB_83700",     8,   # Saint-Raphaël
  "FR_USAMNG_30918",    9,   # Nîmes
  "FR_PAUC_13100",     10,   # Aix-en-Provence
  "FR_CRMHB_35510",    11,   # Cesson-Rennes
  "FR_IPHB_13806",     12,   # Sélestat
  "FR_DHBGL_59140",    13,   # Dijon
  "FR_CCMHB_28000",    14,   # Chartres
  "FR_USIHB_94200",    15,   # Dunkerque
  "FR_USCHB_94000",    16    # Istres
)
momentum_desc <- momentum_desc %>%
  left_join(classement_2021, by = "MOMENTUM")


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
    classement_final = first(classement_final),  # récupère le classement de la table initiale
    .groups = "drop"
  ) %>%
  arrange(desc(total_momentum))


stats_equipes


regression_total <- lm(taux_victoire ~ total_momentum, data = stats_equipes)

# Résumé de la régression
summary(regression_total)



stats_equipes_FR <- stats_equipes %>%
  filter(grepl("^FR", MOMENTUM))  

momentum_desc_FR <- momentum_desc %>%
  filter(grepl("^FR", CD_MATCH))  

# Régression linéaire : taux de victoire ~ total_momentum pour les équipes FR
regression_FR <- lm(taux_victoire ~ total_momentum, data = stats_equipes_FR)

# Résumé de la régression
summary(regression_FR)


# Somme des momentum par match
momentum_par_match <- momentum_desc_FR %>%
  group_by(CD_MATCH) %>%
  summarise(total_momentum = sum(nb_momentums, na.rm = TRUE))

# Nombre de matchs avec au moins un momentum
nb_match_avec_momentum <- momentum_par_match %>%
  filter(total_momentum > 0) %>%
  nrow()

nb_match_avec_momentum
nb_match_total <- n_distinct(momentum_desc_FR$CD_MATCH)
nb_match_avec_momentum/nb_match_total












resultat_momentum <- momentum_desc_FR %>%
  # on enlève les matchs nuls
  filter(!is.na(GAGNE)) %>%
  group_by(CD_MATCH, MOMENTUM) %>%
  summarise(
    total_momentum = sum(nb_momentums, na.rm = TRUE),
    GAGNE = first(GAGNE),
    .groups = "drop"
  ) %>%
  group_by(CD_MATCH) %>%
  summarise(
    gagne_plus_momentum = GAGNE[which.max(total_momentum)] == TRUE,
    .groups = "drop"
  )

# Proportion de matchs où le + de momentum gagne
proportion <- mean(resultat_momentum$gagne_plus_momentum)

proportion





# Corrélation Spearman
cor_spearman <- cor.test(
  stats_equipes_FR$total_momentum/stats_equipes_FR$,
  stats_equipes_FR$classement_final,
  method = "spearman"
)

cor_kendall <- cor.test(
  stats_equipes_FR$total_momentum,
  stats_equipes_FR$classement_final,
  method = "kendall"
)
cor_spearman
cor_kendall

ggplot(stats_equipes_FR, aes(x = total_momentum, y = classement_final)) +
  geom_point(size = 3, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_y_reverse() +
  labs(
    x = "Total de Momentum",
    y = "Classement final",
    title = "Relation entre momentum et classement final",
    subtitle = paste0("Spearman rho = ", round(-0.486, 2), 
                      ", p-value = ", round(0.0566, 3))
  ) +
  theme_minimal()



