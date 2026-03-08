library(dplyr)
library(ggplot2)


##### CHOIX MOMENTUM #####

list_stats <- list(
  "stats_equipes_4_0" = stats_equipes_4_0,
  "stats_equipes_3_0" = stats_equipes_3_0,
  "stats_equipes_2_0" = stats_equipes_2_0
)

momentum_desc<-momentum_desc_4_0
#momentum_desc<-momentum_desc_3_0
#momentum_desc<-momentum_desc_2_0



##### PREPA TABLES #####


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
stats_equipes_FR <- stats_equipes %>%
  filter(grepl("^FR", MOMENTUM))  

momentum_desc <- momentum_desc %>%
  filter(grepl("^FR", CD_MATCH)) 

momentum_desc <- momentum_desc %>%
  left_join(classement_2021, by = "MOMENTUM")

stats_equipes_2_0 <- momentum_desc %>%
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





##### PREMIERS CHIFFRES #####


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



# Optionnel : distribution du nombre de momentum
momentum_dist <- momentum_desc %>%
  count(nb_momentums) %>%
  mutate(freq = n / sum(n) * 100)

momentum_dist

# Régression linéaire : taux de victoire ~ total_momentum pour les équipes FR
regression_FR <- lm(taux_victoire ~ total_momentum, data = stats_equipes_FR)

# Résumé de la régression
summary(regression_FR)



##### CHIFFRES CLEES #####

# Somme des momentum par match
momentum_par_match <- momentum_desc_FR %>%
  group_by(CD_MATCH) %>%
  summarise(total_momentum = sum(nb_momentums, na.rm = TRUE),
            .groups = "drop")

# Nombre de matchs avec au moins un momentum
nb_match_avec_momentum <- momentum_par_match %>%
  filter(total_momentum > 0) %>%
  nrow()

# Nombre total de matchs
nb_match_total <- n_distinct(momentum_desc_FR$CD_MATCH)

# Proportion de matchs avec au moins un momentum
proportion_match_momentum <- nb_match_avec_momentum / nb_match_total

# Résultat momentum et match gagné
resultat_momentum <- momentum_desc_FR %>%
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

# Proportion de matchs gagnés par l’équipe avec le plus de momentum
proportion_gagne_plus_momentum <- mean(resultat_momentum$gagne_plus_momentum)

# Somme totale de tous les nb_momentums
somme_total_momentum <- sum(momentum_desc_FR$nb_momentums, na.rm = TRUE)

# Tableau récapitulatif final
tableau_recap_momentum <- tibble(
  nb_match_total = nb_match_total,
  nb_match_avec_momentum = nb_match_avec_momentum,
  proportion_match_avec_momentum = proportion_match_momentum,
  proportion_gagne_plus_momentum = proportion_gagne_plus_momentum,
  somme_total_momentum = somme_total_momentum
)

tableau_recap_momentum






##### RECAP #####

# Liste des dataframes à traiter
list_momentum <- list(
  "momentum_4_0" = momentum_desc_4_0,
  "momentum_3_0" = momentum_desc_3_0,
  "momentum_2_0" = momentum_desc_2_0
)

# Fonction pour créer le résumé d’un dataframe
resumer_momentum <- function(df, nom_table) {
  
  # Somme totale de tous les nb_momentums
  somme_total_momentum <- sum(df$nb_momentums, na.rm = TRUE)
  
  # Somme des momentum par match
  momentum_par_match <- df %>%
    group_by(CD_MATCH) %>%
    summarise(total_momentum = sum(nb_momentums, na.rm = TRUE), .groups = "drop")
  
  nb_match_avec_momentum <- momentum_par_match %>%
    filter(total_momentum > 0) %>%
    nrow()
  
  nb_match_total <- n_distinct(df$CD_MATCH)
  proportion_match_avec_momentum <- nb_match_avec_momentum / nb_match_total
  
  # Résultat momentum et match gagné
  resultat_momentum <- df %>%
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
  
  proportion_gagne_plus_momentum <- mean(resultat_momentum$gagne_plus_momentum)
  
  # Retourner un tibble avec 1 ligne
  tibble(
    table = nom_table,
    nb_match_total = nb_match_total,
    nb_match_avec_momentum = nb_match_avec_momentum,
    proportion_match_avec_momentum = proportion_match_avec_momentum,
    proportion_gagne_plus_momentum = proportion_gagne_plus_momentum,
    somme_total_momentum = somme_total_momentum
  )
}

# Appliquer la fonction sur tous les dataframes et combiner
tableau_recap_global <- bind_rows(
  lapply(names(list_momentum), function(nom) resumer_momentum(list_momentum[[nom]], nom))
)

tableau_recap_global


##### CLASSEMENTS #####


# Liste des tables
list_stats_equipes <- list(
  "stats_equipes_4_0" = stats_equipes_4_0,
  "stats_equipes_3_0" = stats_equipes_3_0,
  "stats_equipes_2_0" = stats_equipes_2_0
)

# Regression simple classement_final ~ total_momentum
regression_global <- lapply(names(list_stats_equipes), function(nom) {
  df <- list_stats_equipes[[nom]]
  fit <- lm(classement_final ~ total_momentum, data = df)
  data.frame(table = nom,
             r_squared = summary(fit)$r.squared,
             slope = coef(fit)[2],
             intercept = coef(fit)[1])
}) %>% bind_rows()

regression_global

# Graphiques pour chaque tableau
lapply(names(list_stats_equipes), function(nom) {
  df <- list_stats_equipes[[nom]]
  ggplot(df, aes(x = total_momentum, y = classement_final)) +
    geom_point(color = "steelblue", size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    ggtitle(paste("Classement final vs Total Momentum:", nom)) +
    xlab("Total Momentum") +
    ylab("Classement final") +
    theme_minimal()
})

