# ============================================================
# 1️⃣ LIBRAIRIES
# ============================================================

library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(data.table)

# ============================================================
# 6️⃣ VISUALISATION 1 – ACTIONS CRÉATRICES
# ============================================================

############### PLOT 1 Barre empilée ##################

# Préparer les données pour le plot
df_plot <- df_4_4 %>%
  filter(!is.na(ACTION_CREA_1)) %>%
  count(ACTION_CREA_1, ACTION_CREA_2)

# Plot
ggplot(df_plot, aes(x = ACTION_CREA_1, y = n, fill = ACTION_CREA_2)) +
  geom_col(position = "stack") +   # barres empilées par ACTION_CREA_2
  labs(
    x = "Action créatrice 1",
    y = "Nombre d'occurrences",
    fill = "Action créatrice 2"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############### PLOT 2 Barre pas emiplé ##################

# Préparer les données pour le plot
df_plot <- df_4_4 %>%
  filter(!is.na(ACTION_CREA_1)) %>%
  count(ACTION_CREA_1, ACTION_CREA_2)

# Plot barres côte-à-côte
ggplot(df_plot, aes(x = ACTION_CREA_1, y = n, fill = ACTION_CREA_2)) +
  geom_col(position = "dodge") +   # barres côte-à-côte
  labs(
    x = "Action créatrice 1",
    y = "Nombre d'occurrences",
    fill = "Action créatrice 2"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############### PLOT 3 Barre LB_2 en fréquence##################

# Préparer les données pour le plot
df_plot <- df_4_4 %>%
  filter(!is.na(ACTION_CREA_2)) %>%
  count(ACTION_CREA_2)

# Barplot simple
ggplot(df_plot, aes(x = ACTION_CREA_2, y = n, fill = ACTION_CREA_2)) +
  geom_col(show.legend = FALSE) +   # pas de légende si ce n'est pas nécessaire
  labs(
    x = "Action créatrice 2 (LB_2)",
    y = "Nombre d'occurrences"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############### PLOT 4 Barre LB_2 en fréquence##################

# Préparer les données pour le plot
df_plot <- df_4_4 %>%
  filter(!is.na(ACTION_CREA_2)) %>%
  count(ACTION_CREA_2) %>%
  mutate(proportion = n / sum(n))   # calcul de la proportion

# Barplot en proportion
ggplot(df_plot, aes(x = ACTION_CREA_2, y = proportion, fill = ACTION_CREA_2)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # axe y en %
  labs(
    x = "Action créatrice 2 (LB_2)",
    y = "Proportion (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# ============================================================
# 7️⃣ VISUALISATION 2 – CHANGEMENTS DE MOMENTUM
# ============================================================

changement_par_match <- df_4_4 %>%
  filter(!is.na(MOMENTUM), MOMENTUM != "NEUTRE") %>%
  group_by(CD_MATCH) %>%
  arrange(TS_START_SEQUENCE, .by_group = TRUE) %>%
  summarise(
    NB_CHANGEMENTS_MOMENTUM = sum(MOMENTUM != lag(MOMENTUM), na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(changement_par_match,
       aes(x = NB_CHANGEMENTS_MOMENTUM)) +
  geom_histogram(binwidth = 1) +
  labs(
    x = "Nombre de changements de momentum",
    y = "Nombre de matchs",
    title = "Distribution des changements de momentum"
  ) +
  theme_minimal()


# ============================================================
# FONCTION : VISUALISATION TEMPORELLE DU MOMENTUM
# ============================================================
#
# Objectif :
# Produire le graphique de l'évolution de l'état de forme
# lissé dans le temps pour un match donné.
#
# Paramètres :
# - data     : dataframe contenant au minimum
#              CD_MATCH, TS_START_SEQUENCE,
#              ETAT_DE_FORME, ACTION_CREATRICE
# - match_id : identifiant du match à visualiser
# - span     : paramètre de lissage LOESS (défaut = 0.15)
#
# Sortie :
# - Un objet ggplot
# ============================================================

plot_momentum_match <- function(data, match_id, span = 0.15) {
  
  # ----------------------------------------------------------
  # 1️⃣ Vérification que le match existe
  # ----------------------------------------------------------
  
  if (!match_id %in% data$CD_MATCH) {
    stop("Le match demandé n'existe pas dans le dataframe.")
  }
  
  # ----------------------------------------------------------
  # 2️⃣ Fonction interne : conversion du temps en minutes
  # ----------------------------------------------------------
  
  convert_to_minutes <- function(x) {
    parts <- stringr::str_split(x, ":", simplify = TRUE)
    as.numeric(parts[,1]) +
      as.numeric(parts[,2]) / 60 +
      as.numeric(parts[,3]) / 60000
  }
  
  # ----------------------------------------------------------
  # 3️⃣ Filtrage du match et préparation des données
  # ----------------------------------------------------------
  
  df_match <- data %>%
    filter(CD_MATCH == match_id) %>%
    mutate(
      T_START_MIN = convert_to_minutes(TS_START_SEQUENCE)
    ) %>%
    arrange(T_START_MIN)
  
  # ----------------------------------------------------------
  # 4️⃣ Lissage LOESS
  # ----------------------------------------------------------
  
  df_match <- df_match %>%
    mutate(
      ETAT_LISSE = predict(
        loess(ETAT_DE_FORME ~ T_START_MIN, span = span),
        newdata = T_START_MIN
      ),
      etat_sup = ifelse(ETAT_LISSE > 0.5, ETAT_LISSE, NA),
      etat_inf = ifelse(ETAT_LISSE < 0.5, ETAT_LISSE, NA)
    )
  
  # Actions créatrices uniquement
  df_actions <- df_match %>%
    filter(!is.na(ACTION_CREA_2))
  
  # ----------------------------------------------------------
  # 5️⃣ Construction du graphique
  # ----------------------------------------------------------
  
  p <- ggplot(df_match, aes(x = T_START_MIN)) +
    
    # Zone positive
    geom_ribbon(
      aes(ymin = 0.5, ymax = etat_sup),
      fill = "blue",
      alpha = 0.3
    ) +
    
    # Zone négative
    geom_ribbon(
      aes(ymin = etat_inf, ymax = 0.5),
      fill = "red",
      alpha = 0.3
    ) +
    
    # Courbe lissée
    geom_line(
      aes(y = ETAT_LISSE),
      linewidth = 1
    ) +
    
    # Ligne seuil
    geom_hline(
      yintercept = 0.5,
      color = "black",
      linewidth = 0.8,
      linetype = "dashed"
    ) +
    
    # Points des actions créatrices
    geom_point(
      data = df_actions,
      aes(y = ETAT_LISSE),
      size = 2
    ) +
    
    # Labels
    geom_text(
      data = df_actions,
      aes(y = ETAT_LISSE,
          label = ACTION_CREA_2),
      vjust = -0.8,
      size = 3,
      check_overlap = TRUE
    ) +
    
    scale_x_continuous(limits = c(0, 60)) +
    
    labs(
      x = "Temps (minutes)",
      y = "État de forme (lissé)",
      title = paste(
        "Évolution de l'état de forme et actions créatrices – Match",
        match_id
      )
    ) +
    
    theme_minimal()
  
  return(p)
}


plot_momentum_match(df_4_4, "EURO-H_2022_1_1")

plot_momentum_match(df_3_4, "EURO-H_2022_1_1")


# =======================
# Compter les momentums
# =======================


library(dplyr)
library(data.table)
library(tidyr)

compter_momentum_et_resultat <- function(df) {
  
  # 1️⃣ Compter les blocs de momentum
  momentum_counts <- df %>%
    arrange(CD_MATCH, POINTS_TOTAL) %>%
    group_by(CD_MATCH) %>%
    mutate(id_sequence = data.table::rleid(MOMENTUM)) %>%
    ungroup() %>%
    filter(MOMENTUM != "NEUTRE") %>%
    distinct(CD_MATCH, id_sequence, MOMENTUM) %>%
    count(CD_MATCH, MOMENTUM, name = "nb_momentums")
  
  # 2️⃣ Créer la base avec exactement les 2 équipes par match
  equipes_match <- df %>%
    distinct(CD_MATCH, CD_CLUB_DOMICILE, CD_CLUB_EXTERIEUR) %>%
    pivot_longer(
      cols = c(CD_CLUB_DOMICILE, CD_CLUB_EXTERIEUR),
      names_to = NULL,
      values_to = "MOMENTUM"
    )
  
  # 3️⃣ Joindre les comptes de momentum et compléter les NA par 0
  df_momentum_final <- equipes_match %>%
    left_join(momentum_counts, by = c("CD_MATCH", "MOMENTUM")) %>%
    mutate(nb_momentums = replace_na(nb_momentums, 0))
  
  # 4️⃣ Ajouter la colonne GAGNE (TRUE si l'équipe a gagné)
  df_momentum_final <- df_momentum_final %>%
    left_join(
      df %>%
        group_by(CD_MATCH) %>%
        slice_max(order_by = POINTS_TOTAL, n = 1, with_ties = FALSE) %>%
        mutate(
          WINNER = case_when(
            NB_SCORE_DOMICILE > NB_SCORE_EXTERIEUR ~ CD_CLUB_DOMICILE,
            NB_SCORE_EXTERIEUR > NB_SCORE_DOMICILE ~ CD_CLUB_EXTERIEUR,
            TRUE ~ NA_character_  # match nul
          )
        ) %>%
        select(CD_MATCH, WINNER),
      by = "CD_MATCH"
    ) %>%
    mutate(GAGNE = MOMENTUM == WINNER)
  
  # Retourner le dataframe final
  return(df_momentum_final)
}


scores_finaux <- df_4_4 %>%
  group_by(CD_MATCH) %>%
  slice_max(order_by = POINTS_TOTAL, n = 1, with_ties = FALSE) %>%  # dernière ligne du match
  select(CD_MATCH, NB_SCORE_DOMICILE, NB_SCORE_EXTERIEUR, CD_CLUB_DOMICILE, CD_CLUB_EXTERIEUR) %>%
  ungroup()

df_voir <- compter_momentum_et_resultat(df_4_4)

df_voir <- df_voir %>%
  left_join(scores_finaux, by = "CD_MATCH") %>%
  mutate(
    ECART_POINTS = if_else(
      MOMENTUM == CD_CLUB_DOMICILE,
      NB_SCORE_DOMICILE - NB_SCORE_EXTERIEUR,
      NB_SCORE_EXTERIEUR - NB_SCORE_DOMICILE
    )
  ) %>%
  select(-CD_CLUB_DOMICILE, -CD_CLUB_EXTERIEUR, -NB_SCORE_DOMICILE, -NB_SCORE_EXTERIEUR)  # facultatif, pour ne pas garder les colonnes en plus
 
 