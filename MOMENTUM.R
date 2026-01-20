library(readr)
library(dplyr)
library(data.table)

# =======================
# LECTURE DES DONNÉES
# =======================

df1 <- read_csv("data/FCT_MATCH_DETAILS_202109242114.csv")
df2 <- read_csv("data/DIM_MATCH_202109242114.csv")

# =======================
# SÉLECTION DES VARIABLES
# =======================

df1 <- df1 %>%
  select(
    CD_MATCH,
    NB_SCORE_DOMICILE,
    NB_SCORE_EXTERIEUR,
    CD_CLUB,
    TS_START_SEQUENCE,
    TS_END_SEQUENCE,
    LB_SEQUENCE_TYPE,
    LB_RESULTAT,
    LB_RESULTAT_DETAIL
  )

df2 <- df2 %>%
  select(
    CD_MATCH,
    CD_CLUB_DOMICILE,
    CD_CLUB_EXTERIEUR
  )

# =======================
# MERGE & VARIABLES SCORE
# =======================

df <- df1 %>%
  left_join(df2, by = "CD_MATCH") %>%
  mutate(
    ECART_POINT = NB_SCORE_DOMICILE - NB_SCORE_EXTERIEUR,
    POINTS_TOTAL = NB_SCORE_DOMICILE + NB_SCORE_EXTERIEUR
  ) %>%
  arrange(CD_MATCH, TS_START_SEQUENCE)

# =======================
# TYPE D'ACTION
# =======================

df <- df %>%
  mutate(
    TYPE_ACTION = case_when(
      LB_RESULTAT %in% c("ARRÊT DU JEU", "NA") ~ "NEUTRE",
      LB_RESULTAT %in% c("TIR", "TEMPS MORT") ~ "POSITIF",
      LB_RESULTAT %in% c("FAUTE", "FAUTE HORS JEU", "PERTE DE BALLE") ~ "NEGATIF",
      TRUE ~ NA_character_
    )
  )

# Inversion domicile / extérieur
df <- df %>%
  mutate(
    TYPE_ACTION = case_when(
      CD_CLUB == CD_CLUB_DOMICILE ~ TYPE_ACTION,
      CD_CLUB == CD_CLUB_EXTERIEUR & TYPE_ACTION == "POSITIF" ~ "NEGATIF",
      CD_CLUB == CD_CLUB_EXTERIEUR & TYPE_ACTION == "NEGATIF" ~ "POSITIF",
      TRUE ~ TYPE_ACTION
    )
  )

# =======================
# CALCUL DU MOMENTUM
# =======================

calcul_momentum <- function(df, points_mis, profondeur_points) {
  
  df %>%
    group_by(CD_MATCH) %>%
    mutate(
      ETAT_DE_FORME = if_else(
        POINTS_TOTAL <= profondeur_points,
        (ECART_POINT + POINTS_TOTAL) / (POINTS_TOTAL * 2),
        (ECART_POINT -
           ECART_POINT[match(POINTS_TOTAL - profondeur_points, POINTS_TOTAL)] +
           profondeur_points) / (profondeur_points * 2)
      ),
      MOMENTUM = case_when(
        ETAT_DE_FORME > (1 - points_mis / profondeur_points) &
          ETAT_DE_FORME < (points_mis / profondeur_points) ~ "NEUTRE",
        ETAT_DE_FORME >= (points_mis / profondeur_points) ~ as.character(CD_CLUB_DOMICILE),
        ETAT_DE_FORME <= (1 - points_mis / profondeur_points) ~ as.character(CD_CLUB_EXTERIEUR)
      )
    ) %>%
    ungroup()
}

df_4_4 <- calcul_momentum(df, points_mis = 4, profondeur_points = 4)

# =======================
# ACTION CRÉATRICE
# =======================

find_action_group <- function(df_group, profondeur, ban = character(0)) {
  
  n <- nrow(df_group)
  action_creatrice <- rep(NA_character_, n)
  
  nouveau_momentum <- !is.na(df_group$MOMENTUM) &
    df_group$MOMENTUM != "NEUTRE" &
    (is.na(lag(df_group$MOMENTUM)) |
       df_group$MOMENTUM != lag(df_group$MOMENTUM))
  
  for (i in seq_len(n)) {
    
    if (!nouveau_momentum[i]) next
    
    if (df_group$MOMENTUM[i] == df_group$CD_CLUB_DOMICILE[i]) {
      types_valides <- c("POSITIF", "NEUTRE")
    } else {
      types_valides <- c("NEGATIF", "NEUTRE")
    }
    
    start <- i - 1
    end <- max(1, i - profondeur)
    
    if (start >= end) {
      for (j in seq(from = start, to = end, by = -1)) {
        if (!is.na(df_group$TYPE_ACTION[j]) &&
            df_group$TYPE_ACTION[j] %in% types_valides &&
            !(df_group$LB_RESULTAT_DETAIL[j] %in% ban)) {
          
          action_creatrice[i] <- df_group$LB_RESULTAT_DETAIL[j]
          break
        }
      }
    }
  }
  
  action_creatrice
}

calcul_action <- function(df, profondeur_action, ban = character(0)) {
  
  df %>%
    group_by(CD_MATCH) %>%
    mutate(
      ACTION_CREATRICE = find_action_group(cur_data(), profondeur_action, ban)
    ) %>%
    ungroup()
}

# =======================
# APPLICATION
# =======================

df_4_4 <- calcul_action(
  df_4_4,
  profondeur_action = 5,
  ban = c("BUT")
)


# =======================
# Barre des répartitions des actions créatrices
# =======================

library(ggplot2)

df_plot <- df_4_4 %>%
  filter(!is.na(ACTION_CREATRICE)) %>%
  count(ACTION_CREATRICE) %>%
  mutate(
    pourcentage = n / sum(n) * 100
  )

ggplot(df_plot, aes(x = ACTION_CREATRICE, y = pourcentage)) +
  geom_col() +
  labs(
    x = "Action créatrice",
    y = "Pourcentage (%)",
    title = "Répartition des actions créatrices"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# =======================
# NOMBRE MOYEN DE CHANGEMENT DE MOMENTUM
# =======================

library(dplyr)

changement_par_match <- df_4_4 %>%
  filter(!is.na(MOMENTUM), MOMENTUM != "NEUTRE") %>%
  group_by(CD_MATCH) %>%
  arrange(TS_START_SEQUENCE, .by_group = TRUE) %>%
  summarise(
    NB_CHANGEMENTS_MOMENTUM = sum(MOMENTUM != lag(MOMENTUM), na.rm = TRUE)
  ) %>%
  ungroup()

moyenne_changements <- changement_par_match %>%
  summarise(
    MOYENNE_CHANGEMENTS_MOMENTUM = mean(NB_CHANGEMENTS_MOMENTUM)
  )

moyenne_changements

ggplot(changement_par_match, aes(x = NB_CHANGEMENTS_MOMENTUM)) +
  geom_histogram(binwidth = 1) +
  labs(
    x = "Nombre de changements de momentum par match",
    y = "Nombre de matchs",
    title = "Distribution des changements de momentum"
  ) +
  theme_minimal()


# =======================
# Barre des répartitions des actions créatrices
# =======================


library(dplyr)
library(ggplot2)
library(stringr)

convert_to_minutes <- function(x) {
  parts <- str_split(x, ":", simplify = TRUE)
  as.numeric(parts[,1]) +           # minutes
    as.numeric(parts[,2]) / 60 +     # secondes
    as.numeric(parts[,3]) / 60000    # millisecondes
}

df_4_4 <- df_4_4 %>%
  mutate(
    T_START_MIN = convert_to_minutes(TS_START_SEQUENCE),
    T_END_MIN   = convert_to_minutes(TS_END_SEQUENCE)
  )

df_match <- df_4_4 %>%
  filter(CD_MATCH == "EURO-H_2022_1_1") %>%
  arrange(T_START_MIN)

ggplot(df_match, aes(x = T_START_MIN, y = ETAT_DE_FORME)) +
  geom_line() +
  scale_x_continuous(limits = c(0, 60)) +
  labs(
    x = "Temps (minutes)",
    y = "État de forme",
    title = paste("Évolution de l'état de forme – Match", unique(df_match$CD_MATCH))
  ) + geom_hline(yintercept = 0.5, color = "red", linewidth = 1)+
  theme_minimal()

df_match_area <- df_match %>%
  arrange(T_START_MIN) %>%
  mutate(
    etat_sup = ifelse(ETAT_DE_FORME > 0.5, ETAT_DE_FORME, NA),
    etat_inf = ifelse(ETAT_DE_FORME < 0.5, ETAT_DE_FORME, NA)
  )

ggplot(df_match_area, aes(x = T_START_MIN)) +
  
  # Aire au-dessus de 0.5 (bleu)
  geom_ribbon(
    aes(ymin = 0.5, ymax = etat_sup),
    fill = "blue",
    alpha = 0.3
  ) +
  
  # Aire en-dessous de 0.5 (rouge)
  geom_ribbon(
    aes(ymin = etat_inf, ymax = 0.5),
    fill = "red",
    alpha = 0.3
  ) +
  
  # Courbe principale
  geom_line(aes(y = ETAT_DE_FORME), linewidth = 1) +
  
  # Ligne seuil
  geom_hline(yintercept = 0.5, color = "red", linewidth = 1) +
  
  scale_x_continuous(limits = c(0, 60)) +
  labs(
    x = "Temps (minutes)",
    y = "État de forme",
    title = paste(
      "Évolution de l'état de forme – Match",
      unique(df_match_area$CD_MATCH)
    )
  ) +
  theme_minimal()
