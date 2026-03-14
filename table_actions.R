# ============================================================
# 1. LIBRAIRIES
# ============================================================

library(readr)
library(dplyr)
library(lubridate)
library(stringr)

# ============================================================
# 2. IMPORTATION DES DONNÉES
# ============================================================

df_match_details <- read_csv("data/FCT_MATCH_DETAILS_202109242114.csv")
df_match_info    <- read_csv("data/DIM_MATCH_202109242114.csv")

# ============================================================
# 3. CALCUL DU SCORE DE DIFFICULTÉ (depuis momentum2.R)
# ============================================================

df_match_details <- df_match_details %>%
  filter(str_starts(CD_MATCH, "FR")) %>%
  mutate(
    X_RESULTAT = as.numeric(X_RESULTAT),
    Y_RESULTAT = as.numeric(Y_RESULTAT),

    score_difficulte = 0,
    score_difficulte = score_difficulte + ifelse(FG_ATTAQUE_BUT_VIDE == 0, 2, 0),
    score_difficulte = score_difficulte + ifelse(LB_RESULTAT_SECTEUR == "GARDIEN", 4, 0),
    score_difficulte = score_difficulte + ifelse(LB_RESULTAT_SECTEUR == "KUNG-FU", 4, 0),
    score_difficulte = score_difficulte + ifelse(LB_RESULTAT_SECTEUR_DETAIL %in% c("AILE GAUCHE","AILE DROITE"), 2, 0),
    score_difficulte = score_difficulte + ifelse(LB_RESULTAT_SECTEUR_DETAIL %in% c("9-6 GAUCHE","9-6 DROITE","12-9 GAUCHE","12-9 DROITE"), 1, 0),
    score_difficulte = score_difficulte + ifelse(LB_TIR_CATEGORIE %in% c("REBOND","LOB","ROUCOULETTE"), 2, 0),
    score_difficulte = score_difficulte + ifelse(LB_TIR_POSTURE == "EN SUSPENSION", 1, 0),
    tir_difficile    = ifelse(score_difficulte >= 5 & grepl("BUT", LB_RESULTAT_DETAIL, ignore.case = TRUE), 1, 0)
  )

# ============================================================
# 4. SÉLECTION DES VARIABLES
# ============================================================

df_match_details <- df_match_details %>%
  select(
    CD_MATCH,
    NB_SCORE_DOMICILE,
    NB_SCORE_EXTERIEUR,
    CD_CLUB,
    TS_START_SEQUENCE,
    TS_END_SEQUENCE,
    LB_SEQUENCE_TYPE,
    LB_RESULTAT,
    LB_RESULTAT_DETAIL,
    LB_RESULTAT_SECTEUR,
    tir_difficile
  )

df_match_info <- df_match_info %>%
  select(CD_MATCH, CD_CLUB_DOMICILE, CD_CLUB_EXTERIEUR)

# ============================================================
# 5. FUSION ET ORGANISATION
# ============================================================

df <- df_match_details %>%
  left_join(df_match_info, by = "CD_MATCH") %>%
  arrange(CD_MATCH, TS_START_SEQUENCE) %>%
  group_by(CD_MATCH) %>%
  filter(n() > 1) %>%
  ungroup()

# ============================================================
# 6. CORRECTION DES TEMPS MORTS ET TIR DIFFICILE
# ============================================================

df <- df %>%
  mutate(
    LB_SEQUENCE_TYPE   = if_else(LB_RESULTAT == "TEMPS MORT", "TEMPS MORT", LB_SEQUENCE_TYPE),
    LB_RESULTAT_DETAIL = if_else(LB_RESULTAT == "TEMPS MORT", "TEMPS MORT", LB_RESULTAT_DETAIL),
    LB_RESULTAT_DETAIL = if_else(tir_difficile == 1, "TIR DIFFICILE", LB_RESULTAT_DETAIL)
  )

# ============================================================
# 7. INDICATEURS DE BASE
# ============================================================

# Les actions défensives (résultat = l'équipe adverse a défendu)
actions_defensives <- c(
  "ARRÊT", "INTERCEPTION", "NEUTRALISATION", "TIR CONTRÉ",
  "2 MINUTES", "CARTON JAUNE", "CARTON ROUGE", "CONTRE", "CARTON BLEU"
)

df <- df %>%
  mutate(
    DUREE             = as.numeric(TS_END_SEQUENCE - TS_START_SEQUENCE, units = "secs"),
    EST_TIR_DIFFICILE = as.integer(tir_difficile == 1),
    # 1 si l'action offensive a abouti à une action défensive de l'adversaire
    EST_ACTION_DEF    = as.integer(LB_RESULTAT_DETAIL %in% actions_defensives),
    EST_TM            = as.integer(LB_RESULTAT == "TEMPS MORT")
  )

# ============================================================
# 8. SCORES OFFENSIF ET DÉFENSIF
# CD_CLUB = équipe qui fait l'action (offensive)
# ============================================================

df <- df %>%
  mutate(
    SCORE_OFF = if_else(CD_CLUB == CD_CLUB_DOMICILE, NB_SCORE_DOMICILE, NB_SCORE_EXTERIEUR),
    SCORE_DEF = if_else(CD_CLUB == CD_CLUB_DOMICILE, NB_SCORE_EXTERIEUR, NB_SCORE_DOMICILE)
  )

# ============================================================
# 9. SCORES 300 SECONDES AVANT ET APRÈS
# ============================================================

df <- df %>%
  mutate(
    T_AV = TS_START_SEQUENCE - 300,
    T_AP = TS_START_SEQUENCE + 300
  )

df <- df %>%
  group_by(CD_MATCH) %>%
  arrange(TS_START_SEQUENCE, .by_group = TRUE) %>%
  mutate(
    # --- 300s avant ---
    SCORE_300_OFF_AV = sapply(1:n(), function(i) {
      j <- which(TS_START_SEQUENCE <= T_AV[i])
      if (length(j) == 0) return(NA_real_)
      j <- max(j)
      est_dom <- !is.na(CD_CLUB[i]) && !is.na(CD_CLUB_DOMICILE[i]) && CD_CLUB[i] == CD_CLUB_DOMICILE[i]
      if (est_dom) NB_SCORE_DOMICILE[j] else NB_SCORE_EXTERIEUR[j]
    }),
    SCORE_300_DEF_AV = sapply(1:n(), function(i) {
      j <- which(TS_START_SEQUENCE <= T_AV[i])
      if (length(j) == 0) return(NA_real_)
      j <- max(j)
      est_dom <- !is.na(CD_CLUB[i]) && !is.na(CD_CLUB_DOMICILE[i]) && CD_CLUB[i] == CD_CLUB_DOMICILE[i]
      if (est_dom) NB_SCORE_EXTERIEUR[j] else NB_SCORE_DOMICILE[j]
    }),
    # --- 300s après ---
    SCORE_300_OFF_AP = sapply(1:n(), function(i) {
      j <- which(TS_START_SEQUENCE >= T_AP[i])
      if (length(j) == 0) return(NA_real_)
      j <- min(j)
      est_dom <- !is.na(CD_CLUB[i]) && !is.na(CD_CLUB_DOMICILE[i]) && CD_CLUB[i] == CD_CLUB_DOMICILE[i]
      if (est_dom) NB_SCORE_DOMICILE[j] else NB_SCORE_EXTERIEUR[j]
    }),
    SCORE_300_DEF_AP = sapply(1:n(), function(i) {
      j <- which(TS_START_SEQUENCE >= T_AP[i])
      if (length(j) == 0) return(NA_real_)
      j <- min(j)
      est_dom <- !is.na(CD_CLUB[i]) && !is.na(CD_CLUB_DOMICILE[i]) && CD_CLUB[i] == CD_CLUB_DOMICILE[i]
      if (est_dom) NB_SCORE_EXTERIEUR[j] else NB_SCORE_DOMICILE[j]
    })
  ) %>%
  ungroup()

# ============================================================
# 10. ETAT_DE_FORME
# = points marqués par l'équipe offensive sur les 4 derniers points / 4
# ============================================================

df <- df %>%
  mutate(
    POINTS_TOTAL = NB_SCORE_DOMICILE + NB_SCORE_EXTERIEUR
  ) %>%
  group_by(CD_MATCH) %>%
  arrange(POINTS_TOTAL, .by_group = TRUE) %>%
  mutate(
    ETAT_DE_FORME = sapply(1:n(), function(i) {
      pt <- POINTS_TOTAL[i]
      if (pt < 4) return(NA_real_)
      pt_ref <- pt - 4
      j <- which(POINTS_TOTAL == pt_ref)
      if (length(j) == 0) return(NA_real_)
      j <- max(j)
      est_dom <- !is.na(CD_CLUB[i]) && !is.na(CD_CLUB_DOMICILE[i]) && CD_CLUB[i] == CD_CLUB_DOMICILE[i]
      score_off_now <- if (est_dom) NB_SCORE_DOMICILE[i] else NB_SCORE_EXTERIEUR[i]
      score_off_ref <- if (est_dom) NB_SCORE_DOMICILE[j] else NB_SCORE_EXTERIEUR[j]
      (score_off_now - score_off_ref) / 4
    })
  ) %>%
  ungroup()

# ============================================================
# 11. TABLE FINALE
# ============================================================

table_actions <- df %>%
  select(
    CD_MATCH,
    CD_CLUB,
    TS_START_SEQUENCE,
    TS_END_SEQUENCE,
    DUREE,
    EST_TIR_DIFFICILE,
    EST_ACTION_DEF,
    LB_SEQUENCE_TYPE,
    LB_RESULTAT_DETAIL,
    LB_RESULTAT_SECTEUR,
    EST_TM,
    SCORE_OFF,
    SCORE_DEF,
    SCORE_300_OFF_AV,
    SCORE_300_DEF_AV,
    SCORE_300_OFF_AP,
    SCORE_300_DEF_AP,
    ETAT_DE_FORME
  )

write.csv(table_actions, file = "data/table_actions.csv", row.names = FALSE)
