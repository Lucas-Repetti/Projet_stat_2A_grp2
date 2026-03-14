# ============================================================
# ANALYSE DES TEMPS MORTS - Modèle de prédiction
# Pré-requis : momentum2.R doit avoir été exécuté (df disponible)
# ============================================================

library(dplyr)
library(slider)    # install.packages("slider") si besoin
library(rpart)
library(rpart.plot)


# ============================================================
# Paramètres (à ajuster)
# ============================================================

N_ACTIONS_AVANT <- 10   # nb d'actions avant le TM pour les features
N_ACTIONS_APRES <- 5    # nb d'actions après le TM pour évaluer l'efficacité


# ============================================================
# 1. Indicateurs binaires + rolling window sur toutes les actions
# ============================================================

df_prep <- df %>%
  group_by(CD_MATCH) %>%
  arrange(TS_START_SEQUENCE, .by_group = TRUE) %>%
  mutate(

    # --- indicateurs binaires par action ---
    ind_tir_dom    = as.integer(!is.na(ACTION_DOMICILE)  & ACTION_DOMICILE  %in% c("BUT","HORS CADRE","POTEAU","TIR DIFFICILE","TIR CONTRÉ")),
    ind_tir_ext    = as.integer(!is.na(ACTION_EXTERIEUR) & ACTION_EXTERIEUR %in% c("BUT","HORS CADRE","POTEAU","TIR DIFFICILE","TIR CONTRÉ")),
    ind_but_dom    = as.integer(!is.na(ACTION_DOMICILE)  & ACTION_DOMICILE  == "BUT"),
    ind_but_ext    = as.integer(!is.na(ACTION_EXTERIEUR) & ACTION_EXTERIEUR == "BUT"),
    ind_arret_dom  = as.integer(!is.na(ACTION_DOMICILE)  & ACTION_DOMICILE  == "ARRÊT"),
    ind_arret_ext  = as.integer(!is.na(ACTION_EXTERIEUR) & ACTION_EXTERIEUR == "ARRÊT"),
    ind_exclu_dom  = as.integer(!is.na(ACTION_DOMICILE)  & ACTION_DOMICILE  == "2 MINUTES"),
    ind_exclu_ext  = as.integer(!is.na(ACTION_EXTERIEUR) & ACTION_EXTERIEUR == "2 MINUTES"),

    # --- rolling sums sur les N_ACTIONS_AVANT actions précédentes (sans inclure la ligne courante) ---
    ROLL_TIR_DOM    = slide_sum(ind_tir_dom,   before = N_ACTIONS_AVANT, after = -1),
    ROLL_TIR_EXT    = slide_sum(ind_tir_ext,   before = N_ACTIONS_AVANT, after = -1),
    ROLL_BUT_DOM    = slide_sum(ind_but_dom,   before = N_ACTIONS_AVANT, after = -1),
    ROLL_BUT_EXT    = slide_sum(ind_but_ext,   before = N_ACTIONS_AVANT, after = -1),
    ROLL_ARRET_DOM  = slide_sum(ind_arret_dom, before = N_ACTIONS_AVANT, after = -1),
    ROLL_ARRET_EXT  = slide_sum(ind_arret_ext, before = N_ACTIONS_AVANT, after = -1),
    ROLL_EXCLU_DOM  = slide_sum(ind_exclu_dom, before = N_ACTIONS_AVANT, after = -1),
    ROLL_EXCLU_EXT  = slide_sum(ind_exclu_ext, before = N_ACTIONS_AVANT, after = -1),

    # --- score futur (N_ACTIONS_APRES actions plus tard dans le même match) ---
    SCORE_DOM_FUTUR = lead(NB_SCORE_DOMICILE,  N_ACTIONS_APRES),
    SCORE_EXT_FUTUR = lead(NB_SCORE_EXTERIEUR, N_ACTIONS_APRES)

  ) %>%
  ungroup()


# ============================================================
# 2. Filtrer les TM et construire la table d'analyse
# ============================================================

df_tm <- df_prep %>%
  filter(LB_RESULTAT_DETAIL == "TEMPS MORT") %>%
  mutate(

    IS_DOM = (CD_CLUB == CD_CLUB_DOMICILE),

    # Écart au moment du TM (du point de vue de l'équipe qui le prend)
    ECART_ACTUEL = if_else(IS_DOM,
      NB_SCORE_DOMICILE - NB_SCORE_EXTERIEUR,
      NB_SCORE_EXTERIEUR - NB_SCORE_DOMICILE
    ),

    # Écart N_ACTIONS_APRES actions plus tard
    ECART_FUTUR = if_else(IS_DOM,
      SCORE_DOM_FUTUR - SCORE_EXT_FUTUR,
      SCORE_EXT_FUTUR - SCORE_DOM_FUTUR
    ),

    # Label : 1 si l'écart s'est strictement amélioré après le TM
    # (définition stricte : il faut gagner au moins 1 but d'avantage)
    TM_EFFICACE = as.integer(ECART_FUTUR > ECART_ACTUEL),

    # Features orientées "équipe TM" (propre) vs adversaire
    ROLL_TIR_PROPRE     = if_else(IS_DOM, ROLL_TIR_DOM,   ROLL_TIR_EXT),
    ROLL_TIR_ADVERSE    = if_else(IS_DOM, ROLL_TIR_EXT,   ROLL_TIR_DOM),
    ROLL_BUT_PROPRE     = if_else(IS_DOM, ROLL_BUT_DOM,   ROLL_BUT_EXT),
    ROLL_BUT_ADVERSE    = if_else(IS_DOM, ROLL_BUT_EXT,   ROLL_BUT_DOM),
    ROLL_ARRET_PROPRE   = if_else(IS_DOM, ROLL_ARRET_DOM, ROLL_ARRET_EXT),
    ROLL_ARRET_ADVERSE  = if_else(IS_DOM, ROLL_ARRET_EXT, ROLL_ARRET_DOM),
    ROLL_EXCLU_PROPRE   = if_else(IS_DOM, ROLL_EXCLU_DOM, ROLL_EXCLU_EXT),
    ROLL_EXCLU_ADVERSE  = if_else(IS_DOM, ROLL_EXCLU_EXT, ROLL_EXCLU_DOM),

    # Domination adverse en cours au moment du TM
    DOM_ADVERSE_EN_COURS = as.integer(!is.na(CLUB_DOMINANT) & CLUB_DOMINANT != CD_CLUB)

  ) %>%
  filter(!is.na(TM_EFFICACE))


# ============================================================
# 3. Statistiques descriptives
# ============================================================

cat("=== Paramètres utilisés ===\n")
cat("Fenêtre avant (features) :", N_ACTIONS_AVANT, "actions\n")
cat("Fenêtre après (label)    :", N_ACTIONS_APRES, "actions\n\n")

cat("=== Répartition des TM ===\n")
cat("Total TM analysés :", nrow(df_tm), "\n")
cat("TM efficaces      :", sum(df_tm$TM_EFFICACE), "\n")
cat("Taux d'efficacité :", round(mean(df_tm$TM_EFFICACE) * 100, 1), "%\n\n")

cat("=== Moyennes des features selon l'efficacité du TM ===\n")
df_tm %>%
  group_by(TM_EFFICACE) %>%
  summarise(
    n                   = n(),
    ecart_moyen         = round(mean(ECART_ACTUEL,       na.rm = TRUE), 2),
    tir_adverse_moyen   = round(mean(ROLL_TIR_ADVERSE,   na.rm = TRUE), 2),
    but_adverse_moyen   = round(mean(ROLL_BUT_ADVERSE,   na.rm = TRUE), 2),
    arret_propre_moyen  = round(mean(ROLL_ARRET_PROPRE,  na.rm = TRUE), 2),
    exclu_adverse_moyen = round(mean(ROLL_EXCLU_ADVERSE, na.rm = TRUE), 2),
    dom_adverse_pct     = round(mean(DOM_ADVERSE_EN_COURS, na.rm = TRUE) * 100, 1)
  ) %>%
  print()


# ============================================================
# 4. Régression logistique
# ============================================================

modele_logit <- glm(
  TM_EFFICACE ~ ECART_ACTUEL + ROLL_TIR_ADVERSE + ROLL_BUT_ADVERSE +
                ROLL_ARRET_PROPRE + ROLL_EXCLU_ADVERSE + DOM_ADVERSE_EN_COURS,
  data   = df_tm,
  family = binomial
)

cat("\n=== Régression logistique ===\n")
print(summary(modele_logit))


# ============================================================
# 5. Arbre de décision (règles interprétables pour entraîneurs)
# ============================================================

arbre <- rpart(
  TM_EFFICACE ~ ECART_ACTUEL + ROLL_TIR_ADVERSE + ROLL_BUT_ADVERSE +
                ROLL_ARRET_PROPRE + ROLL_EXCLU_ADVERSE + DOM_ADVERSE_EN_COURS,
  data    = df_tm,
  method  = "class",
  parms   = list(prior = c(0.5, 0.5)),   # équilibre artificiel des classes
  control = rpart.control(cp = 0.001, minsplit = 20, maxdepth = 4)
)

cat("\n=== Règles de l'arbre de décision ===\n")
print(arbre)

rpart.plot(
  arbre,
  type  = 4,
  extra = 104,
  main  = paste0("Quand prendre un temps mort ?\n(fenêtre = ", N_ACTIONS_AVANT, " actions)")
)
