# ============================================================
# DÉTECTION PRÉCOCE D'UNE DOMINATION ADVERSE
# Pré-requis : momentum2.R doit avoir été exécuté (df disponible)
# ============================================================

library(dplyr)
library(slider)    # install.packages("slider") si besoin
library(rpart)
library(rpart.plot)


# ============================================================
# Paramètres (à ajuster)
# ============================================================

N_ACTIONS_AVANT <- 10   # fenêtre rolling pour les features
N_ACTIONS_APRES <- 8    # horizon de prédiction : dans combien d'actions l'alerte se déclenche


# ============================================================
# 1. Indicateurs binaires + rolling features + label alerte
# ============================================================

df_prep <- df %>%
  group_by(CD_MATCH) %>%
  arrange(TS_START_SEQUENCE, .by_group = TRUE) %>%
  mutate(

    # --- début d'une nouvelle domination (changement de CLUB_MOMENTUM) ---
    # domicile commence à dominer
    dom_start_dom = as.integer(
      !is.na(CLUB_MOMENTUM) & CLUB_MOMENTUM == CD_CLUB_DOMICILE &
      (is.na(lag(CLUB_MOMENTUM)) | lag(CLUB_MOMENTUM) != CD_CLUB_DOMICILE)
    ),
    # extérieur commence à dominer
    dom_start_ext = as.integer(
      !is.na(CLUB_MOMENTUM) & CLUB_MOMENTUM == CD_CLUB_EXTERIEUR &
      (is.na(lag(CLUB_MOMENTUM)) | lag(CLUB_MOMENTUM) != CD_CLUB_EXTERIEUR)
    ),

    # --- indicateurs binaires par action ---
    ind_tir_dom    = as.integer(!is.na(ACTION_DOMICILE)  & ACTION_DOMICILE  %in% c("BUT","HORS CADRE","POTEAU","TIR DIFFICILE","TIR CONTRÉ")),
    ind_tir_ext    = as.integer(!is.na(ACTION_EXTERIEUR) & ACTION_EXTERIEUR %in% c("BUT","HORS CADRE","POTEAU","TIR DIFFICILE","TIR CONTRÉ")),
    ind_but_dom    = as.integer(!is.na(ACTION_DOMICILE)  & ACTION_DOMICILE  == "BUT"),
    ind_but_ext    = as.integer(!is.na(ACTION_EXTERIEUR) & ACTION_EXTERIEUR == "BUT"),
    ind_arret_dom  = as.integer(!is.na(ACTION_DOMICILE)  & ACTION_DOMICILE  == "ARRÊT"),
    ind_arret_ext  = as.integer(!is.na(ACTION_EXTERIEUR) & ACTION_EXTERIEUR == "ARRÊT"),
    ind_exclu_dom  = as.integer(!is.na(ACTION_DOMICILE)  & ACTION_DOMICILE  == "2 MINUTES"),
    ind_exclu_ext  = as.integer(!is.na(ACTION_EXTERIEUR) & ACTION_EXTERIEUR == "2 MINUTES"),

    # --- rolling sums sur les N_ACTIONS_AVANT actions précédentes ---
    ROLL_TIR_DOM    = slide_sum(ind_tir_dom,   before = N_ACTIONS_AVANT, after = -1),
    ROLL_TIR_EXT    = slide_sum(ind_tir_ext,   before = N_ACTIONS_AVANT, after = -1),
    ROLL_BUT_DOM    = slide_sum(ind_but_dom,   before = N_ACTIONS_AVANT, after = -1),
    ROLL_BUT_EXT    = slide_sum(ind_but_ext,   before = N_ACTIONS_AVANT, after = -1),
    ROLL_ARRET_DOM  = slide_sum(ind_arret_dom, before = N_ACTIONS_AVANT, after = -1),
    ROLL_ARRET_EXT  = slide_sum(ind_arret_ext, before = N_ACTIONS_AVANT, after = -1),
    ROLL_EXCLU_DOM  = slide_sum(ind_exclu_dom, before = N_ACTIONS_AVANT, after = -1),
    ROLL_EXCLU_EXT  = slide_sum(ind_exclu_ext, before = N_ACTIONS_AVANT, after = -1),

    # --- label alerte (dans les N_ACTIONS_APRES prochaines actions, une domination adverse démarre-t-elle ?) ---
    # du point de vue domicile : l'extérieur va-t-il commencer à dominer ?
    ALERTE_DOM = as.integer(slide_sum(dom_start_ext, before = -1, after = N_ACTIONS_APRES) > 0),
    # du point de vue extérieur : le domicile va-t-il commencer à dominer ?
    ALERTE_EXT = as.integer(slide_sum(dom_start_dom, before = -1, after = N_ACTIONS_APRES) > 0),

    # écart actuel (du point de vue domicile)
    ECART_DOM = NB_SCORE_DOMICILE - NB_SCORE_EXTERIEUR,

    # domination en cours
    DOM_EN_COURS_DOM = as.integer(!is.na(CLUB_MOMENTUM) & CLUB_MOMENTUM == CD_CLUB_DOMICILE),
    DOM_EN_COURS_EXT = as.integer(!is.na(CLUB_MOMENTUM) & CLUB_MOMENTUM == CD_CLUB_EXTERIEUR)

  ) %>%
  ungroup()


# ============================================================
# 2. Table unifiée (une ligne = un moment du match, une équipe)
# ============================================================

# Perspective domicile
df_dom <- df_prep %>%
  transmute(
    CD_MATCH, TS_START_SEQUENCE,
    EQUIPE               = "DOMICILE",
    ALERTE               = ALERTE_DOM,
    ECART                = ECART_DOM,
    DOM_PROPRE_EN_COURS  = DOM_EN_COURS_DOM,
    DOM_ADVERSE_EN_COURS = DOM_EN_COURS_EXT,
    ROLL_TIR_PROPRE      = ROLL_TIR_DOM,
    ROLL_TIR_ADVERSE     = ROLL_TIR_EXT,
    ROLL_BUT_PROPRE      = ROLL_BUT_DOM,
    ROLL_BUT_ADVERSE     = ROLL_BUT_EXT,
    ROLL_ARRET_PROPRE    = ROLL_ARRET_DOM,
    ROLL_ARRET_ADVERSE   = ROLL_ARRET_EXT,
    ROLL_EXCLU_PROPRE    = ROLL_EXCLU_DOM,
    ROLL_EXCLU_ADVERSE   = ROLL_EXCLU_EXT
  )

# Perspective extérieur
df_ext <- df_prep %>%
  transmute(
    CD_MATCH, TS_START_SEQUENCE,
    EQUIPE               = "EXTERIEUR",
    ALERTE               = ALERTE_EXT,
    ECART                = -ECART_DOM,   # inversé : positif = extérieur mène
    DOM_PROPRE_EN_COURS  = DOM_EN_COURS_EXT,
    DOM_ADVERSE_EN_COURS = DOM_EN_COURS_DOM,
    ROLL_TIR_PROPRE      = ROLL_TIR_EXT,
    ROLL_TIR_ADVERSE     = ROLL_TIR_DOM,
    ROLL_BUT_PROPRE      = ROLL_BUT_EXT,
    ROLL_BUT_ADVERSE     = ROLL_BUT_DOM,
    ROLL_ARRET_PROPRE    = ROLL_ARRET_EXT,
    ROLL_ARRET_ADVERSE   = ROLL_ARRET_DOM,
    ROLL_EXCLU_PROPRE    = ROLL_EXCLU_EXT,
    ROLL_EXCLU_ADVERSE   = ROLL_EXCLU_DOM
  )

df_alerte <- bind_rows(df_dom, df_ext) %>%
  filter(!is.na(ALERTE))


# ============================================================
# 3. Statistiques descriptives
# ============================================================

cat("=== Paramètres ===\n")
cat("Fenêtre features :", N_ACTIONS_AVANT, "actions\n")
cat("Horizon alerte   :", N_ACTIONS_APRES, "actions\n\n")

cat("=== Répartition des alertes ===\n")
cat("Total observations  :", nrow(df_alerte), "\n")
cat("Alertes déclenchées :", sum(df_alerte$ALERTE), "\n")
cat("Taux d'alerte       :", round(mean(df_alerte$ALERTE) * 100, 1), "%\n\n")

cat("=== Moyennes des features selon l'alerte ===\n")
df_alerte %>%
  group_by(ALERTE) %>%
  summarise(
    n                    = n(),
    ecart_moyen          = round(mean(ECART,                na.rm = TRUE), 2),
    tir_adverse_moyen    = round(mean(ROLL_TIR_ADVERSE,     na.rm = TRUE), 2),
    but_adverse_moyen    = round(mean(ROLL_BUT_ADVERSE,     na.rm = TRUE), 2),
    arret_propre_moyen   = round(mean(ROLL_ARRET_PROPRE,    na.rm = TRUE), 2),
    exclu_propre_moyen   = round(mean(ROLL_EXCLU_PROPRE,    na.rm = TRUE), 2),
    dom_adverse_pct      = round(mean(DOM_ADVERSE_EN_COURS, na.rm = TRUE) * 100, 1)
  ) %>%
  print()


# ============================================================
# 4. Régression logistique
# ============================================================

modele_logit <- glm(
  ALERTE ~ ECART + ROLL_TIR_ADVERSE + ROLL_BUT_ADVERSE +
            ROLL_ARRET_PROPRE + ROLL_EXCLU_PROPRE + DOM_ADVERSE_EN_COURS,
  data   = df_alerte,
  family = binomial
)

cat("\n=== Régression logistique ===\n")
print(summary(modele_logit))


# ============================================================
# 5. Arbre de décision (règles interprétables pour entraîneurs)
# ============================================================

arbre <- rpart(
  ALERTE ~ ECART + ROLL_TIR_ADVERSE + ROLL_BUT_ADVERSE +
            ROLL_ARRET_PROPRE + ROLL_EXCLU_PROPRE + DOM_ADVERSE_EN_COURS,
  data    = df_alerte,
  method  = "class",
  parms   = list(prior = c(0.5, 0.5)),
  control = rpart.control(cp = 0.001, minsplit = 20, maxdepth = 4)
)

cat("\n=== Règles de l'arbre de décision ===\n")
print(arbre)

rpart.plot(
  arbre,
  type  = 4,
  extra = 104,
  main  = paste0("Détection précoce d'une domination adverse\n(horizon = ",
                 N_ACTIONS_APRES, " actions)")
)


# ============================================================
# 6. Score de danger en temps réel (optionnel)
# ============================================================
# Ajoute une probabilité de domination adverse imminente sur chaque ligne de df

df_alerte$PROBA_ALERTE <- predict(modele_logit, newdata = df_alerte, type = "response")

cat("\n=== Distribution du score de danger ===\n")
cat("Quand ALERTE = 0 : probabilité moyenne =",
    round(mean(df_alerte$PROBA_ALERTE[df_alerte$ALERTE == 0], na.rm = TRUE), 3), "\n")
cat("Quand ALERTE = 1 : probabilité moyenne =",
    round(mean(df_alerte$PROBA_ALERTE[df_alerte$ALERTE == 1], na.rm = TRUE), 3), "\n")
