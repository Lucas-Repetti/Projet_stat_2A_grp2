# ============================================================
# markov_momentum.R
# Matrices de transition du momentum (chaîne de Markov)
# ============================================================

library(readr)
library(dplyr)
library(data.table)
library(stringr)
library(tidyr)

# ============================================================
# 1. DONNÉES ET PRÉPARATION
# ============================================================

df1 <- read_csv("data/FCT_MATCH_DETAILS_202109242114.csv")
df2 <- read_csv("data/DIM_MATCH_202109242114.csv")

df1 <- df1 %>%
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
  ) %>%
  select(
    CD_MATCH, NB_SCORE_DOMICILE, NB_SCORE_EXTERIEUR, CD_CLUB,
    TS_START_SEQUENCE, LB_RESULTAT, LB_RESULTAT_DETAIL, tir_difficile
  )

df2 <- df2 %>% select(CD_MATCH, CD_CLUB_DOMICILE, CD_CLUB_EXTERIEUR)

df <- df1 %>%
  left_join(df2, by = "CD_MATCH") %>%
  mutate(
    LB_RESULTAT_DETAIL = if_else(LB_RESULTAT == "TEMPS MORT", "TEMPS MORT", LB_RESULTAT_DETAIL),
    LB_RESULTAT_DETAIL = if_else(tir_difficile == 1, "TIR DIFFICILE", LB_RESULTAT_DETAIL),
    ECART_POINT        = NB_SCORE_DOMICILE - NB_SCORE_EXTERIEUR,
    POINTS_TOTAL       = NB_SCORE_DOMICILE + NB_SCORE_EXTERIEUR,
    CD_CLUB_OTHER      = if_else(CD_CLUB == CD_CLUB_DOMICILE, CD_CLUB_EXTERIEUR, CD_CLUB_DOMICILE)
  ) %>%
  arrange(CD_MATCH, TS_START_SEQUENCE) %>%
  group_by(CD_MATCH) %>%
  filter(n() > 1) %>%
  ungroup()

# ============================================================
# 2. CALCUL DU MOMENTUM (paramètres 4,4 — référence)
# ============================================================

points_mis        <- 4
profondeur_points <- 4
seuil             <- points_mis / profondeur_points

df <- df %>%
  arrange(CD_MATCH, POINTS_TOTAL) %>%
  group_by(CD_MATCH) %>%
  mutate(
    ETAT_DE_FORME = if_else(
      POINTS_TOTAL <= profondeur_points,
      (ECART_POINT + POINTS_TOTAL) / (POINTS_TOTAL * 2),
      (ECART_POINT - ECART_POINT[match(POINTS_TOTAL - profondeur_points, POINTS_TOTAL)] + profondeur_points) / (profondeur_points * 2)
    ),
    MOMENTUM = case_when(
      ETAT_DE_FORME >= seuil       ~ as.character(CD_CLUB_DOMICILE),
      ETAT_DE_FORME <= (1 - seuil) ~ as.character(CD_CLUB_EXTERIEUR),
      TRUE                         ~ "NEUTRE"
    )
  ) %>%
  mutate(debut_bloc = MOMENTUM != "NEUTRE" & lag(MOMENTUM, default = "NEUTRE") == "NEUTRE") %>%
  mutate(
    MOMENTUM = {
      mom <- MOMENTUM; pts <- POINTS_TOTAL
      for (i in which(debut_bloc)) {
        borne_inf <- pts[i] - profondeur_points + 1
        idx <- which(pts >= borne_inf & pts < pts[i] & mom == "NEUTRE")
        mom[idx] <- mom[i]
      }
      mom
    }
  ) %>%
  mutate(MOMENTUM = if_else(POINTS_TOTAL < profondeur_points, "NEUTRE", MOMENTUM)) %>%
  ungroup() %>%
  select(-debut_bloc)

# ============================================================
# 3. ÉTAT DE MARKOV (référentiel fixe domicile/extérieur)
#
#   NEUTRE        → aucune équipe n'a le momentum
#   MOMENTUM_DOM  → l'équipe domicile a le momentum
#   MOMENTUM_EXT  → l'équipe extérieure a le momentum
# ============================================================

df <- df %>%
  mutate(
    ETAT = case_when(
      is.na(MOMENTUM) | MOMENTUM == "NEUTRE"    ~ "NEUTRE",
      MOMENTUM == as.character(CD_CLUB_DOMICILE) ~ "MOMENTUM_DOM",
      MOMENTUM == as.character(CD_CLUB_EXTERIEUR) ~ "MOMENTUM_EXT",
      TRUE                                        ~ "NEUTRE"
    )
  )

# ============================================================
# 4. TABLE DES TRANSITIONS (état[i] → état[i+1])
# ============================================================

etats <- c("NEUTRE", "MOMENTUM_DOM", "MOMENTUM_EXT")

df_transitions <- df %>%
  arrange(CD_MATCH, TS_START_SEQUENCE) %>%
  group_by(CD_MATCH) %>%
  mutate(ETAT_SUIV = lead(ETAT)) %>%
  filter(!is.na(ETAT_SUIV)) %>%
  ungroup()

# ============================================================
# 5. FONCTION : matrice de transition à partir d'une sous-table
# ============================================================

matrice_transition <- function(data) {
  tab <- table(
    factor(data$ETAT,      levels = etats),
    factor(data$ETAT_SUIV, levels = etats)
  )
  # Éviter division par 0 : si une ligne est vide, laisser NA
  P <- tab / rowSums(tab)
  P[is.nan(P)] <- NA
  P
}

# ============================================================
# 6. MATRICE GLOBALE
# ============================================================

P_global <- matrice_transition(df_transitions)

cat("\n=== Matrice de transition globale ===\n")
cat("Lignes = état avant | Colonnes = état après\n")
cat("DOM/EXT = équipe domicile/extérieure a le momentum\n\n")
print(round(P_global, 3))

# ============================================================
# 7. MATRICES CONDITIONNELLES PAR TYPE D'ACTION
# ============================================================

actions_cles <- c(
  "BUT", "TIR DIFFICILE", "TEMPS MORT",
  "ARRÊT", "INTERCEPTION", "TIR CONTRÉ",
  "2 MINUTES", "HORS CADRE", "PASSE INCOMPLETE",
  "FAUTE OFFENSIVE", "JEU PASSIF"
)

cat("\n=== Matrices conditionnelles par type d'action ===\n")

resultats_par_action <- lapply(actions_cles, function(action) {
  sous_df <- df_transitions %>% filter(LB_RESULTAT_DETAIL == action)
  n <- nrow(sous_df)
  if (n < 10) {
    cat(sprintf("\n--- %s : trop peu d'observations (%d) ---\n", action, n))
    return(NULL)
  }
  P <- matrice_transition(sous_df)
  cat(sprintf("\n--- %s (n = %d) ---\n", action, n))
  print(round(P, 3))
  list(action = action, matrice = P, n = n)
})
names(resultats_par_action) <- actions_cles

# ============================================================
# 8. DISTRIBUTION STATIONNAIRE
# Vecteur propre gauche associé à la valeur propre 1 de P
# π tel que π·P = π  →  interprétation : % du temps dans chaque état
# ============================================================

stationnaire <- function(P) {
  # Retirer les lignes/colonnes avec NA
  P_complete <- P
  P_complete[is.na(P_complete)] <- 0
  vals <- eigen(t(P_complete))
  idx  <- which.min(abs(Re(vals$values) - 1))
  v    <- Re(vals$vectors[, idx])
  if (sum(v) < 0) v <- -v   # s'assurer que le vecteur est positif
  v / sum(v)
}

pi_global <- stationnaire(P_global)
names(pi_global) <- etats

cat("\n=== Distribution stationnaire (globale) ===\n")
cat("% du temps passé dans chaque état à l'équilibre :\n")
print(round(pi_global * 100, 1))

# ============================================================
# 9. TEMPS MOYEN DE RETOUR DANS CHAQUE ÉTAT
# E[retour en état s] = 1 / π[s]  (en nombre d'actions)
# ============================================================

cat("\n=== Temps moyen de retour dans chaque état (nb d'actions) ===\n")
print(round(1 / pi_global, 1))

# ============================================================
# 10. RÉSUMÉ : IMPACT DE CHAQUE ACTION SUR LE MOMENTUM
#
# Pour chaque action, on compare P(gagner momentum | état = NEUTRE)
# entre cette action et la matrice globale
# ============================================================

cat("\n=== Impact sur le gain de momentum DOM (depuis état NEUTRE) ===\n")
cat("P(MOMENTUM_DOM après) - P(MOMENTUM_DOM globale depuis NEUTRE)\n")
cat("Un DELTA positif = l'action favorise le momentum domicile\n\n")

p_ref <- P_global["NEUTRE", "MOMENTUM_DOM"]

impacts <- lapply(actions_cles, function(nm) {
  res <- resultats_par_action[[nm]]
  if (is.null(res)) return(NULL)
  p_action <- res$matrice["NEUTRE", "MOMENTUM_DOM"]
  if (is.na(p_action)) return(NULL)
  data.frame(
    ACTION   = nm,
    N        = res$n,
    P_GAIN   = round(p_action, 3),
    DELTA    = round(p_action - p_ref, 3)
  )
})

df_impacts <- do.call(rbind, impacts)
df_impacts <- df_impacts[order(-df_impacts$DELTA), ]
print(df_impacts)

# ============================================================
# 11. EXPORT
# ============================================================

export_transitions <- do.call(rbind, lapply(actions_cles, function(nm) {
  res <- resultats_par_action[[nm]]
  if (is.null(res)) return(NULL)
  as.data.frame(as.table(res$matrice)) %>%
    rename(ETAT_AVANT = Var1, ETAT_APRES = Var2, PROB = Freq) %>%
    mutate(ACTION = nm, N_OBS = res$n)
}))

write.csv(export_transitions, "data/markov_transitions.csv", row.names = FALSE)

export_global <- as.data.frame(as.table(P_global)) %>%
  rename(ETAT_AVANT = Var1, ETAT_APRES = Var2, PROB = Freq) %>%
  mutate(ACTION = "GLOBAL", N_OBS = nrow(df_transitions))

write.csv(
  rbind(export_global, export_transitions),
  "data/markov_transitions_complet.csv",
  row.names = FALSE
)

cat("\nFichiers exportés :\n")
cat("  data/markov_transitions.csv          (par action)\n")
cat("  data/markov_transitions_complet.csv  (global + par action)\n")
