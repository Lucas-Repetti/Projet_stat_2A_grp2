# ============================================================
# stat_desc_momentum_2_2.R
# Statistiques descriptives du momentum – Sections 2.2.1 et 2.2.2
#
# Définition retenue :
#   Une équipe a le momentum si elle a marqué les 3 derniers buts
#   sur les 3 derniers points joués (3 buts consécutifs).
#
# Plan :
#   2.2.1  Fréquence et répartition des momentum
#          P1 – Distribution du nombre de phases par match
#          P2 – Répartition domicile vs extérieur
#          P3 – Durée des phases (en points)
#          P4 – Position temporelle d'apparition dans le match
#
#   2.2.2  Contexte et profil des situations menant à un momentum
#          P5 – Écart de score au déclenchement
#          P6 – Type de séquence : momentum vs neutre
#          P7 – Momentum et résultat du match
# ============================================================

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(stringr)
library(scales)

# ============================================================
# 0. IMPORTATION ET PRÉPARATION DES DONNÉES
# ============================================================

df1 <- read_csv("data/FCT_MATCH_DETAILS_202109242114.csv")
df2 <- read_csv("data/DIM_MATCH_202109242114.csv")
df3 <- read_csv("data/DIM_CLUB_202109242114.csv")

df1 <- df1 %>%
  filter(str_starts(CD_MATCH, "FR")) %>%
  select(CD_MATCH, NB_SCORE_DOMICILE, NB_SCORE_EXTERIEUR, CD_CLUB,
         TS_START_SEQUENCE, LB_RESULTAT, LB_RESULTAT_DETAIL, LB_SEQUENCE_TYPE)

df2 <- df2 %>% select(CD_MATCH, CD_CLUB_DOMICILE, CD_CLUB_EXTERIEUR)
df3 <- df3 %>% select(CD_CLUB, LB_CLUB, LB_VILLE)

df <- df1 %>%
  left_join(df2, by = "CD_MATCH") %>%
  left_join(df3, by = "CD_CLUB") %>%
  mutate(
    ECART_POINT  = NB_SCORE_DOMICILE - NB_SCORE_EXTERIEUR,
    POINTS_TOTAL = NB_SCORE_DOMICILE + NB_SCORE_EXTERIEUR
  ) %>%
  arrange(CD_MATCH, TS_START_SEQUENCE) %>%
  group_by(CD_MATCH) %>%
  filter(n() > 1) %>%
  ungroup()

# ============================================================
# 1. CALCUL DU MOMENTUM (3 buts consécutifs)
# ============================================================

n_mom <- 3

df <- df %>%
  arrange(CD_MATCH, POINTS_TOTAL) %>%
  group_by(CD_MATCH) %>%
  mutate(
    ETAT_DE_FORME = if_else(
      POINTS_TOTAL <= n_mom,
      (ECART_POINT + POINTS_TOTAL) / (POINTS_TOTAL * 2),
      (ECART_POINT - ECART_POINT[match(POINTS_TOTAL - n_mom, POINTS_TOTAL)] + n_mom) / (n_mom * 2)
    ),
    MOMENTUM = case_when(
      ETAT_DE_FORME >= 1 ~ as.character(CD_CLUB_DOMICILE),
      ETAT_DE_FORME <= 0 ~ as.character(CD_CLUB_EXTERIEUR),
      TRUE               ~ "NEUTRE"
    )
  ) %>%
  mutate(debut_bloc = MOMENTUM != "NEUTRE" & lag(MOMENTUM, default = "NEUTRE") == "NEUTRE") %>%
  mutate(
    MOMENTUM = {
      mom <- MOMENTUM; pts <- POINTS_TOTAL
      for (i in which(debut_bloc)) {
        borne_inf <- pts[i] - n_mom + 1
        idx <- which(pts >= borne_inf & pts < pts[i] & mom == "NEUTRE")
        mom[idx] <- mom[i]
      }
      mom
    }
  ) %>%
  mutate(MOMENTUM = if_else(POINTS_TOTAL < n_mom, "NEUTRE", MOMENTUM)) %>%
  ungroup() %>%
  select(-debut_bloc)

# Référentiel fixe domicile / extérieur
df <- df %>%
  mutate(
    ETAT = case_when(
      is.na(MOMENTUM) | MOMENTUM == "NEUTRE"      ~ "NEUTRE",
      MOMENTUM == as.character(CD_CLUB_DOMICILE)  ~ "MOMENTUM_DOM",
      MOMENTUM == as.character(CD_CLUB_EXTERIEUR) ~ "MOMENTUM_EXT",
      TRUE                                         ~ "NEUTRE"
    )
  )

# Résultats finaux par match
scores_finaux <- df %>%
  group_by(CD_MATCH) %>%
  slice_max(order_by = POINTS_TOTAL, n = 1, with_ties = FALSE) %>%
  mutate(
    WINNER_ETAT = case_when(
      NB_SCORE_DOMICILE > NB_SCORE_EXTERIEUR ~ "MOMENTUM_DOM",
      NB_SCORE_EXTERIEUR > NB_SCORE_DOMICILE ~ "MOMENTUM_EXT",
      TRUE                                   ~ "NUL"
    ),
    POINTS_TOTAL_MATCH = POINTS_TOTAL
  ) %>%
  select(CD_MATCH, WINNER_ETAT, POINTS_TOTAL_MATCH) %>%
  ungroup()

# Table des phases de momentum (blocs consécutifs du même état)
phases <- df %>%
  arrange(CD_MATCH, POINTS_TOTAL) %>%
  group_by(CD_MATCH) %>%
  mutate(id_bloc = data.table::rleid(ETAT)) %>%
  ungroup() %>%
  group_by(CD_MATCH, id_bloc, ETAT) %>%
  summarise(
    debut_pts = min(POINTS_TOTAL),
    fin_pts   = max(POINTS_TOTAL),
    duree_pts = n(),
    .groups   = "drop"
  ) %>%
  filter(ETAT != "NEUTRE") %>%
  left_join(scores_finaux, by = "CD_MATCH") %>%
  mutate(moment_relatif = debut_pts / POINTS_TOTAL_MATCH)

# Résumé par match (nombre de phases DOM et EXT)
phases_par_match <- phases %>%
  group_by(CD_MATCH, ETAT) %>%
  summarise(nb_phases = n(), .groups = "drop") %>%
  pivot_wider(names_from = ETAT, values_from = nb_phases, values_fill = 0) %>%
  mutate(nb_phases_total = MOMENTUM_DOM + MOMENTUM_EXT) %>%
  left_join(scores_finaux, by = "CD_MATCH")

cat("=== Données prêtes ===\n")
cat("Matchs Starligue :", n_distinct(df$CD_MATCH), "\n")
cat("Phases de momentum identifiées :", nrow(phases), "\n\n")

# ============================================================
# 2.2.1  FRÉQUENCE ET RÉPARTITION DES MOMENTUM
# ============================================================

cat("============================================================\n")
cat("2.2.1  FRÉQUENCE ET RÉPARTITION DES MOMENTUM\n")
cat("============================================================\n\n")

# ------------------------------------------------------------------
# PLOT 1 – Distribution du nombre total de phases de momentum par match
# ------------------------------------------------------------------

dist_phases <- phases_par_match %>%
  count(nb_phases_total) %>%
  mutate(prop = n / sum(n) * 100)

p1 <- ggplot(dist_phases, aes(x = nb_phases_total, y = prop)) +
  geom_col(fill = "#2171b5", width = 0.7) +
  geom_text(aes(label = paste0(round(prop, 1), "%")),
            vjust = -0.4, size = 3.5, fontface = "bold") +
  scale_x_continuous(breaks = 0:max(dist_phases$nb_phases_total)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    x = "Nombre total de phases de momentum par match",
    y = "Part des matchs (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank())

ggsave("output/desc_2_2_1_dist_phases.png", p1, width = 7, height = 5, dpi = 150)

cat("PLOT 1 – Distribution du nombre de phases de momentum par match\n")
cat("---\n")
cat("Lecture : chaque barre indique la proportion de matchs ayant connu\n")
cat("exactement N phases de momentum (DOM ou EXT confondues).\n")
cat("La distribution montre la fréquence à laquelle le phénomène de\n")
cat("domination successive se produit au cours d'une partie. Une\n")
cat("distribution centrée autour de 3-5 phases indique que les\n")
cat("renversements de momentum sont une réalité structurante du match,\n")
cat("sans pour autant être permanents. Un pic sur de faibles valeurs\n")
cat("signalerait des matchs très équilibrés ; un pic sur de fortes\n")
cat("valeurs révèlerait des matchs aux multiples rebondissements.\n\n")

# ------------------------------------------------------------------
# PLOT 2 – Répartition des phases : domicile vs extérieur
# ------------------------------------------------------------------

repartition_etat <- phases %>%
  count(ETAT) %>%
  mutate(
    prop  = n / sum(n) * 100,
    label = if_else(ETAT == "MOMENTUM_DOM", "Domicile", "Extérieur")
  )

p2 <- ggplot(repartition_etat, aes(x = label, y = prop, fill = label)) +
  geom_col(width = 0.45) +
  geom_text(aes(label = paste0(round(prop, 1), "%")),
            vjust = -0.4, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Domicile" = "#2171b5", "Extérieur" = "#bdd7e7")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), limits = c(0, 100)) +
  labs(x = NULL, y = "Part des phases de momentum (%)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave("output/desc_2_2_1_dom_ext.png", p2, width = 5, height = 5, dpi = 150)

cat("PLOT 2 – Répartition des phases de momentum : domicile vs extérieur\n")
cat("---\n")
cat("Ce graphique mesure si l'avantage à domicile se traduit également\n")
cat("dans la dynamique du momentum. Une part plus élevée en faveur de\n")
cat("l'équipe qui reçoit serait cohérente avec l'effet de soutien du\n")
cat("public, qui peut amplifier l'élan collectif lors de séries de buts.\n")
cat("Un écart prononcé entre les deux barres constituerait un premier\n")
cat("signal que le momentum n'est pas un phénomène neutre vis-à-vis du\n")
cat("statut domicile/extérieur, et devra être pris en compte dans\n")
cat("les analyses ultérieures.\n\n")

# ------------------------------------------------------------------
# PLOT 3 – Distribution de la durée des phases (en nombre de points)
# ------------------------------------------------------------------

p3 <- ggplot(phases, aes(x = duree_pts)) +
  geom_histogram(binwidth = 1, fill = "#2171b5", color = "white", boundary = 0.5) +
  scale_x_continuous(breaks = seq(3, max(phases$duree_pts, na.rm = TRUE), by = 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    x = "Durée de la phase de momentum (nombre de points joués)",
    y = "Nombre de phases"
  ) +
  theme_minimal(base_size = 12)

ggsave("output/desc_2_2_1_duree_phases.png", p3, width = 6, height = 5, dpi = 150)

cat("PLOT 3 – Distribution de la durée des phases de momentum\n")
cat("---\n")
cat("La durée d'une phase est mesurée en nombre de points joués pendant\n")
cat("l'épisode de momentum. Par construction, elle vaut au minimum 3\n")
cat("(les 3 buts consécutifs qui déclenchent le momentum, avec\n")
cat("rétro-propagation aux actions qui les précèdent immédiatement).\n")
cat("La forme de la distribution renseigne sur la résistance des équipes :\n")
cat("une distribution fortement concentrée sur 3-4 points traduit des\n")
cat("épisodes brefs, vite interrompus par un but de l'adversaire ;\n")
cat("une queue longue vers la droite signale des matchs où une équipe\n")
cat("parvient à maintenir sa domination sur une longue séquence,\n")
cat("révélatrice d'une supériorité technique ou physique marquée.\n\n")

# ------------------------------------------------------------------
# PLOT 4 – Position temporelle de l'apparition des phases dans le match
# ------------------------------------------------------------------

p4 <- ggplot(phases, aes(x = moment_relatif, fill = ETAT)) +
  geom_density(alpha = 0.55, color = NA) +
  scale_fill_manual(
    values = c("MOMENTUM_DOM" = "#2171b5", "MOMENTUM_EXT" = "#bdd7e7"),
    labels = c("MOMENTUM_DOM" = "Domicile", "MOMENTUM_EXT" = "Extérieur")
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25)
  ) +
  labs(
    x = "Position dans le match (% du score total atteint)",
    y = "Densité",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

ggsave("output/desc_2_2_1_temporel.png", p4, width = 7, height = 5, dpi = 150)

cat("PLOT 4 – Distribution temporelle de l'apparition des phases de momentum\n")
cat("---\n")
cat("L'axe horizontal représente la position dans le match au moment\n")
cat("où débute chaque phase de momentum, exprimée en proportion du score\n")
cat("total final (0 % = début, 100 % = fin du match). Le choix de\n")
cat("l'échelle en points joués plutôt qu'en secondes écoulées est\n")
cat("délibéré : il reflète la progression effective du jeu. Si les\n")
cat("densités sont élevées en fin de match (au-delà de 70 %), cela\n")
cat("indique que les momentum se cristallisent surtout dans les dernières\n")
cat("minutes, moment où les équipes accélèrent ou tentent de renverser\n")
cat("le score. Une différence de forme entre la courbe DOM et EXT\n")
cat("renseignerait sur des stratégies distinctes : l'équipe extérieure\n")
cat("peut chercher à provoquer un momentum en milieu de match pour\n")
cat("installer un doute, tandis que l'équipe domicile pourrait le\n")
cat("déclencher plutôt en fin de rencontre pour sécuriser le résultat.\n\n")

# ============================================================
# 2.2.2  CONTEXTE ET PROFIL DES SITUATIONS MENANT À UN MOMENTUM
# ============================================================

cat("============================================================\n")
cat("2.2.2  CONTEXTE ET PROFIL DES SITUATIONS MENANT À UN MOMENTUM\n")
cat("============================================================\n\n")

# ------------------------------------------------------------------
# PLOT 5 – Écart de score au moment du déclenchement du momentum
# ------------------------------------------------------------------

# Associer l'écart de score à l'instant de début de chaque phase
debut_phases <- phases %>%
  left_join(
    df %>%
      select(CD_MATCH, POINTS_TOTAL, ECART_POINT) %>%
      distinct(CD_MATCH, POINTS_TOTAL, .keep_all = TRUE),
    by = c("CD_MATCH", "debut_pts" = "POINTS_TOTAL")
  ) %>%
  mutate(
    # Exprimer l'écart du point de vue de l'équipe qui prend le momentum
    ECART_VU_PAR = if_else(ETAT == "MOMENTUM_DOM", ECART_POINT, -ECART_POINT),
    Equipe = if_else(ETAT == "MOMENTUM_DOM", "Domicile", "Extérieur")
  )

p5 <- ggplot(debut_phases, aes(x = Equipe, y = ECART_VU_PAR, fill = Equipe)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.25, width = 0.4) +
  scale_fill_manual(values = c("Domicile" = "#2171b5", "Extérieur" = "#bdd7e7")) +
  labs(
    x = NULL,
    y = "Écart de score (positif = équipe en momentum mène)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave("output/desc_2_2_2_ecart_score.png", p5, width = 5, height = 5, dpi = 150)

cat("PLOT 5 – Écart de score au moment du déclenchement du momentum\n")
cat("---\n")
cat("Ce boxplot présente l'écart de score au point précis où une phase\n")
cat("de momentum commence, exprimé du point de vue de l'équipe qui entre\n")
cat("en momentum (valeur positive = cette équipe est devant au score,\n")
cat("valeur négative = elle est derrière). On cherche ici à comprendre\n")
cat("dans quel contexte scoriel surgit le momentum : est-ce une réaction\n")
cat("de l'équipe en difficulté (médiane négative), ou la confirmation d'une\n")
cat("domination déjà existante (médiane positive) ? Une médiane proche de\n")
cat("zéro indiquerait que les momentum émergent plutôt en situation\n")
cat("d'égalité, lors des duels point-à-point caractéristiques du handball\n")
cat("de haut niveau. Les éventuelles différences entre domicile et extérieur\n")
cat("renseignent sur des dynamiques de jeu spécifiques à chaque statut.\n\n")

# ------------------------------------------------------------------
# PLOT 6 – Type de séquence offensive : phases de momentum vs neutre
# ------------------------------------------------------------------

# Nettoyer les libellés et calculer les proportions par phase
df_types <- df %>%
  filter(!is.na(LB_SEQUENCE_TYPE)) %>%
  mutate(
    Phase = if_else(ETAT == "NEUTRE", "Phase neutre", "Phase de momentum"),
    Type  = str_to_title(str_trim(LB_SEQUENCE_TYPE))
  ) %>%
  group_by(Phase, Type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Phase) %>%
  mutate(prop = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(prop >= 1.5)   # filtrer les modalités marginales

# Tri par fréquence globale
ordre <- df_types %>%
  group_by(Type) %>%
  summarise(n_total = sum(n)) %>%
  arrange(desc(n_total)) %>%
  pull(Type)

df_types <- df_types %>%
  mutate(Type = factor(Type, levels = rev(ordre)))

p6 <- ggplot(df_types, aes(x = Type, y = prop, fill = Phase)) +
  geom_col(position = "dodge", width = 0.6) +
  coord_flip() +
  scale_fill_manual(
    values = c("Phase de momentum" = "#2171b5", "Phase neutre" = "#bdd7e7")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    x = NULL,
    y = "Part des séquences (%)",
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")

ggsave("output/desc_2_2_2_type_sequence.png", p6, width = 7, height = 5, dpi = 150)

cat("PLOT 6 – Types de séquences offensives : phases de momentum vs phases neutres\n")
cat("---\n")
cat("Ce graphique compare la distribution des types de séquences offensives\n")
cat("(attaque placée, contre-attaque, montée de balle, jet de 7m…)\n")
cat("selon que la séquence se déroule pendant une phase de momentum ou\n")
cat("en période neutre. Une sur-représentation de la contre-attaque en\n")
cat("phase de momentum serait cohérente avec l'idée que les séquences\n")
cat("offensives rapides, portées par l'élan collectif, sont un moteur\n")
cat("des séries de buts. À l'inverse, une sur-représentation de\n")
cat("l'attaque placée en période neutre illustrerait la nature plus\n")
cat("posée et équilibrée des phases sans domination marquée. Ce profil\n")
cat("de séquence constitue ainsi un indicateur tactique du contexte\n")
cat("dans lequel les momentum prennent forme.\n\n")

# ------------------------------------------------------------------
# PLOT 7 – Momentum et résultat du match
# ------------------------------------------------------------------

# Taux de victoire de l'équipe domicile selon qui a plus de phases
df_p7 <- phases_par_match %>%
  filter(!is.na(WINNER_ETAT), WINNER_ETAT != "NUL") %>%
  mutate(
    cat_momentum = case_when(
      MOMENTUM_DOM > MOMENTUM_EXT ~ "Dom. a\nplus de phases",
      MOMENTUM_DOM < MOMENTUM_EXT ~ "Ext. a\nplus de phases",
      TRUE                        ~ "Égalité\nde phases"
    ),
    DOM_gagne = (WINNER_ETAT == "MOMENTUM_DOM")
  ) %>%
  group_by(cat_momentum) %>%
  summarise(
    taux_victoire_dom = mean(DOM_gagne, na.rm = TRUE) * 100,
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(cat_momentum = factor(cat_momentum,
    levels = c("Dom. a\nplus de phases", "Égalité\nde phases", "Ext. a\nplus de phases")))

p7 <- ggplot(df_p7, aes(x = cat_momentum, y = taux_victoire_dom, fill = cat_momentum)) +
  geom_col(width = 0.5) +
  geom_text(
    aes(label = paste0(round(taux_victoire_dom, 1), "%\n(n = ", n, ")")),
    vjust = -0.3, size = 3.5, fontface = "bold"
  ) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = c(
    "Dom. a\nplus de phases"  = "#2171b5",
    "Égalité\nde phases"      = "#6baed6",
    "Ext. a\nplus de phases"  = "#bdd7e7"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), limits = c(0, 100)) +
  labs(
    x = NULL,
    y = "Taux de victoire de l'équipe domicile (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave("output/desc_2_2_2_momentum_victoire.png", p7, width = 6, height = 5, dpi = 150)

cat("PLOT 7 – Taux de victoire selon l'avantage en phases de momentum\n")
cat("---\n")
cat("Ce graphique examine la relation entre l'avantage en nombre de phases\n")
cat("de momentum et le résultat du match. Le taux de victoire de l'équipe\n")
cat("domicile est présenté selon trois configurations : elle a eu plus de\n")
cat("phases que son adversaire, autant, ou moins. La ligne pointillée à 50%\n")
cat("matérialise le seuil d'un avantage nul. Si le momentum est un\n")
cat("phénomène pertinent, on doit observer un taux nettement supérieur\n")
cat("à 50 % lorsque l'équipe domicile domine en nombre de phases, et\n")
cat("inférieur dans la situation opposée. Cet indicateur constitue une\n")
cat("première validation descriptive de la valeur prédictive du momentum\n")
cat("avant tout travail de modélisation.\n\n")

# ============================================================
# RÉCAPITULATIF
# ============================================================

cat("============================================================\n")
cat("Graphiques sauvegardés dans output/ :\n")
cat("  Section 2.2.1\n")
cat("    desc_2_2_1_dist_phases.png   – Nb de phases par match\n")
cat("    desc_2_2_1_dom_ext.png       – Domicile vs extérieur\n")
cat("    desc_2_2_1_duree_phases.png  – Durée des phases\n")
cat("    desc_2_2_1_temporel.png      – Position dans le match\n")
cat("  Section 2.2.2\n")
cat("    desc_2_2_2_ecart_score.png      – Écart de score au déclenchement\n")
cat("    desc_2_2_2_type_sequence.png    – Types de séquences\n")
cat("    desc_2_2_2_momentum_victoire.png – Lien momentum-résultat\n")
cat("============================================================\n")
