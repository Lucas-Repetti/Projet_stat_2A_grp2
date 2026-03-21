# ============================================================
# stat_desc_momentum_2_2.R
# Statistiques descriptives du momentum – Sections 2.2.1 et 2.2.2
#
# Définition retenue :
#   Une équipe est en situation de momentum si elle a marqué
#   les 3 derniers buts consécutifs (sur les 3 derniers points joués).
#
# Plan :
#   2.2.1  Fréquence et répartition des momentum
#          P1 – Distribution du nombre de phases par match
#          P2 – Densité d'apparition sur la durée du match (0-60 min)
#          P3 – Répartition par quart de match (4 × 15 min)
#          P4 – Distribution de la durée des phases (en minutes)
#
#   2.2.2  Contexte et profil des situations menant à un momentum
#          P5 – Visualisation temporelle d'un match : état de forme,
#               momentum et temps morts
#          P6 – Actions précédant les déclenchements de momentum
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
         TS_START_SEQUENCE, TS_END_SEQUENCE,
         LB_RESULTAT, LB_RESULTAT_DETAIL, LB_SEQUENCE_TYPE)

df2 <- df2 %>% select(CD_MATCH, CD_CLUB_DOMICILE, CD_CLUB_EXTERIEUR)
df3 <- df3 %>% select(CD_CLUB, LB_CLUB, LB_VILLE)

# Conversion timestamp → minutes (format MM:SS:00)
convert_to_minutes <- function(x) {
  parts <- str_split(as.character(x), ":", simplify = TRUE)
  suppressWarnings(as.numeric(parts[, 1]) + as.numeric(parts[, 2]) / 60)
}

df <- df1 %>%
  left_join(df2, by = "CD_MATCH") %>%
  left_join(df3, by = "CD_CLUB") %>%
  left_join(
    df3 %>% rename(CD_CLUB_DOMICILE = CD_CLUB,
                   LB_VILLE_DOMICILE = LB_VILLE,
                   LB_CLUB_DOMICILE  = LB_CLUB),
    by = "CD_CLUB_DOMICILE"
  ) %>%
  left_join(
    df3 %>% rename(CD_CLUB_EXTERIEUR = CD_CLUB,
                   LB_VILLE_EXTERIEUR = LB_VILLE,
                   LB_CLUB_EXTERIEUR  = LB_CLUB),
    by = "CD_CLUB_EXTERIEUR"
  ) %>%
  mutate(
    ECART_POINT    = NB_SCORE_DOMICILE - NB_SCORE_EXTERIEUR,
    POINTS_TOTAL   = NB_SCORE_DOMICILE + NB_SCORE_EXTERIEUR,
    T_MIN          = convert_to_minutes(TS_START_SEQUENCE),
    T_MIN_END      = convert_to_minutes(TS_END_SEQUENCE),
    LB_VILLE_OTHER = if_else(LB_VILLE == LB_VILLE_DOMICILE,
                             LB_VILLE_EXTERIEUR, LB_VILLE_DOMICILE)
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
  mutate(
    debut_bloc = MOMENTUM != "NEUTRE" & lag(MOMENTUM, default = "NEUTRE") == "NEUTRE"
  ) %>%
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

# Référentiel fixe DOM / EXT
df <- df %>%
  mutate(
    ETAT = case_when(
      is.na(MOMENTUM) | MOMENTUM == "NEUTRE"      ~ "NEUTRE",
      MOMENTUM == as.character(CD_CLUB_DOMICILE)  ~ "MOMENTUM_DOM",
      MOMENTUM == as.character(CD_CLUB_EXTERIEUR) ~ "MOMENTUM_EXT",
      TRUE                                         ~ "NEUTRE"
    )
  )

# Résultats finaux
scores_finaux <- df %>%
  group_by(CD_MATCH) %>%
  slice_max(POINTS_TOTAL, n = 1, with_ties = FALSE) %>%
  mutate(
    WINNER_ETAT        = case_when(
      NB_SCORE_DOMICILE > NB_SCORE_EXTERIEUR ~ "MOMENTUM_DOM",
      NB_SCORE_EXTERIEUR > NB_SCORE_DOMICILE ~ "MOMENTUM_EXT",
      TRUE                                   ~ "NUL"
    ),
    POINTS_TOTAL_MATCH = POINTS_TOTAL
  ) %>%
  select(CD_MATCH, WINNER_ETAT, POINTS_TOTAL_MATCH) %>%
  ungroup()

# Référence temps min par (match, score)
temps_ref <- df %>%
  filter(!is.na(T_MIN), T_MIN >= 0) %>%
  group_by(CD_MATCH, POINTS_TOTAL) %>%
  summarise(T_MIN_REF = min(T_MIN, na.rm = TRUE), .groups = "drop")

temps_ref_end <- df %>%
  filter(!is.na(T_MIN_END), T_MIN_END >= 0) %>%
  group_by(CD_MATCH, POINTS_TOTAL) %>%
  summarise(T_MIN_END_REF = max(T_MIN_END, na.rm = TRUE), .groups = "drop")

# Table des phases de momentum
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
  left_join(temps_ref     %>% rename(T_MIN_DEBUT  = T_MIN_REF),
            by = c("CD_MATCH", "debut_pts" = "POINTS_TOTAL")) %>%
  left_join(temps_ref_end %>% rename(T_MIN_FIN    = T_MIN_END_REF),
            by = c("CD_MATCH", "fin_pts"   = "POINTS_TOTAL")) %>%
  mutate(
    duree_min      = pmax(T_MIN_FIN - T_MIN_DEBUT, 0),
    moment_relatif = debut_pts / POINTS_TOTAL_MATCH
  ) %>%
  filter(!is.na(T_MIN_DEBUT), T_MIN_DEBUT >= 0, T_MIN_DEBUT <= 65)

# Résumé par match
phases_par_match <- phases %>%
  group_by(CD_MATCH, ETAT) %>%
  summarise(nb_phases = n(), .groups = "drop") %>%
  pivot_wider(names_from = ETAT, values_from = nb_phases, values_fill = 0) %>%
  mutate(
    MOMENTUM_DOM   = if ("MOMENTUM_DOM"   %in% names(.)) MOMENTUM_DOM   else 0L,
    MOMENTUM_EXT   = if ("MOMENTUM_EXT"   %in% names(.)) MOMENTUM_EXT   else 0L,
    nb_phases_total = MOMENTUM_DOM + MOMENTUM_EXT
  ) %>%
  left_join(scores_finaux, by = "CD_MATCH")

cat("=== Données prêtes ===\n")
cat("Matchs Starligue :", n_distinct(df$CD_MATCH), "\n")
cat("Phases de momentum détectées :", nrow(phases), "\n\n")

# ============================================================
# 2.2.1  FRÉQUENCE ET RÉPARTITION DES MOMENTUM
# ============================================================

cat("============================================================\n")
cat("2.2.1  FRÉQUENCE ET RÉPARTITION DES MOMENTUM\n")
cat("============================================================\n\n")

# ------------------------------------------------------------------
# PLOT 1 – Distribution du nombre de phases de momentum par match
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
cat("Chaque barre indique la proportion de matchs ayant connu exactement N\n")
cat("épisodes de momentum (toutes équipes confondues). Ce graphique répond\n")
cat("à la question : le momentum est-il un événement rare ou récurrent\n")
cat("au cours d'une partie de handball ? Si la distribution est centrée\n")
cat("autour de 3 à 5 phases, cela confirme que les renversements de\n")
cat("domination sont une réalité structurante du match – ni anecdotiques,\n")
cat("ni omniprésents. Cette fréquence justifie d'étudier le momentum comme\n")
cat("un indicateur de jeu à part entière plutôt que comme un artefact\n")
cat("statistique.\n\n")

# ------------------------------------------------------------------
# PLOT 2 – Densité d'apparition des phases sur la durée du match (0-60 min)
# ------------------------------------------------------------------

phases_temps <- phases %>%
  filter(!is.na(T_MIN_DEBUT), T_MIN_DEBUT >= 0, T_MIN_DEBUT <= 62)

p2 <- ggplot(phases_temps, aes(x = T_MIN_DEBUT, fill = ETAT)) +
  geom_density(alpha = 0.55, color = NA, bw = 3) +
  scale_fill_manual(
    values = c("MOMENTUM_DOM" = "#2171b5", "MOMENTUM_EXT" = "#6baed6"),
    labels = c("MOMENTUM_DOM" = "Domicile", "MOMENTUM_EXT" = "Extérieur")
  ) +
  scale_x_continuous(limits = c(0, 62), breaks = seq(0, 60, by = 10)) +
  labs(
    x     = "Minute de jeu",
    y     = "Densité",
    fill  = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

ggsave("output/desc_2_2_1_densite_temps.png", p2, width = 7, height = 5, dpi = 150)

cat("PLOT 2 – Densité d'apparition des phases de momentum selon la minute de jeu\n")
cat("---\n")
cat("Ce graphique de densité représente, pour chaque minute du match (0 à 60),\n")
cat("la fréquence relative à laquelle une phase de momentum commence à cet\n")
cat("instant, en distinguant l'équipe domicile (bleu foncé) et l'équipe\n")
cat("extérieure (bleu clair). Un pic en début de match signalerait que les\n")
cat("premières minutes sont décisives pour installer une dynamique. Un pic\n")
cat("en fin de match indiquerait au contraire que les momentum surgissent\n")
cat("surtout dans les moments de tension finale, lorsque les équipes\n")
cat("accélèrent ou tentent de renverser le score. Des différences entre les\n")
cat("deux courbes renseignent sur des comportements distincts : l'équipe\n")
cat("domicile peut tendre à dominer en début de rencontre (soutien du public),\n")
cat("tandis que l'équipe extérieure réagirait davantage en cours de match.\n\n")

# ------------------------------------------------------------------
# PLOT 3 – Répartition des phases de momentum par quart de match
# ------------------------------------------------------------------

phases_quart <- phases_temps %>%
  mutate(
    quart = case_when(
      T_MIN_DEBUT <  15 ~ "0 – 15 min",
      T_MIN_DEBUT <  30 ~ "15 – 30 min",
      T_MIN_DEBUT <  45 ~ "30 – 45 min",
      TRUE              ~ "45 – 60 min"
    ),
    quart = factor(quart, levels = c("0 – 15 min", "15 – 30 min",
                                     "30 – 45 min", "45 – 60 min"))
  )

quart_stats <- phases_quart %>%
  count(quart) %>%
  mutate(prop = n / sum(n) * 100)

p3 <- ggplot(quart_stats, aes(x = quart, y = prop)) +
  geom_col(fill = "#2171b5", width = 0.6) +
  geom_text(aes(label = paste0(round(prop, 1), "%\n(n = ", n, ")")),
            vjust = -0.3, size = 3.5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    x = "Quart de match",
    y = "Part des phases de momentum (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank())

ggsave("output/desc_2_2_1_quarts.png", p3, width = 6, height = 5, dpi = 150)

cat("PLOT 3 – Répartition des phases de momentum par quart de match\n")
cat("---\n")
cat("Le match est découpé en quatre intervalles de 15 minutes. Ce graphique\n")
cat("montre dans quelle période les épisodes de momentum débutent le plus\n")
cat("souvent. Cette lecture par quart permet de repérer des effets de\n")
cat("séquence propres au handball : la fin de première mi-temps (30 – 45 min)\n")
cat("est un moment de transition particulier, et les équipes cherchent souvent\n")
cat("à marquer les derniers buts avant la pause. Le dernier quart (45 – 60 min)\n")
cat("concentre les efforts défensifs et offensifs dans un temps réduit, ce\n")
cat("qui peut favoriser ou réduire les séquences de buts consécutifs selon\n")
cat("la configuration du score. Dans notre cadre, un momentum en fin de match\n")
cat("aura un impact direct sur le résultat, ce qui rend cette période\n")
cat("particulièrement critique.\n\n")

# ------------------------------------------------------------------
# PLOT 4 – Distribution de la durée des phases (groupes de 1 minute)
# ------------------------------------------------------------------

phases_duree <- phases %>%
  filter(!is.na(duree_min), duree_min >= 0, duree_min <= 20) %>%
  mutate(
    groupe_duree = case_when(
      duree_min <  1 ~ "< 1 min",
      duree_min <  2 ~ "1 – 2 min",
      duree_min <  3 ~ "2 – 3 min",
      duree_min <  5 ~ "3 – 5 min",
      TRUE           ~ "> 5 min"
    ),
    groupe_duree = factor(groupe_duree,
                          levels = c("< 1 min", "1 – 2 min", "2 – 3 min",
                                     "3 – 5 min", "> 5 min"))
  )

duree_stats <- phases_duree %>%
  count(groupe_duree) %>%
  mutate(prop = n / sum(n) * 100)

p4 <- ggplot(duree_stats, aes(x = groupe_duree, y = prop)) +
  geom_col(fill = "#2171b5", width = 0.6) +
  geom_text(aes(label = paste0(round(prop, 1), "%")),
            vjust = -0.4, size = 3.8, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    x = "Durée de la phase",
    y = "Part des phases (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank())

ggsave("output/desc_2_2_1_duree_minutes.png", p4, width = 6, height = 5, dpi = 150)

cat("PLOT 4 – Distribution de la durée des phases de momentum (en minutes)\n")
cat("---\n")
cat("Ce graphique regroupe les phases de momentum par durée réelle (en minutes\n")
cat("de jeu effectif) : moins d'une minute, 1-2 min, 2-3 min, 3-5 min, plus\n")
cat("de 5 min. La durée d'une phase renseigne sur la résistance des équipes :\n")
cat("des phases très courtes (< 1 min) correspondent à des séries de buts\n")
cat("rapidement stoppées par un but adverse ; des phases longues (> 3 min)\n")
cat("révèlent une domination soutenue, potentiellement amplifiée par des\n")
cat("facteurs tactiques (temps mort, exclusion) ou physiques (fatigue,\n")
cat("différentiel athlétique). Dans le cadre de l'analyse de l'impact des\n")
cat("temps morts, la durée des phases avant et après le temps mort constitue\n")
cat("un indicateur clé : interrompre un momentum long est précisément l'un\n")
cat("des objectifs stratégiques de cet outil tactique.\n\n")

# ============================================================
# 2.2.2  CONTEXTE ET PROFIL DES SITUATIONS MENANT À UN MOMENTUM
# ============================================================

cat("============================================================\n")
cat("2.2.2  CONTEXTE ET PROFIL DES SITUATIONS MENANT À UN MOMENTUM\n")
cat("============================================================\n\n")

# ------------------------------------------------------------------
# PLOT 5 – Visualisation temporelle d'un match : état de forme,
#           momentum et temps morts
#
# Adapté de plot_momentum_match() (Plot_momentum.R),
# avec ajout des barres verticales pour les temps morts.
# ------------------------------------------------------------------

plot_momentum_avec_tm <- function(data, match_id, span = 0.2) {

  if (!match_id %in% data$CD_MATCH) {
    stop(paste("Le match", match_id, "n'existe pas dans le dataframe."))
  }

  df_match <- data %>%
    filter(CD_MATCH == match_id) %>%
    arrange(T_MIN)

  # Noms des équipes
  equipe_dom <- unique(df_match$LB_VILLE_DOMICILE)[1]
  equipe_ext <- unique(df_match$LB_VILLE_EXTERIEUR)[1]
  if (is.na(equipe_dom)) equipe_dom <- unique(df_match$LB_CLUB_DOMICILE)[1]
  if (is.na(equipe_ext)) equipe_ext <- unique(df_match$LB_CLUB_EXTERIEUR)[1]

  # Filtrer les actions ayant un temps valide et un état de forme calculé
  df_match <- df_match %>%
    filter(!is.na(ETAT_DE_FORME), !is.na(T_MIN), T_MIN >= 0, T_MIN <= 65)

  # Lissage LOESS (robuste aux NA)
  df_match <- tryCatch({
    df_match %>%
      mutate(
        ETAT_LISSE = predict(
          loess(ETAT_DE_FORME ~ T_MIN, span = span, data = .),
          newdata = T_MIN
        ),
        etat_sup = if_else(ETAT_LISSE > 0.5, ETAT_LISSE, NA_real_),
        etat_inf = if_else(ETAT_LISSE < 0.5, ETAT_LISSE, NA_real_)
      )
  }, error = function(e) {
    df_match %>%
      mutate(ETAT_LISSE = ETAT_DE_FORME,
             etat_sup = if_else(ETAT_DE_FORME > 0.5, ETAT_DE_FORME, NA_real_),
             etat_inf = if_else(ETAT_DE_FORME < 0.5, ETAT_DE_FORME, NA_real_))
  })

  # Temps morts
  df_tm <- df_match %>%
    filter(LB_RESULTAT == "TEMPS MORT")

  # Actions marquant le début d'un épisode de momentum (changement NEUTRE → non-NEUTRE)
  df_onset <- df_match %>%
    arrange(T_MIN) %>%
    mutate(ETAT_LAG = lag(ETAT, default = "NEUTRE")) %>%
    filter(ETAT != "NEUTRE", ETAT_LAG == "NEUTRE", !is.na(LB_RESULTAT_DETAIL)) %>%
    mutate(
      label_action = str_to_title(str_trunc(LB_RESULTAT_DETAIL, 12))
    )

  ymax <- max(df_match$ETAT_LISSE, na.rm = TRUE)
  ymin <- min(df_match$ETAT_LISSE, na.rm = TRUE)
  ypad <- (ymax - ymin) * 0.05

  p <- ggplot(df_match, aes(x = T_MIN)) +

    # Zone domicile (au-dessus de 0.5)
    geom_ribbon(aes(ymin = 0.5, ymax = etat_sup),
                fill = "#2171b5", alpha = 0.35) +

    # Zone extérieur (en dessous de 0.5)
    geom_ribbon(aes(ymin = etat_inf, ymax = 0.5),
                fill = "#6baed6", alpha = 0.35) +

    # Courbe lissée
    geom_line(aes(y = ETAT_LISSE), linewidth = 0.9, color = "grey20") +

    # Ligne de neutralité
    geom_hline(yintercept = 0.5, linetype = "dashed",
               linewidth = 0.7, color = "grey40") +

    # Barres verticales pour les temps morts
    {
      if (nrow(df_tm) > 0)
        geom_vline(data = df_tm, aes(xintercept = T_MIN),
                   linetype = "solid", linewidth = 0.8,
                   color = "#08519c", alpha = 0.7)
      else
        NULL
    } +

    # Points aux débuts de momentum
    {
      if (nrow(df_onset) > 0)
        geom_point(data = df_onset, aes(y = ETAT_LISSE),
                   size = 2, color = "grey20")
      else
        NULL
    } +

    # Labels aux débuts de momentum
    {
      if (nrow(df_onset) > 0)
        geom_text(data = df_onset, aes(y = ETAT_LISSE, label = label_action),
                  vjust = -0.8, size = 2.8, check_overlap = TRUE, color = "grey20")
      else
        NULL
    } +

    # Annotation équipe domicile
    annotate("text", x = 3, y = ymax - ypad,
             label = equipe_dom, fontface = "bold",
             size = 4, color = "#2171b5", hjust = 0) +

    # Annotation équipe extérieure
    annotate("text", x = 3, y = ymin + ypad,
             label = equipe_ext, fontface = "bold",
             size = 4, color = "#6baed6", hjust = 0) +

    scale_x_continuous(limits = c(0, 63), breaks = seq(0, 60, by = 10)) +
    scale_y_continuous(
      limits = c(min(0, ymin - ypad * 2), max(1, ymax + ypad * 2)),
      labels = scales::percent_format()
    ) +
    labs(
      x = "Temps (minutes)",
      y = "Part des 3 derniers buts marqués par l'équipe domicile"
    ) +
    theme_minimal(base_size = 12)

  return(p)
}

# Sélectionner automatiquement un match avec au moins un temps mort
match_avec_tm <- df %>%
  filter(LB_RESULTAT == "TEMPS MORT") %>%
  count(CD_MATCH) %>%
  filter(n >= 2) %>%
  pull(CD_MATCH) %>%
  sort() %>%
  first()

p5 <- plot_momentum_avec_tm(df, match_avec_tm)

ggsave(
  paste0("output/desc_2_2_2_match_", match_avec_tm, ".png"),
  p5, width = 9, height = 5, dpi = 150
)

cat("PLOT 5 – Visualisation temporelle du momentum sur un match\n")
cat(paste0("         Match sélectionné : ", match_avec_tm, "\n"))
cat("---\n")
cat("Ce graphique représente l'évolution du momentum sur l'ensemble d'un match.\n")
cat("L'axe des ordonnées mesure l'état de forme de l'équipe domicile :\n")
cat("il correspond à la proportion de buts marqués par cette équipe parmi les\n")
cat("3 derniers points joués. La valeur 1 (100 %) signifie que l'équipe domicile\n")
cat("a marqué les 3 derniers buts consécutifs (momentum domicile) ; la valeur 0\n")
cat("correspond au scénario inverse (momentum extérieur) ; 0.5 est la zone neutre.\n")
cat("La courbe lissée par LOESS permet de visualiser les grandes tendances au-delà\n")
cat("du bruit séquentiel. Les zones bleues (au-dessus de 0.5 = domicile domine)\n")
cat("et bleu clair (en dessous = extérieur domine) matérialisent visuellement\n")
cat("les phases de momentum. Les barres verticales bleu foncé indiquent les\n")
cat("moments où un temps mort a été demandé : on peut ainsi observer visuellement\n")
cat("si les temps morts sont pris en réponse à un momentum adverse, et si\n")
cat("le momentum change de camp dans la foulée.\n\n")

# ------------------------------------------------------------------
# PLOT 6 – Actions précédant le déclenchement d'un momentum
# ------------------------------------------------------------------

# Identifier la dernière action NEUTRE avant chaque début de phase
# (action à POINTS_TOTAL = debut_pts - 1 dans le même match)
actions_avant_momentum <- phases %>%
  mutate(pts_avant = debut_pts - 1) %>%
  left_join(
    df %>%
      filter(ETAT == "NEUTRE", !is.na(LB_RESULTAT_DETAIL)) %>%
      select(CD_MATCH, POINTS_TOTAL, LB_RESULTAT_DETAIL, LB_RESULTAT) %>%
      distinct(CD_MATCH, POINTS_TOTAL, .keep_all = TRUE),
    by = c("CD_MATCH", "pts_avant" = "POINTS_TOTAL")
  ) %>%
  filter(!is.na(LB_RESULTAT_DETAIL))

# Nettoyer et regrouper les libellés (ne garder que les modalités représentatives)
top_actions <- actions_avant_momentum %>%
  count(LB_RESULTAT_DETAIL, sort = TRUE) %>%
  mutate(prop = n / sum(n) * 100) %>%
  filter(prop >= 2) %>%             # au moins 2 % des cas
  mutate(
    Action = str_to_title(LB_RESULTAT_DETAIL),
    Action = factor(Action, levels = rev(Action))  # ordre décroissant
  )

p6 <- ggplot(top_actions, aes(x = Action, y = prop)) +
  geom_col(fill = "#2171b5", width = 0.65) +
  geom_text(aes(label = paste0(round(prop, 1), "%")),
            hjust = -0.15, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    x = NULL,
    y = "Part des déclenchements de momentum (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

ggsave("output/desc_2_2_2_actions_creatrices.png", p6, width = 7, height = 5, dpi = 150)

cat("PLOT 6 – Actions précédant le déclenchement d'un momentum\n")
cat("---\n")
cat("Ce graphique montre la distribution des actions (LB_RESULTAT_DETAIL) qui\n")
cat("précèdent immédiatement le début d'une phase de momentum. Concrètement,\n")
cat("il s'agit de la dernière action en situation neutre avant que l'une des\n")
cat("deux équipes n'entame sa série de 3 buts consécutifs. Ces actions\n")
cat("constituent le contexte déclencheur du momentum. Un arrêt du gardien\n")
cat("(ARRÊT), une interception ou un tir hors-cadre adverse signifient que\n")
cat("c'est une action DÉFENSIVE réussie qui provoque le transfert de balle\n")
cat("et permet à l'équipe d'entamer sa série. Ce profil est fondamental pour\n")
cat("comprendre les mécanismes du momentum en handball : il met en évidence\n")
cat("le rôle du gardien et de la défense comme moteurs de la dynamique\n")
cat("offensive à venir, et non simplement le talent offensif de l'équipe\n")
cat("qui enchaîne les buts. Ces résultats font écho aux travaux de psychologie\n")
cat("sportive qui soulignent l'importance des émotions collectives déclenchées\n")
cat("par une belle action défensive dans l'installation d'un élan de momentum.\n\n")

# ============================================================
# RÉCAPITULATIF
# ============================================================

cat("============================================================\n")
cat("Graphiques sauvegardés dans output/ :\n")
cat("\n  Section 2.2.1 – Fréquence et répartition\n")
cat("    desc_2_2_1_dist_phases.png      – Nb de phases par match\n")
cat("    desc_2_2_1_densite_temps.png    – Densité sur 0-60 min (DOM vs EXT)\n")
cat("    desc_2_2_1_quarts.png           – Répartition par quart de 15 min\n")
cat("    desc_2_2_1_duree_minutes.png    – Durée des phases (groupes min)\n")
cat("\n  Section 2.2.2 – Contexte et profil\n")
cat(paste0("    desc_2_2_2_match_", match_avec_tm, ".png\n"))
cat("    desc_2_2_2_actions_creatrices.png – Actions précédant les momentum\n")
cat("============================================================\n")
