# ============================================================
# stat_desc_momentum_2_2.R
# Statistiques descriptives du momentum – Sections 2.2.1 et 2.2.2
#
# Définition retenue :
#   Une équipe est en situation de momentum si elle a marqué
#   les 4 derniers buts consécutifs (sur les 4 derniers points joués).
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
# 1. CALCUL DU MOMENTUM (4 buts consécutifs)
# ============================================================

n_mom <- 4

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
cat("épisodes de momentum au total (toutes équipes confondues). Un épisode\n")
cat("de momentum est défini comme une phase où l'une des deux équipes a\n")
cat("marqué 4 buts consécutifs. Ce graphique répond à une question préalable\n")
cat("fondamentale : le momentum est-il un événement rare ou au contraire\n")
cat("structurant dans un match de handball ? Si la distribution est très\n")
cat("concentrée sur 0 ou 1 phase, le phénomène serait anecdotique et\n")
cat("difficile à étudier statistiquement. Si elle est centrée sur 3 à 6\n")
cat("phases par match, cela confirme que les renversements de domination\n")
cat("sont réguliers et que le momentum constitue bien une réalité de jeu\n")
cat("à part entière, justifiant son étude comme indicateur de performance\n")
cat("et comme cible potentielle pour des décisions tactiques comme le\n")
cat("temps mort.\n\n")

# ------------------------------------------------------------------
# PLOT 2 – Densité d'apparition des phases sur la durée du match (0-60 min)
# ------------------------------------------------------------------

phases_temps <- phases %>%
  filter(!is.na(T_MIN_DEBUT), T_MIN_DEBUT >= 0, T_MIN_DEBUT <= 62)

p2 <- ggplot(phases_temps, aes(x = T_MIN_DEBUT)) +
  geom_density(fill = "#2171b5", alpha = 0.55, color = NA, bw = 3) +
  scale_x_continuous(limits = c(0, 62), breaks = seq(0, 60, by = 10)) +
  labs(
    x = "Minute de jeu",
    y = "Densité"
  ) +
  theme_minimal(base_size = 12)

ggsave("output/desc_2_2_1_densite_temps.png", p2, width = 7, height = 5, dpi = 150)

cat("PLOT 2 – Densité d'apparition des phases de momentum selon la minute de jeu\n")
cat("---\n")
cat("Ce graphique de densité lissée représente, pour chaque minute du match\n")
cat("(0 à 60), la fréquence relative à laquelle une phase de momentum débute\n")
cat("à cet instant, toutes équipes confondues. La densité est estimée par\n")
cat("noyau gaussien (bandwidth = 3 min) pour obtenir une courbe continue.\n")
cat("\n")
cat("Une courbe relativement plate indiquerait que le momentum peut surgir\n")
cat("à tout moment du match sans période privilégiée. Au contraire, des pics\n")
cat("marqués révèlent des moments charnières : un pic en début de match\n")
cat("suggère que les premières minutes sont décisives pour s'installer dans\n")
cat("le jeu ; un pic en fin de match traduit l'accélération tactique des\n")
cat("équipes qui cherchent à sécuriser ou renverser le score. Ce graphique\n")
cat("oriente l'analyse vers les moments où un temps mort serait le plus\n")
cat("pertinent pour briser un élan naissant.\n\n")

# ------------------------------------------------------------------
# PLOT 3 – Répartition des phases de momentum par quart de match
# ------------------------------------------------------------------

phases_quart <- phases_temps %>%
  mutate(
    periode = case_when(
      T_MIN_DEBUT <  10 ~ "0 – 10 min",
      T_MIN_DEBUT <  20 ~ "10 – 20 min",
      T_MIN_DEBUT <  30 ~ "20 – 30 min",
      T_MIN_DEBUT <  40 ~ "30 – 40 min",
      T_MIN_DEBUT <  50 ~ "40 – 50 min",
      TRUE              ~ "50 – 60 min"
    ),
    periode = factor(periode, levels = c("0 – 10 min", "10 – 20 min",
                                         "20 – 30 min", "30 – 40 min",
                                         "40 – 50 min", "50 – 60 min"))
  )

quart_stats <- phases_quart %>%
  count(periode) %>%
  mutate(prop = n / sum(n) * 100)

p3 <- ggplot(quart_stats, aes(x = periode, y = prop)) +
  geom_col(fill = "#2171b5", width = 0.6) +
  geom_text(aes(label = paste0(round(prop, 1), "%\n(n = ", n, ")")),
            vjust = -0.3, size = 3.5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    x = "Période de jeu",
    y = "Part des phases de momentum (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 9))

ggsave("output/desc_2_2_1_periodes.png", p3, width = 7, height = 5, dpi = 150)

cat("PLOT 3 – Répartition des phases de momentum par période de 10 minutes\n")
cat("---\n")
cat("Le match est découpé en six intervalles de 10 minutes. Ce découpage\n")
cat("plus fin permet de repérer des moments charnières dans la dynamique\n")
cat("de jeu. La période 20-30 min correspond aux dernières minutes de la\n")
cat("première mi-temps, où les équipes cherchent souvent à prendre l'avantage\n")
cat("avant la pause. La période 30-40 min couvre les premières minutes de la\n")
cat("seconde mi-temps, moment où les ajustements tactiques peuvent provoquer\n")
cat("un changement de dynamique. Enfin, la période 50-60 min est la plus\n")
cat("critique : un momentum non interrompu dans ces dernières minutes a un\n")
cat("impact direct sur le résultat. Ce graphique permet d'identifier si\n")
cat("certaines périodes sont structurellement plus propices à l'émergence\n")
cat("d'un momentum, ce qui renseigne sur les moments où un temps mort\n")
cat("serait le plus pertinent.\n\n")

# ------------------------------------------------------------------
# PLOT 4 – Distribution de la durée des phases (groupes de 1 minute)
# ------------------------------------------------------------------

phases_duree <- phases %>%
  filter(!is.na(duree_min), duree_min >= 0, duree_min <= 30) %>%
  mutate(
    groupe_duree = case_when(
      duree_min <  3 ~ "< 3 min",
      duree_min <  4 ~ "3 – 4 min",
      duree_min <  5 ~ "4 – 5 min",
      duree_min <  6 ~ "5 – 6 min",
      duree_min <  7 ~ "6 – 7 min",
      duree_min <  8 ~ "7 – 8 min",
      TRUE           ~ "> 8 min"
    ),
    groupe_duree = factor(groupe_duree,
                          levels = c("< 3 min", "3 – 4 min", "4 – 5 min",
                                     "5 – 6 min", "6 – 7 min", "7 – 8 min",
                                     "> 8 min"))
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

ggsave("output/desc_2_2_1_duree_minutes.png", p4, width = 7, height = 5, dpi = 150)

cat("PLOT 4 – Distribution de la durée des phases de momentum (en minutes)\n")
cat("---\n")
cat("Ce graphique regroupe les phases de momentum par durée réelle de jeu,\n")
cat("en intervalles d'une minute de 3 à 8 minutes (< 3 min, 3-4, 4-5,\n")
cat("5-6, 6-7, 7-8 et > 8 min). La durée d'une phase est mesurée entre le\n")
cat("premier et le dernier point joué pendant l'épisode de momentum.\n")
cat("Une phase courte (< 3 min) est rapidement interrompue, souvent par\n")
cat("un but adverse qui brise la série ; une phase longue (> 5 min) traduit\n")
cat("une domination soutenue, potentiellement amplifiée par une exclusion\n")
cat("temporaire, la fatigue ou un différentiel athlétique marqué. Dans\n")
cat("notre problématique, cette distribution est directement liée à\n")
cat("l'efficacité du temps mort comme outil d'interruption : c'est\n")
cat("précisément pour stopper les phases longues que les entraîneurs y\n")
cat("ont recours. La proportion de phases très longues (> 7 min) révèle\n")
cat("l'ampleur des dominations extrêmes, celles qui peuvent faire basculer\n")
cat("définitivement un match.\n\n")

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

  # Temps morts : identifier l'équipe qui le demande via CD_CLUB
  df_tm <- df_match %>%
    filter(LB_RESULTAT == "TEMPS MORT") %>%
    mutate(
      equipe_tm = if_else(
        !is.na(CD_CLUB) & !is.na(CD_CLUB_DOMICILE) & CD_CLUB == CD_CLUB_DOMICILE,
        if_else(!is.na(LB_VILLE_DOMICILE), LB_VILLE_DOMICILE, LB_CLUB_DOMICILE),
        if_else(!is.na(LB_VILLE_EXTERIEUR), LB_VILLE_EXTERIEUR, LB_CLUB_EXTERIEUR)
      ),
      label_tm = paste0("TM\n", equipe_tm)
    )

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
                fill = "red", alpha = 0.25) +

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

    # Labels des temps morts (équipe qui le demande)
    {
      if (nrow(df_tm) > 0)
        geom_text(data = df_tm,
                  aes(x = T_MIN, y = max(df_match$ETAT_LISSE, na.rm = TRUE),
                      label = label_tm),
                  inherit.aes = FALSE,
                  hjust = -0.1, size = 2.6, color = "#08519c", lineheight = 0.9)
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
             size = 4, color = "red", hjust = 0) +

    scale_x_continuous(limits = c(0, 63), breaks = seq(0, 60, by = 10)) +
    scale_y_continuous(
      limits = c(min(0, ymin - ypad * 2), max(1, ymax + ypad * 2)),
      labels = scales::percent_format()
    ) +
    labs(
      x = "Temps (minutes)",
      y = "Part des 4 derniers buts marqués par l'équipe domicile"
    ) +
    theme_minimal(base_size = 12)

  return(p)
}

match_id_exemple <- "FR-1-H_32_33_5723"

p5 <- plot_momentum_avec_tm(df, match_id_exemple)

ggsave(
  "output/desc_2_2_2_match_exemple.png",
  p5, width = 9, height = 5, dpi = 150
)

cat("PLOT 5 – Visualisation temporelle du momentum sur un match\n")
cat(paste0("         Match : ", match_id_exemple, "\n"))
cat("---\n")
cat("Ce graphique représente l'évolution du momentum sur l'ensemble d'un match.\n")
cat("L'axe des ordonnées indique l'état de forme de l'équipe domicile,\n")
cat("défini comme la proportion de buts marqués par cette équipe parmi les\n")
cat("4 derniers points joués. La valeur 1 (100 %) signifie que l'équipe\n")
cat("domicile a marqué les 4 derniers buts consécutifs (momentum domicile) ;\n")
cat("la valeur 0 correspond au scénario inverse (momentum extérieur) ;\n")
cat("0,5 représente la zone neutre (ni l'un ni l'autre n'est en momentum).\n")
cat("\n")
cat("La courbe est lissée par régression LOESS (Locally Estimated Scatterplot\n")
cat("Smoothing). Cette méthode ajuste localement une régression polynomiale\n")
cat("sur une fenêtre glissante autour de chaque point, ce qui produit une\n")
cat("courbe continue qui suit les tendances de fond tout en atténuant le\n")
cat("bruit des actions individuelles. Elle permet ainsi de distinguer les\n")
cat("grandes phases de domination des simples fluctuations ponctuelles.\n")
cat("\n")
cat("Les zones bleues (au-dessus de 0,5) et rouges (en dessous) matérialisent\n")
cat("les périodes de momentum domicile et extérieur. Les barres verticales\n")
cat("bleues indiquent les temps morts, avec le nom de l'équipe qui les demande.\n")
cat("On peut ainsi observer directement si les temps morts sont pris en\n")
cat("réaction à un momentum adverse et si la dynamique change dans la foulée,\n")
cat("ce qui constitue l'hypothèse centrale de notre étude.\n\n")

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

# Nettoyer : exclure les buts (triviaux) et neutralisations
# puis garder les modalités représentatives
top_actions <- actions_avant_momentum %>%
  filter(!LB_RESULTAT_DETAIL %in% c("BUT", "NEUTRALISATION")) %>%
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
cat("Ce graphique montre la distribution des actions (LB_RESULTAT_DETAIL)\n")
cat("qui précèdent immédiatement le début d'une phase de momentum.\n")
cat("Concrètement, il s'agit de la dernière action enregistrée en situation\n")
cat("neutre avant que l'une des deux équipes n'entame sa série de 4 buts\n")
cat("consécutifs. Ces actions constituent le contexte déclencheur du momentum.\n")
cat("\n")
cat("Deux catégories ont été délibérément exclues de cette analyse :\n")
cat("- BUT : les buts sont déjà intégrés dans la définition même du momentum\n")
cat("  (4 buts consécutifs). Les inclure reviendrait à une tautologie. De plus,\n")
cat("  les buts classés comme difficiles (TIR DIFFICILE) ont été conservés\n")
cat("  séparément mais s'avèrent finalement peu nombreux et peu associés à\n")
cat("  un déclenchement de momentum, ce qui suggère que la dynamique de\n")
cat("  momentum est davantage liée à la récupération de balle qu'à la\n")
cat("  qualité intrinsèque du tir.\n")
cat("- NEUTRALISATION : contrairement à un arrêt ou une interception, la\n")
cat("  neutralisation ne permet pas à l'équipe adverse de récupérer le ballon\n")
cat("  (l'attaque conserve la possession). Elle ne constitue donc pas un\n")
cat("  déclencheur plausible de momentum pour l'équipe qui défend, et son\n")
cat("  inclusion aurait faussé la lecture du graphique.\n")
cat("\n")
cat("Les actions restantes – arrêts du gardien, interceptions, tirs\n")
cat("hors-cadre, fautes offensives, 2 minutes, etc. – représentent des\n")
cat("situations où la défense récupère le ballon, permettant à l'équipe\n")
cat("d'enchaîner des attaques. Ce profil met en évidence le rôle central\n")
cat("du gardien et de la défense comme moteurs de la dynamique offensive :\n")
cat("le momentum naît souvent d'une belle action défensive plutôt que d'un\n")
cat("talent offensif intrinsèque.\n\n")

# ============================================================
# RÉCAPITULATIF
# ============================================================

cat("============================================================\n")
cat("Graphiques sauvegardés dans output/ :\n")
cat("\n  Section 2.2.1 – Fréquence et répartition\n")
cat("    desc_2_2_1_dist_phases.png      – Nb de phases par match\n")
cat("    desc_2_2_1_densite_temps.png    – Densité sur 0-60 min\n")
cat("    desc_2_2_1_periodes.png         – Répartition par période de 10 min\n")
cat("    desc_2_2_1_duree_minutes.png    – Durée des phases (groupes min)\n")
cat("\n  Section 2.2.2 – Contexte et profil\n")
cat("    desc_2_2_2_match_exemple.png       – Match FR-1-H_32_33_5723\n")
cat("    desc_2_2_2_actions_creatrices.png – Actions précédant les momentum\n")
cat("============================================================\n")
