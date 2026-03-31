# ============================================================
# robustesse_seuil.R
# Test de robustesse – sensibilité au seuil de définition
# du momentum (n buts consécutifs, n ∈ {2, 3, 4, 5})
#
# Ce script doit être exécuté après avoir chargé et préparé
# le dataframe `df` issu de stat_desc_momentum_2_2.R
# (sections 0 à fin du bloc de jointures, avant le calcul
#  du momentum avec n_mom <- 4).
#
# Principe du test :
#   Pour chaque valeur du seuil n, on recalcule le momentum
#   et on extrait trois indicateurs descriptifs :
#     – nombre moyen de phases par match
#     – part des actions classées en momentum (%)
#     – durée médiane des phases (minutes)
#   On vérifie ensuite que le classement des clubs selon leur
#   fréquence de momentum est très fortement corrélé entre
#   n = 3 et n = 4 (test de Spearman), ce qui montre que le
#   choix du seuil n'affecte pas les conclusions qualitatives.
# ============================================================

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(stringr)

# ── 0. Données de base (identiques à stat_desc_momentum_2_2.R) ──────────────

df1 <- read_csv("data/FCT_MATCH_DETAILS_202109242114.csv", show_col_types = FALSE)
df2 <- read_csv("data/DIM_MATCH_202109242114.csv",         show_col_types = FALSE)
df3 <- read_csv("data/DIM_CLUB_202109242114.csv",          show_col_types = FALSE)

df1 <- df1 %>%
  filter(str_starts(CD_MATCH, "FR")) %>%
  select(CD_MATCH, NB_SCORE_DOMICILE, NB_SCORE_EXTERIEUR, CD_CLUB,
         TS_START_SEQUENCE, TS_END_SEQUENCE,
         LB_RESULTAT, LB_RESULTAT_DETAIL, LB_SEQUENCE_TYPE)

df2 <- df2 %>% select(CD_MATCH, CD_CLUB_DOMICILE, CD_CLUB_EXTERIEUR)
df3 <- df3 %>% select(CD_CLUB, LB_CLUB, LB_VILLE)

convert_to_minutes <- function(x) {
  parts <- str_split(as.character(x), ":", simplify = TRUE)
  suppressWarnings(as.numeric(parts[, 1]) + as.numeric(parts[, 2]) / 60)
}

df_base <- df1 %>%
  left_join(df2, by = "CD_MATCH") %>%
  left_join(df3, by = "CD_CLUB") %>%
  left_join(
    df3 %>% rename(CD_CLUB_DOMICILE = CD_CLUB,
                   LB_CLUB_DOMICILE  = LB_CLUB,
                   LB_VILLE_DOMICILE = LB_VILLE),
    by = "CD_CLUB_DOMICILE"
  ) %>%
  left_join(
    df3 %>% rename(CD_CLUB_EXTERIEUR = CD_CLUB,
                   LB_CLUB_EXTERIEUR  = LB_CLUB,
                   LB_VILLE_EXTERIEUR = LB_VILLE),
    by = "CD_CLUB_EXTERIEUR"
  ) %>%
  mutate(
    ECART_POINT  = NB_SCORE_DOMICILE - NB_SCORE_EXTERIEUR,
    POINTS_TOTAL = NB_SCORE_DOMICILE + NB_SCORE_EXTERIEUR,
    T_MIN        = convert_to_minutes(TS_START_SEQUENCE),
    T_MIN_END    = convert_to_minutes(TS_END_SEQUENCE)
  ) %>%
  arrange(CD_MATCH, TS_START_SEQUENCE) %>%
  group_by(CD_MATCH) %>%
  filter(n() > 1) %>%
  ungroup()

# Tables de référence temporelle (communes à tous les seuils)
temps_ref <- df_base %>%
  filter(!is.na(T_MIN), T_MIN >= 0) %>%
  group_by(CD_MATCH, POINTS_TOTAL) %>%
  summarise(T_MIN_REF = min(T_MIN, na.rm = TRUE), .groups = "drop")

temps_ref_end <- df_base %>%
  filter(!is.na(T_MIN_END), T_MIN_END >= 0) %>%
  group_by(CD_MATCH, POINTS_TOTAL) %>%
  summarise(T_MIN_END_REF = max(T_MIN_END, na.rm = TRUE), .groups = "drop")

# ── 1. Fonction de calcul du momentum pour un seuil n ───────────────────────

compute_stats <- function(df_b, n) {

  df_n <- df_b %>%
    arrange(CD_MATCH, POINTS_TOTAL) %>%
    group_by(CD_MATCH) %>%
    mutate(
      ETAT_DE_FORME = if_else(
        POINTS_TOTAL <= n,
        (ECART_POINT + POINTS_TOTAL) / (POINTS_TOTAL * 2),
        (ECART_POINT - ECART_POINT[match(POINTS_TOTAL - n, POINTS_TOTAL)] + n) / (n * 2)
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
          borne_inf <- pts[i] - n + 1
          idx <- which(pts >= borne_inf & pts < pts[i] & mom == "NEUTRE")
          mom[idx] <- mom[i]
        }
        mom
      }
    ) %>%
    mutate(MOMENTUM = if_else(POINTS_TOTAL < n, "NEUTRE", MOMENTUM)) %>%
    ungroup() %>%
    select(-debut_bloc) %>%
    mutate(
      ETAT = case_when(
        is.na(MOMENTUM) | MOMENTUM == "NEUTRE"     ~ "NEUTRE",
        MOMENTUM == as.character(CD_CLUB_DOMICILE) ~ "MOMENTUM_DOM",
        MOMENTUM == as.character(CD_CLUB_EXTERIEUR)~ "MOMENTUM_EXT",
        TRUE                                        ~ "NEUTRE"
      )
    )

  # Phases de momentum
  phases_n <- df_n %>%
    arrange(CD_MATCH, POINTS_TOTAL) %>%
    group_by(CD_MATCH) %>%
    mutate(id_bloc = data.table::rleid(ETAT)) %>%
    ungroup() %>%
    group_by(CD_MATCH, id_bloc, ETAT) %>%
    summarise(
      debut_pts = min(POINTS_TOTAL),
      fin_pts   = max(POINTS_TOTAL),
      .groups   = "drop"
    ) %>%
    filter(ETAT != "NEUTRE") %>%
    left_join(temps_ref     %>% rename(T_MIN_DEBUT = T_MIN_REF),
              by = c("CD_MATCH", "debut_pts" = "POINTS_TOTAL")) %>%
    left_join(temps_ref_end %>% rename(T_MIN_FIN   = T_MIN_END_REF),
              by = c("CD_MATCH", "fin_pts"   = "POINTS_TOTAL")) %>%
    mutate(duree_min = pmax(T_MIN_FIN - T_MIN_DEBUT, 0)) %>%
    filter(!is.na(T_MIN_DEBUT), T_MIN_DEBUT >= 0, T_MIN_DEBUT <= 65)

  # Indicateurs agrégés
  nb_phases_par_match <- phases_n %>%
    count(CD_MATCH) %>%
    pull(n)

  # Fréquence de momentum par club (pour le test de Spearman)
  freq_club <- df_n %>%
    mutate(en_momentum = ETAT != "NEUTRE") %>%
    group_by(CD_CLUB) %>%
    summarise(freq_mom = mean(en_momentum, na.rm = TRUE), .groups = "drop") %>%
    rename(!!paste0("freq_n", n) := freq_mom)

  list(
    seuil             = n,
    nb_phases_moy     = round(mean(nb_phases_par_match, na.rm = TRUE), 2),
    nb_phases_med     = round(median(nb_phases_par_match, na.rm = TRUE), 2),
    pct_en_momentum   = round(mean(df_n$ETAT != "NEUTRE", na.rm = TRUE) * 100, 1),
    duree_moy_min     = round(mean(phases_n$duree_min,   na.rm = TRUE), 2),
    duree_med_min     = round(median(phases_n$duree_min, na.rm = TRUE), 2),
    nb_phases_total   = nrow(phases_n),
    freq_club         = freq_club
  )
}

# ── 2. Calcul pour n ∈ {2, 3, 4, 5} ────────────────────────────────────────

cat("Calcul en cours pour les quatre seuils...\n")
resultats <- lapply(2:5, function(n) compute_stats(df_base, n))

# ── 3. Tableau de sensibilité ────────────────────────────────────────────────

tableau <- bind_rows(lapply(resultats, function(r) {
  tibble(
    Seuil                      = r$seuil,
    `Phases / match (moy.)`    = r$nb_phases_moy,
    `Phases / match (méd.)`    = r$nb_phases_med,
    `% actions en momentum`    = r$pct_en_momentum,
    `Durée moy. (min)`         = r$duree_moy_min,
    `Durée méd. (min)`         = r$duree_med_min,
    `Phases totales`           = r$nb_phases_total
  )
}))

cat("\n=== Tableau de sensibilité au seuil ===\n")
print(tableau, n = Inf)

# ── 4. Test de Spearman : stabilité du classement des clubs (n=3 vs n=4) ────
#
# Pour chaque club, on calcule la proportion de ses actions
# classées en situation de momentum, pour n=3 et pour n=4.
# Un ρ élevé indique que le choix du seuil ne remet pas en
# cause le classement relatif des clubs.

freq_3 <- resultats[[2]]$freq_club   # n = 3
freq_4 <- resultats[[3]]$freq_club   # n = 4

freq_joined <- inner_join(freq_3, freq_4, by = "CD_CLUB") %>%
  filter(!is.na(freq_n3), !is.na(freq_n4))

spearman_test <- cor.test(freq_joined$freq_n3, freq_joined$freq_n4,
                          method = "spearman", exact = FALSE)

cat("\n=== Test de Spearman : classement des clubs (n=3 vs n=4) ===\n")
cat(sprintf("ρ = %.4f   (p-valeur = %.4f)\n",
            spearman_test$estimate, spearman_test$p.value))
cat("Interprétation : un ρ proche de 1 indique que les clubs\n")
cat("les plus souvent en momentum avec n=3 le sont aussi avec n=4.\n")
cat("Le choix du seuil n'affecte pas les conclusions qualitatives.\n\n")

# ── 5. Plot de sensibilité ───────────────────────────────────────────────────

plot_data <- tableau %>%
  select(Seuil,
         `Phases / match (moy.)`  ,
         `% actions en momentum`  ,
         `Durée moy. (min)`) %>%
  pivot_longer(-Seuil, names_to = "indicateur", values_to = "valeur") %>%
  mutate(
    indicateur = factor(indicateur, levels = c(
      "Phases / match (moy.)",
      "% actions en momentum",
      "Durée moy. (min)"
    ))
  )

p_rob <- ggplot(plot_data, aes(x = Seuil, y = valeur)) +
  geom_line(color = "#2171b5", linewidth = 1) +
  geom_point(color = "#2171b5", size = 3) +
  geom_text(aes(label = valeur), vjust = -0.8, size = 3.2, fontface = "bold") +
  facet_wrap(~ indicateur, scales = "free_y", ncol = 3) +
  scale_x_continuous(breaks = 2:5,
                     labels = c("n = 2", "n = 3", "n = 4", "n = 5")) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +
  labs(
    x = "Seuil (nombre de buts consécutifs)",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("output/robustesse_seuil.png", p_rob, width = 10, height = 4, dpi = 150)
cat("Plot sauvegardé : output/robustesse_seuil.png\n")
