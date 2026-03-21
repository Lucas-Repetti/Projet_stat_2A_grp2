# ============================================================
# plot_choix_momentum.R
# Justification du choix du seuil de définition du momentum
# (nombre de buts consécutifs : 2, 3 ou 4)
# ============================================================

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(stringr)

# ============================================================
# 1. DONNÉES
# ============================================================

df1 <- read_csv("data/FCT_MATCH_DETAILS_202109242114.csv")
df2 <- read_csv("data/DIM_MATCH_202109242114.csv")

df1 <- df1 %>%
  filter(str_starts(CD_MATCH, "FR")) %>%
  select(CD_MATCH, NB_SCORE_DOMICILE, NB_SCORE_EXTERIEUR, CD_CLUB, TS_START_SEQUENCE)

df2 <- df2 %>% select(CD_MATCH, CD_CLUB_DOMICILE, CD_CLUB_EXTERIEUR)

df <- df1 %>%
  left_join(df2, by = "CD_MATCH") %>%
  mutate(
    ECART_POINT  = NB_SCORE_DOMICILE - NB_SCORE_EXTERIEUR,
    POINTS_TOTAL = NB_SCORE_DOMICILE + NB_SCORE_EXTERIEUR
  ) %>%
  arrange(CD_MATCH, TS_START_SEQUENCE) %>%
  group_by(CD_MATCH) %>%
  filter(n() > 1) %>%
  ungroup()

# ============================================================
# 2. CALCUL DU MOMENTUM POUR N = 2, 3, 4 BUTS CONSÉCUTIFS
# ============================================================

calcul_momentum_n <- function(df, n) {
  df %>%
    arrange(CD_MATCH, POINTS_TOTAL) %>%
    group_by(CD_MATCH) %>%
    mutate(
      ETAT_DE_FORME = if_else(
        POINTS_TOTAL <= n,
        (ECART_POINT + POINTS_TOTAL) / (POINTS_TOTAL * 2),
        (ECART_POINT - ECART_POINT[match(POINTS_TOTAL - n, POINTS_TOTAL)] + n) / (n * 2)
      ),
      MOMENTUM = case_when(
        ETAT_DE_FORME >= 1       ~ "DOM",
        ETAT_DE_FORME <= 0       ~ "EXT",
        TRUE                     ~ "NEUTRE"
      )
    ) %>%
    mutate(debut_bloc = MOMENTUM != "NEUTRE" & lag(MOMENTUM, default = "NEUTRE") == "NEUTRE") %>%
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
    select(-debut_bloc)
}

# ============================================================
# 3. STATISTIQUES PAR SEUIL
# ============================================================

stats_par_seuil <- lapply(2:4, function(n) {
  df_mom <- calcul_momentum_n(df, n)

  df_mom %>%
    arrange(CD_MATCH, POINTS_TOTAL) %>%
    group_by(CD_MATCH) %>%
    mutate(id_seq = data.table::rleid(MOMENTUM)) %>%
    summarise(
      # Nombre de phases de domination distinctes
      nb_phases      = n_distinct(id_seq[MOMENTUM != "NEUTRE"]),
      # % de séquences de jeu en situation de domination
      pct_momentum   = mean(MOMENTUM != "NEUTRE") * 100,
      # Durée moyenne d'une phase (en nombre de points)
      duree_moy      = {
        phases <- rle(MOMENTUM)
        moy <- mean(phases$lengths[phases$values != "NEUTRE"])
        ifelse(is.nan(moy), 0, moy)
      },
      .groups = "drop"
    ) %>%
    mutate(seuil = paste0(n, " buts\nconsécutifs"))
}) %>%
  bind_rows()

# ============================================================
# 4. TABLE RÉSUMÉ (pour annotation des graphiques)
# ============================================================

resume <- stats_par_seuil %>%
  group_by(seuil) %>%
  summarise(
    nb_phases_moy  = round(mean(nb_phases), 1),
    pct_mom_moy    = round(mean(pct_momentum), 1),
    duree_moy_moy  = round(mean(duree_moy), 2),
    .groups = "drop"
  )

print(resume)

# ============================================================
# 5. GRAPHIQUES
# ============================================================

couleurs <- c("2 buts\nconsécutifs" = "#bdd7e7",
              "3 buts\nconsécutifs" = "#6baed6",
              "4 buts\nconsécutifs" = "#2171b5")

# --- Nombre moyen de phases de domination par match ---
p_phases <- ggplot(resume, aes(x = seuil, y = nb_phases_moy, fill = seuil)) +
  geom_col(width = 0.55, color = "white") +
  geom_text(aes(label = nb_phases_moy), vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = couleurs) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Nombre de phases / match") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# --- % de séquences en situation de domination ---
p_pct <- ggplot(resume, aes(x = seuil, y = pct_mom_moy, fill = seuil)) +
  geom_col(width = 0.55, color = "white") +
  geom_text(aes(label = paste0(pct_mom_moy, "%")), vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = couleurs) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), limits = c(0, 100)) +
  labs(x = NULL, y = "% de séquences en domination") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# --- Sauvegarde ---

ggsave("output/plot_momentum_nb_phases.png", p_phases,
       width = 5, height = 5, dpi = 150)

ggsave("output/plot_momentum_pct.png", p_pct,
       width = 5, height = 5, dpi = 150)

cat("Graphiques sauvegardés :\n")
cat("  output/plot_momentum_nb_phases.png\n")
cat("  output/plot_momentum_pct.png\n")
