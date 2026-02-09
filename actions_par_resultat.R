library(ggplot2)
library(dplyr)
library(grid)  # pour unit()

df_match_area <- df_match_area %>%
  arrange(CD_MATCH, TS_START_SEQUENCE) %>%
  group_by(CD_MATCH) %>%
  mutate(
    # remplacer NA par FALSE pour ETAT_DE_FORME >= 0.5
    above_half = if_else(is.na(ETAT_DE_FORME), FALSE, ETAT_DE_FORME >= 0.5),
    
    # TRUE si traversée du seuil
    change = above_half != lag(above_half, default = above_half[1]),
    
    # numéro de momentum
    MOMENTUM_NUM = cumsum(change) + 1L
  ) %>%
  ungroup() %>%
  select(-above_half, -change)  # retirer colonnes intermédiaires si tu veux




# -----------------------------
# 1. Préparer les actions par momentum
# -----------------------------
actions_momentum <- df_match_area %>%
  filter(!is.na(MOMENTUM_NUM), !is.na(LB_RESULTAT_DETAIL)) %>%
  mutate(
    # déterminer l'équipe qui contrôle le momentum
    equipe_momentum = if_else(ETAT_DE_FORME >= 0.5, CD_CLUB_DOMICILE, CD_CLUB_EXTERIEUR),
    
    # coordonnees pour plot
    X_ACTION  = X_DERNIERE_PASSE,
    Y_ACTION  = Y_DERNIERE_PASSE
  ) %>%
  filter(!is.na(X_ACTION), !is.na(Y_ACTION))

# -----------------------------
# 2. Plots par momentum pour toutes les actions
# -----------------------------
plots_actions <- actions_momentum %>%
  group_split(MOMENTUM_NUM) %>%
  lapply(function(df) {
    p_terrain +
      geom_point(
        data = df,
        aes(x = X_ACTION, y = Y_ACTION, color = LB_RESULTAT_DETAIL),
        size = 3,
        alpha = 0.8
      ) +
      scale_color_brewer(palette = "Set3", name = "Type d'action") +
      labs(title = paste("Actions – Momentum", unique(df$MOMENTUM_NUM)))
  })

# Afficher les plots par momentum
for(p in plots_actions) print(p)

# -----------------------------
# 3. Préparer les actions qui changent le momentum
# -----------------------------
actions_changement_momentum <- df_match_area %>%
  arrange(CD_MATCH, TS_START_SEQUENCE) %>%
  group_by(CD_MATCH) %>%
  mutate(
    above_half = if_else(is.na(ETAT_DE_FORME), FALSE, ETAT_DE_FORME >= 0.5),
    change_momentum = above_half != lag(above_half, default = above_half[1])
  ) %>%
  ungroup() %>%
  filter(change_momentum, !is.na(X_DERNIERE_PASSE), !is.na(Y_DERNIERE_PASSE)) %>%
  mutate(
    X_ACTION = X_DERNIERE_PASSE,
    Y_ACTION = Y_DERNIERE_PASSE
  )

# -----------------------------
# 4. Plot des actions qui changent le momentum
# -----------------------------
p_changement_momentum <- p_terrain +
  geom_point(
    data = actions_changement_momentum,
    aes(x = X_ACTION, y = Y_ACTION, color = LB_RESULTAT_DETAIL),
    size = 3,
    alpha = 0.9
  ) +
  scale_color_brewer(palette = "Dark2", name = "Action déclencheur") +
  labs(title = "Actions ayant changé le momentum")

print(p_changement_momentum)
