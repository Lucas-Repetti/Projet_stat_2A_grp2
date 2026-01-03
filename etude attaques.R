data <- actions_clees
head(data)

library(dplyr)
library(ggplot2)
library(tidyr)

# Mise en place
shots <- data %>%
  filter(LB_RESULTAT == "TIR") %>%
  mutate(
    but = LB_RESULTAT_DETAIL == "BUT"
  )

print(
  shots %>%
    summarise(
      tirs_totaux = n(),
      buts = sum(but),
      taux_but = mean(but)
    )
)



# Différente séquence d'attaque

tirs_par_attaque <- shots %>%
  group_by(LB_SEQUENCE_TYPE) %>%
  summarise(
    tirs = n(),
    buts = sum(but),
    taux_but = mean(but),
    .groups = "drop"
  ) %>%
  arrange(desc(tirs))

print(tirs_par_attaque)

ggplot(tirs_par_attaque,
       aes(x = reorder(LB_SEQUENCE_TYPE, tirs), y = tirs)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Volume de tirs par type d’attaque",
       x = "Type d’attaque",
       y = "Nombre de tirs") +
  theme_minimal()




# résultats

resultats_attaque <- data %>%
  filter(LB_SEQUENCE_TYPE %in% c("ATTAQUE PLACÉE", "CONTRE ATTAQUE")) %>%
  count(LB_RESULTAT) %>%
  mutate(pct = n / sum(n))

print(resultats_attaque)

ggplot(resultats_attaque,
       aes(x = reorder(LB_RESULTAT, -n), y = pct)) +
  geom_col(fill = "purple") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Répartition des résultats d’attaque",
       x = "Résultat",
       y = "Pourcentage") +
  theme_minimal()

# détails résultats

details_tirs <- shots %>%
  count(LB_RESULTAT_DETAIL) %>%
  mutate(pct = n / sum(n)) %>%
  arrange(desc(n))

print(details_tirs)

ggplot(details_tirs,
       aes(x = reorder(LB_RESULTAT_DETAIL, n), y = pct)) +
  geom_col(fill = "purple") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Détail des résultats des tirs",
       x = "Détail du tir",
       y = "Pourcentage") +
  theme_minimal()


# efficacité tirs → buts


efficacite_attaque <- shots %>%
  group_by(LB_SEQUENCE_TYPE) %>%
  summarise(
    tirs = n(),
    buts = sum(but),
    taux_but = mean(but),
    .groups = "drop"
  ) %>%
  arrange(desc(taux_but))

print(efficacite_attaque)

ggplot(efficacite_attaque,
       aes(x = reorder(LB_SEQUENCE_TYPE, taux_but), y = taux_but)) +
  geom_col(fill = "purple") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Efficacité des attaques (taux de but)",
       x = "Type d’attaque",
       y = "Taux de but") +
  theme_minimal()

#provocation fautes

fautes_attaque <- data %>%
  filter(LB_SEQUENCE_TYPE %in% c("ATTAQUE PLACÉE", "CONTRE ATTAQUE")) %>%
  group_by(LB_SEQUENCE_TYPE) %>%
  summarise(
    actions = n(),
    fautes = sum(LB_RESULTAT == "FAUTE", na.rm = TRUE),
    fautes_par_action = fautes / actions,
    .groups = "drop"
  )

print(fautes_attaque)

ggplot(fautes_attaque,
       aes(x = LB_SEQUENCE_TYPE, y = fautes_par_action)) +
  geom_col(fill = "purple") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Part des attaques se terminant par une faute",
       x = "Type d’attaque",
       y = "% d’actions avec faute") +
  theme_minimal()

# SYNTHESE :
# L’attaque placée concentre l’essentiel des actions et des tirs, mais avec une efficacité modérée (~55%).
# Les contre-attaques et les jets de 7m sont beaucoup plus efficaces, bien que nettement moins fréquents.
# La majorité des attaques aboutissent à un tir, tandis que la faute constitue une issue non négligeable du jeu placé.
# Ces résultats confirment l’importance du contexte d’attaque dans l’analyse de la performance offensive.