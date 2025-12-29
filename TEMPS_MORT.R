library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

actions_clees<- actions_clees %>%
  mutate(ligne=row_number())

temps_mort <- actions_clees %>%
  filter(LB_RESULTAT == "TEMPS MORT") %>%
  select(CD_MATCH, CD_CLUB, ligne)

actions_apres_temps_mort <- actions_clees %>%
  inner_join(
    temps_mort,
    by = "CD_MATCH"
  ) %>%
  filter(
    ligne.x >= ligne.y &
      ligne.x <= ligne.y + 5
  ) %>%
  select(-ligne.y) %>%
  rename(ligne = ligne.x) %>%
  rename(CD_CLUB=CD_CLUB.x) %>%
  select(
    CD_MATCH,
    LB_RESULTAT,
    LB_RESULTAT_DETAIL,
    CD_CLUB,
    NB_SCORE_DOMICILE,
    NB_SCORE_EXTERIEUR
  )

#On va maintenant chercher à  voir si le temps mort a été bénéfique à l'équipe qui l'a pris
tm <- actions_clees %>%
  arrange(CD_MATCH, ligne) %>%   # très important
  group_by(CD_MATCH) %>%
  mutate(
    flag_tm = LB_RESULTAT == "TEMPS MORT",
    id_tm = cumsum(flag_tm)
  ) %>%
  filter(id_tm > 0) %>%
  group_by(CD_MATCH, id_tm) %>%
  slice_head(n = 6) %>%   # TM + max 5 actions
  ungroup() %>%
  inner_join(
    matchs,
    by = "CD_MATCH"
  ) %>%
  rename(
    NB_SCORE_DOMICILE = NB_SCORE_DOMICILE.x,
    NB_SCORE_EXTERIEUR = NB_SCORE_EXTERIEUR.x
  )


resume_temps_mort <- tm %>%
  group_by(CD_MATCH, id_tm) %>%
  summarise(
    CD_CLUB_TM = first(CD_CLUB[LB_RESULTAT == "TEMPS MORT"]),
    buts_marques = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      last(NB_SCORE_DOMICILE) - first(NB_SCORE_DOMICILE),
      last(NB_SCORE_EXTERIEUR) - first(NB_SCORE_EXTERIEUR)
    ),
    
    buts_encaisses = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      last(NB_SCORE_EXTERIEUR) - first(NB_SCORE_EXTERIEUR),
      last(NB_SCORE_DOMICILE) - first(NB_SCORE_DOMICILE)
    ),
    
    pertes_balle = sum(
      LB_RESULTAT == "PERTE DE BALLE" &
        CD_CLUB == CD_CLUB_TM,
      na.rm = TRUE
    ),
    
    recuperation_balle = sum(
      LB_RESULTAT == "PERTE DE BALLE" &
        CD_CLUB != CD_CLUB_TM,
      na.rm = TRUE
    ),
    
    diff_avant_TM = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      first(NB_SCORE_DOMICILE) - first(NB_SCORE_EXTERIEUR),
      first(NB_SCORE_EXTERIEUR) - first(NB_SCORE_DOMICILE)
    ),
    
    diff_apres_TM = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      last(NB_SCORE_DOMICILE) - last(NB_SCORE_EXTERIEUR),
      last(NB_SCORE_EXTERIEUR) - last(NB_SCORE_DOMICILE)
    ),
    
    evolution_score = diff_apres_TM - diff_avant_TM,
    
    .groups = "drop"
  )


resume_temps_mort <- resume_temps_mort %>%
  mutate(
    impact_TM = case_when(
      evolution_score > 0 ~ "Bénéfique",
      evolution_score == 0 ~ "Neutre",
      evolution_score < 0 ~ "Négatif"
    )
  )

compte_efficacite_tm <- resume_temps_mort %>%
  group_by(impact_TM) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

ggplot(resume_temps_mort, aes(x = impact_TM)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Répartition des temps mort selon leur impact",
       x = "Statut d'efficacité",
       y = "Nombre") +
  theme_minimal()

#A court-terme, le temps-mort est plutôt bénéfique

tm10 <- actions_clees %>%
  arrange(CD_MATCH, ligne) %>%   # très important
  group_by(CD_MATCH) %>%
  mutate(
    flag_tm = LB_RESULTAT == "TEMPS MORT",
    id_tm = cumsum(flag_tm)
  ) %>%
  filter(id_tm > 0) %>%
  group_by(CD_MATCH, id_tm) %>%
  slice_head(n = 11) %>%   # TM + max 10 actions
  ungroup() %>%
  inner_join(
    matchs,
    by = "CD_MATCH"
  ) %>%
  rename(
    NB_SCORE_DOMICILE = NB_SCORE_DOMICILE.x,
    NB_SCORE_EXTERIEUR = NB_SCORE_EXTERIEUR.x
  )


resume_temps_mort10 <- tm10 %>%
  group_by(CD_MATCH, id_tm) %>%
  summarise(
    CD_CLUB_TM = first(CD_CLUB[LB_RESULTAT == "TEMPS MORT"]),
    buts_marques = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      last(NB_SCORE_DOMICILE) - first(NB_SCORE_DOMICILE),
      last(NB_SCORE_EXTERIEUR) - first(NB_SCORE_EXTERIEUR)
    ),
    
    buts_encaisses = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      last(NB_SCORE_EXTERIEUR) - first(NB_SCORE_EXTERIEUR),
      last(NB_SCORE_DOMICILE) - first(NB_SCORE_DOMICILE)
    ),
    
    pertes_balle = sum(
      LB_RESULTAT == "PERTE DE BALLE" &
        CD_CLUB == CD_CLUB_TM,
      na.rm = TRUE
    ),
    
    recuperation_balle = sum(
      LB_RESULTAT == "PERTE DE BALLE" &
        CD_CLUB != CD_CLUB_TM,
      na.rm = TRUE
    ),
    
    diff_avant_TM = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      first(NB_SCORE_DOMICILE) - first(NB_SCORE_EXTERIEUR),
      first(NB_SCORE_EXTERIEUR) - first(NB_SCORE_DOMICILE)
    ),
    
    diff_apres_TM = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      last(NB_SCORE_DOMICILE) - last(NB_SCORE_EXTERIEUR),
      last(NB_SCORE_EXTERIEUR) - last(NB_SCORE_DOMICILE)
    ),
    
    evolution_score = diff_apres_TM - diff_avant_TM,
    
    .groups = "drop"
  )


resume_temps_mort10 <- resume_temps_mort10 %>%
  mutate(
    impact_TM = case_when(
      evolution_score > 0 ~ "Bénéfique",
      evolution_score == 0 ~ "Neutre",
      evolution_score < 0 ~ "Négatif"
    )
  )

compte_efficacite_tm10 <- resume_temps_mort10 %>%
  group_by(impact_TM) %>%
  summarise(
    Efficacité = n(),
    .groups = "drop"
  )

ggplot(resume_temps_mort10, aes(x = impact_TM)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Répartition des temps mort selon leur impact",
       x = "Statut d'efficacité",
       y = "Nombre") +
  theme_minimal()

# A long-terme, il y a plus de tm bénéfiques mais aussi plus de tm négatifs (seulement la part de tm neutre diminue)
#Mais cela dépend aussi du niveau de l'équipe, puisque les équipes n'ont pas un niveau égal par ailleurs
#Donc on va devoir comparer à la dynamique d'avant temps-mort
