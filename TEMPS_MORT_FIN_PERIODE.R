library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(slider)

tm_fin_periode <- actions_clees_details %>%
  arrange(CD_MATCH, ligne) %>%
  filter(
    TS_START_SEQUENCE >= 29  & TS_START_SEQUENCE <= 30|
      (TS_START_SEQUENCE >= 59)
  ) %>%
  mutate(
    flag_tm = LB_RESULTAT == "TEMPS MORT",
    id_tm = cumsum(flag_tm)
  ) %>%                   
  filter(id_tm > 0) %>%
  group_by(id_tm) %>%
  slice_head(n = 7) %>%
  ungroup() %>%
  inner_join(
    matchs,
    by = "CD_MATCH"
  ) %>%
  rename(
    NB_SCORE_DOMICILE = NB_SCORE_DOMICILE.x,
    NB_SCORE_EXTERIEUR = NB_SCORE_EXTERIEUR.x
  )

score_final_match <- actions_clees_details %>%
  arrange(CD_MATCH, ligne) %>%
  group_by(CD_MATCH) %>%
  slice_tail(n = 1) %>%   # dernière action du match
  ungroup() %>%
  select(
    CD_MATCH,
    score_final_dom = NB_SCORE_DOMICILE,
    score_final_ext = NB_SCORE_EXTERIEUR
  )

resume_temps_mort_fin_periode<- tm_fin_periode %>%
  inner_join(
    score_final_match,
    by = "CD_MATCH") %>%
  group_by(CD_MATCH, id_tm) %>%
  summarise(
    Temps = first(TS_START_SEQUENCE),
    CD_CLUB_TM = first(CD_CLUB[LB_RESULTAT == "TEMPS MORT"]),
    
    score_equipe_TM = if_else (
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      first(NB_SCORE_DOMICILE[LB_RESULTAT == "TEMPS MORT"]),
      first(NB_SCORE_EXTERIEUR[LB_RESULTAT == "TEMPS MORT"])
    ),
    
    score_autre_equipe_ = if_else (
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      first(NB_SCORE_EXTERIEUR[LB_RESULTAT == "TEMPS MORT"]),
      first(NB_SCORE_DOMICILE[LB_RESULTAT == "TEMPS MORT"])
    ),
    
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
    
    score_final_equipe_tm = if_else (
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      first(score_final_dom),
      first(score_final_ext)),
    
    score_final_autre_equipe = if_else (
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      first(score_final_ext),
      first(score_final_dom)),
    
    .groups = "drop"
  ) %>%
  filter(!is.na(CD_CLUB_TM))


resume_temps_mort_fin_periode <- resume_temps_mort_fin_periode %>%
  mutate(
    impact_TM = case_when(
      evolution_score > 0 ~ "Bénéfique",
      evolution_score == 0 ~ "Neutre",
      evolution_score < 0 ~ "Négatif"
    )
  )

compte_efficacite_tm_fin_periode <- resume_temps_mort_fin_periode %>%
  group_by(impact_TM) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

ggplot(resume_temps_mort_fin_periode, aes(x = impact_TM)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Répartition des temps mort selon leur impact",
       x = "Statut d'efficacité",
       y = "Nombre") +
  theme_minimal()



