library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(slider)

tm_jeu <- actions_clees_details %>%
  arrange(CD_MATCH, ligne) %>%
  filter(
    TS_START_SEQUENCE < 29 |
      (TS_START_SEQUENCE >= 30 & TS_START_SEQUENCE < 59)
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

resume_temps_mort_jeu<- tm_jeu %>%
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


resume_temps_mort_jeu <- resume_temps_mort_jeu %>%
  mutate(
    impact_TM = case_when(
      evolution_score > 0 ~ "Bénéfique",
      evolution_score == 0 ~ "Neutre",
      evolution_score < 0 ~ "Négatif"
    )
  )

compte_efficacite_tm_jeu <- resume_temps_mort_jeu %>%
  group_by(impact_TM) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

ggplot(resume_temps_mort_jeu, aes(x = impact_TM)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Répartition des temps mort selon leur impact",
       x = "Statut d'efficacité",
       y = "Nombre") +
  theme_minimal()

#On regarde les actions avant le TM
tm_jeu_avant <- actions_clees_details %>%
  arrange(CD_MATCH, ligne) %>%
  filter(
    TS_START_SEQUENCE < 29 |
      (TS_START_SEQUENCE >= 30 & TS_START_SEQUENCE < 59)
  ) %>%
  mutate(
    flag_tm = LB_RESULTAT == "TEMPS MORT"
  ) %>%
  filter(flag_tm) %>%                              # on garde les TM
  select(CD_MATCH, ligne_tm = ligne) %>%           # ligne exacte du TM
  left_join(
    actions_clees_details,
    by = "CD_MATCH"
  ) %>%
  arrange(CD_MATCH, ligne) %>%
  filter(
    ligne <= ligne_tm &                            # inclut le TM
      ligne >= ligne_tm - 6                        # 6 lignes avant
  ) %>%
  group_by(CD_MATCH, ligne_tm) %>%                 # un bloc par TM
  arrange(ligne) %>%
  mutate( 
    id_tm_unique = cur_group_id()                  # <-- ID unique par temps mort
  ) %>%
  ungroup() %>%
  inner_join(
    matchs,
    by = "CD_MATCH"
  ) %>%
  rename(
    NB_SCORE_DOMICILE = NB_SCORE_DOMICILE.x,
    NB_SCORE_EXTERIEUR = NB_SCORE_EXTERIEUR.x
  )




resume_temps_mort_jeu_avant<- tm_jeu_avant %>%
  inner_join(
    score_final_match,
    by = "CD_MATCH") %>%
  group_by(CD_MATCH, id_tm_unique) %>%
  summarise(
    Temps = last(TS_START_SEQUENCE),
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
    
    buts_marques_avant = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      last(NB_SCORE_DOMICILE) - first(NB_SCORE_DOMICILE),
      last(NB_SCORE_EXTERIEUR) - first(NB_SCORE_EXTERIEUR)
    ),
    
    buts_encaisses_avant = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      last(NB_SCORE_EXTERIEUR) - first(NB_SCORE_EXTERIEUR),
      last(NB_SCORE_DOMICILE) - first(NB_SCORE_DOMICILE)
    ),
    
    pertes_balle_avant = sum(
      LB_RESULTAT == "PERTE DE BALLE" &
        CD_CLUB == CD_CLUB_TM,
      na.rm = TRUE
    ),
    
    recuperation_balle_avant = sum(
      LB_RESULTAT == "PERTE DE BALLE" &
        CD_CLUB != CD_CLUB_TM,
      na.rm = TRUE
    ),
    
    diff_5actions_avant_TM = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      first(NB_SCORE_DOMICILE) - first(NB_SCORE_EXTERIEUR),
      first(NB_SCORE_EXTERIEUR) - first(NB_SCORE_DOMICILE)
    ),
    
    diff_TM = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      last(NB_SCORE_DOMICILE) - last(NB_SCORE_EXTERIEUR),
      last(NB_SCORE_EXTERIEUR) - last(NB_SCORE_DOMICILE)
    ),
    
    evolution_score_avant = diff_TM - diff_5actions_avant_TM,
    
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

efficacite_TM_jeu<-resume_temps_mort_jeu %>%
  inner_join(
    resume_temps_mort_jeu_avant,
    by = c("id_tm" = "id_tm_unique"))%>%
  rename(
    CD_MATCH = CD_MATCH.x,
    Temps = Temps.x,
    CD_CLUB_TM = CD_CLUB_TM.x,
    score_equipe_TM = score_equipe_TM.x,
    score_autre_equipe = score_autre_equipe_.x,
    score_final_equipe_tm = score_final_equipe_tm.x,
    score_final_autre_equipe = score_final_autre_equipe.x
  ) %>%
  select(CD_MATCH,
         id_tm,
         Temps,
         CD_CLUB_TM,
         score_equipe_TM,
         score_autre_equipe,
         buts_marques_avant,
         buts_encaisses_avant,
         pertes_balle_avant,
         recuperation_balle_avant,
         diff_5actions_avant_TM,
         diff_TM,
         evolution_score_avant,
         buts_marques,
         buts_encaisses,
         pertes_balle,
         recuperation_balle,
         diff_apres_TM,
         evolution_score,
         score_final_equipe_tm,
         score_final_autre_equipe
  )

efficacite_TM_jeu <- efficacite_TM_jeu %>%
  mutate(
    impact_TM = case_when(
      evolution_score > 0 ~ "Bénéfique",
      evolution_score == 0 ~ "Neutre",
      evolution_score < 0 ~ "Négatif"
    ),
    comparaison_avant_apres = case_when(
      evolution_score>evolution_score_avant+3 ~ "Temps-mort à impact exceptionnel",
      evolution_score>evolution_score_avant+2 ~ "Temps-mort très efficace",
      evolution_score>evolution_score_avant+1 ~ "Temps-mort efficace",
      evolution_score>evolution_score_avant ~ "Faible impact",
      evolution_score==evolution_score_avant ~ "Pas de changement de dynamique",
      evolution_score<evolution_score_avant ~ "Temps-mort contre-productif",
    )
  )

compte_efficacite_tm_jeu <- efficacite_TM_jeu %>%
  group_by(impact_TM) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

ggplot(efficacite_TM_jeu, aes(x = impact_TM)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Répartition des temps mort selon leur impact",
       x = "Statut d'efficacité",
       y = "Nombre") +
  theme_minimal()

compte_comparaison_av_ap_tm_jeu <- efficacite_TM_jeu %>%
  group_by(comparaison_avant_apres) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

ggplot(efficacite_TM_jeu, aes(x = comparaison_avant_apres)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Répartition des temps mort selon leur impact",
       x = "Statut d'efficacité",
       y = "Nombre") +
  theme_minimal()
