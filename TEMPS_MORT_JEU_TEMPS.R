library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)

tm_jeu_temps <- actions_clees_details %>%
  arrange(CD_MATCH, ligne) %>%
  filter(
    TS_START_SEQUENCE < 29 |
      (TS_START_SEQUENCE >= 30 & TS_START_SEQUENCE < 59)
  ) %>%
  mutate(
    flag_tm = LB_RESULTAT == "TEMPS MORT",
    id_tm = cumsum(flag_tm)
  ) %>%
  mutate(
    TS_START_SEQUENCE_sec = case_when(
      str_detect(TS_START_SEQUENCE, "^\\d{1,2}:\\d{2}$") ~
        period_to_seconds(ms(TS_START_SEQUENCE)),
      
      str_detect(TS_START_SEQUENCE, "^\\d{1,2}:\\d{2}:\\d{2}$") ~
        period_to_seconds(hms(TS_START_SEQUENCE)),
      
      str_detect(TS_START_SEQUENCE, "^\\d+$") ~
        as.numeric(TS_START_SEQUENCE),
      
      TRUE ~ NA_real_
    )
  ) %>%
  filter(id_tm > 0) %>%
  group_by(CD_MATCH, id_tm) %>%
  mutate(
    ts_tm = TS_START_SEQUENCE_sec[flag_tm][1]   # temps du temps-mort
  ) %>%
  filter(
    TS_START_SEQUENCE_sec >= ts_tm,
    TS_START_SEQUENCE_sec <= ts_tm + seconds(180)
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

resume_temps_mort_jeu_temps<- tm_jeu_temps %>%
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

resume_temps_mort_jeu_temps <- resume_temps_mort_jeu_temps %>%
  mutate(
    impact_TM = case_when(
      evolution_score > 0 ~ "Bénéfique",
      evolution_score == 0 ~ "Neutre",
      evolution_score < 0 ~ "Négatif"
    )
  )

resume_temps_mort_jeu_temps %>%
  group_by(impact_TM) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

ggplot(resume_temps_mort_jeu_temps, aes(x = impact_TM)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Répartition des temps mort selon leur impact",
       x = "Statut d'efficacité",
       y = "Nombre") +
  theme_minimal()

#On regarde les actions avant le TM
tm_jeu_avant_temps <- actions_clees_details %>%
  arrange(CD_MATCH, ligne) %>%
  filter(
    TS_START_SEQUENCE < 29 |
      (TS_START_SEQUENCE >= 30 & TS_START_SEQUENCE < 59)
  ) %>%
  mutate(
    flag_tm = LB_RESULTAT == "TEMPS MORT"
  ) %>%
  filter(flag_tm) %>%                              # on garde les TM
  mutate(
    ts_tm_sec = case_when(
      str_detect(TS_START_SEQUENCE, "^\\d{1,2}:\\d{2}$") ~
        period_to_seconds(ms(TS_START_SEQUENCE)),
      str_detect(TS_START_SEQUENCE, "^\\d{1,2}:\\d{2}:\\d{2}$") ~
        period_to_seconds(hms(TS_START_SEQUENCE)),
      str_detect(TS_START_SEQUENCE, "^\\d+$") ~
        as.numeric(TS_START_SEQUENCE),
      TRUE ~ NA_real_
    )
  ) %>%
  select(CD_MATCH, ts_tm_sec) %>%                  # on garde le temps du TM
  left_join(
    actions_clees_details,
    by = "CD_MATCH"
  ) %>%
  mutate(
    TS_START_SEQUENCE_sec = case_when(
      str_detect(TS_START_SEQUENCE, "^\\d{1,2}:\\d{2}$") ~
        period_to_seconds(ms(TS_START_SEQUENCE)),
      str_detect(TS_START_SEQUENCE, "^\\d{1,2}:\\d{2}:\\d{2}$") ~
        period_to_seconds(hms(TS_START_SEQUENCE)),
      str_detect(TS_START_SEQUENCE, "^\\d+$") ~
        as.numeric(TS_START_SEQUENCE),
      TRUE ~ NA_real_
    )
  ) %>%
  arrange(CD_MATCH, ligne) %>%
  filter(
    TS_START_SEQUENCE_sec <= ts_tm_sec &
      TS_START_SEQUENCE_sec >= ts_tm_sec - 180
  ) %>%
  group_by(CD_MATCH, ts_tm_sec) %>%
  mutate(
    id_tm_unique = cur_group_id()
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

resume_temps_mort_jeu_avant_temps<- tm_jeu_avant_temps %>%
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
    
    diff_180s_avant_TM = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      first(NB_SCORE_DOMICILE) - first(NB_SCORE_EXTERIEUR),
      first(NB_SCORE_EXTERIEUR) - first(NB_SCORE_DOMICILE)
    ),
    
    diff_TM = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      last(NB_SCORE_DOMICILE) - last(NB_SCORE_EXTERIEUR),
      last(NB_SCORE_EXTERIEUR) - last(NB_SCORE_DOMICILE)
    ),
    
    evolution_score_avant = diff_TM - diff_180s_avant_TM,
    
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

efficacite_TM_jeu_temps<-resume_temps_mort_jeu_temps %>%
  inner_join(
    resume_temps_mort_jeu_avant_temps,
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
         diff_180s_avant_TM,
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

efficacite_TM_jeu_temps <- efficacite_TM_jeu_temps %>%
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
    ),
    situation_score = case_when(
      diff_TM < 0  ~ "mené",
      diff_TM == 0 ~ "égalité",
      diff_TM > 0  ~ "avance"
    ),
    momentum = case_when(
      evolution_score_avant<0 ~ "négatif",
      evolution_score_avant==0 ~ "neutre",
      evolution_score_avant>0 ~ "positif"
    )
  )

efficacite_TM_jeu_temps %>%
  group_by(situation_score) %>%
  summarise(
    n = n(),
    impact_moyen = mean(evolution_score, na.rm = TRUE),
    impact_mediane = median(evolution_score, na.rm = TRUE),
    sd = sd(evolution_score, na.rm = TRUE)
  )

ggplot(efficacite_TM_jeu_temps,
       aes(x = situation_score, y = evolution_score)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Impact du temps mort selon la situation de score",
    x = "Situation au moment du TM",
    y = "Impact du TM (évolution du score)"
  )

efficacite_TM_jeu_temps %>%
  group_by(impact_TM) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

ggplot(efficacite_TM_jeu_temps, aes(x = impact_TM)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Répartition des temps mort selon leur impact",
       x = "Statut d'efficacité",
       y = "Nombre") +
  theme_minimal()

compte_comparaison_av_ap_tm_jeu_temps <- efficacite_TM_jeu_temps %>%
  group_by(comparaison_avant_apres) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

ggplot(efficacite_TM_jeu_temps, aes(x = comparaison_avant_apres)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Répartition des temps mort selon leur impact",
       x = "Statut d'efficacité",
       y = "Nombre") +
  theme_minimal()


kruskal.test(evolution_score ~ situation_score, data = efficacite_TM_jeu_temps)
#Pas de différence statistiquement significative de l’impact du TM selon le score au moment du TM



#On va désormais chercher à trier les temps-mort selon le score
#Score négatif
tm_jeu_score_négatif_temps<-efficacite_TM_jeu_temps %>%
  filter(diff_TM<0)

tm_jeu_score_négatif_temps %>%
  group_by(impact_TM) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

tm_jeu_score_négatif_temps %>%
  group_by(comparaison_avant_apres) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

#Egalité
tm_jeu_egalite_temps <- efficacite_TM_jeu_temps %>%
  filter(diff_TM==0)

tm_jeu_egalite_temps %>%
  group_by(impact_TM) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

tm_jeu_egalite_temps %>%
  group_by(comparaison_avant_apres) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

#Positif
tm_jeu_score_positif_temps <-efficacite_TM_jeu_temps %>%
  filter(diff_TM>0)

tm_jeu_score_positif_temps %>%
  group_by(impact_TM) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

tm_jeu_score_positif_temps %>%
  group_by(comparaison_avant_apres) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

#On peut aussi trier selon le momentum
#Momentum négatif
tm_jeu_momentum_négatif_temps<-efficacite_TM_jeu_temps %>%
  filter(evolution_score_avant<0)

tm_jeu_momentum_négatif_temps %>%
  group_by(impact_TM) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

tm_jeu_momentum_négatif_temps %>%
  group_by(comparaison_avant_apres) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

#Momentum positif
tm_jeu_momentum_positif_temps<-efficacite_TM_jeu_temps %>%
  filter(evolution_score_avant>0)

tm_jeu_momentum_positif_temps %>%
  group_by(impact_TM) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

tm_jeu_momentum_positif_temps %>%
  group_by(comparaison_avant_apres) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

#Momentum égale
tm_jeu_momentum_egal_temps<-efficacite_TM_jeu_temps %>%
  filter(evolution_score_avant==0)

tm_jeu_momentum_egal_temps %>%
  group_by(impact_TM) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

tm_jeu_momentum_egal_temps %>%
  group_by(comparaison_avant_apres) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

#Table de contingence
tab <- table(
  efficacite_TM_jeu_temps$momentum,
  efficacite_TM_jeu_temps$comparaison_avant_apres
)
tab
prop.table(tab, margin = 1)

#On va désormais chercher à distinguer la période à laquelle ont été pris les temps-morts
efficacite_TM_jeu_temps <- efficacite_TM_jeu_temps %>%
  mutate(
    periode_temps_mort = case_when(
      Temps >= 0  & Temps < 15 ~ "Round d'observation",
      Temps >= 15 & Temps < 29 ~ "Milieu de première mi-temps",
      Temps >= 30 & Temps < 45 ~ "Début de seconde mi-temps",
      Temps >= 45 & Temps < 59 ~ "Money Time",
      TRUE ~ "Hors période"
    )
  )

#Table de contingence
tab2 <- table(
  efficacite_TM_jeu_temps$comparaison_avant_apres,
  efficacite_TM_jeu_temps$periode_temps_mort
)
tab2
prop.table(tab2, margin = 1)

efficacite_TM_jeu_temps %>%
  group_by(periode_temps_mort) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

# Voir khi-deux (table effectifs théoriques, pour voir s'il y a une dépendance entre la période et l'efficacité du temps-mort)
tab_theo <- chisq.test(tab2)$expected
tab_theo
test <- chisq.test(tab2)
test
# Les variables ne sont pas indépendantes
tab_complet <- data.frame(
  Modalite_X = rep(rownames(tab2), times = ncol(tab2)),
  Modalite_Y = rep(colnames(tab2), each = nrow(tab2)),
  Observe = as.vector(tab2),
  Theorique = round(as.vector(tab_theo), 2),
  Ecart = round(as.vector((tab2 - tab_theo) / tab_theo * 100), 2)
)

tab_complet
#TM à impact exceptionnel au début du match et pas de changement de dynamique au début de seconde mi-temps sur-représentés
#Pas de changement de dynamique au début du match et TM à impact exceptionnel sous-représentés

#On va regarder quels ont été les TM les plus efficaces
plus_grande_evolution_du_score<-efficacite_TM_jeu_temps %>%
  arrange(desc(evolution_score))%>%
  filter(evolution_score>=2)

plus_grande_evolution_du_score%>%
  group_by(CD_CLUB_TM) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

#Nantes est l'équipe qui a le plus de bonne évolution du score après TM
tm_nantes<-efficacite_TM_jeu_temps %>%
  filter(CD_CLUB_TM=="FR_HBCN_44200") %>%
  arrange(desc(evolution_score))

efficacite_tm_nantes_selon_periode<-tm_nantes%>%
  group_by(periode_temps_mort,comparaison_avant_apres) %>%
  summarise(
    Nombre = n(),
    .groups = "drop"
  )

#Premier TM : -3 sur les 3 minutes avant TM,+4 sur les 3 minutes après
#Nantes bcp de TM en milieu de première période
#Alberto Enterrios, coach de l'époque : il se distingue lors de la saison 2020-2021 en étant élu meilleur entraîneur à la fois de la Ligue des champions et du Championnat de France.

#Creer une table de récapitulatif par équipe

tm_equipe<-efficacite_TM_jeu_temps %>%
  group_by(CD_CLUB_TM) %>%
  mutate()
