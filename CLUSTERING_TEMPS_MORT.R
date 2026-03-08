library(dplyr)
library(stringr)
library(lubridate)
library(purrr)


actions_clees<- actions_clees %>%
  arrange(CD_MATCH, TS_START_SEQUENCE) %>%
  mutate(ligne=row_number())

tm_jeu_temps_4 <- actions_clees %>%
  arrange(CD_MATCH, ligne) %>%
  mutate(
    flag_tm = LB_RESULTAT == "TEMPS MORT",
    id_tm = cumsum(flag_tm)
  ) %>%
  mutate(
    TS_START_SEQUENCE_sec = case_when(
      str_detect(TS_START_SEQUENCE, "^\\d{1,2}:\\d{2}$") ~ period_to_seconds(ms(TS_START_SEQUENCE)),
      str_detect(TS_START_SEQUENCE, "^\\d{1,2}:\\d{2}:\\d{2}$") ~ period_to_seconds(hms(TS_START_SEQUENCE)),
      str_detect(TS_START_SEQUENCE, "^\\d+$") ~ as.numeric(TS_START_SEQUENCE),
      TRUE ~ NA_real_
    )
  ) %>%
  filter(id_tm > 0) %>%
  group_by(CD_MATCH, id_tm) %>%
  mutate(
    ts_tm = TS_START_SEQUENCE_sec[flag_tm][1]
  ) %>%
  ungroup() %>%
  # Calculer le temps du prochain TM pour chaque ligne
  group_by(CD_MATCH) %>%
  arrange(TS_START_SEQUENCE_sec, .by_group = TRUE) %>%
  mutate(
    # créer un identifiant unique pour chaque TM
    id_tm_ligne = cumsum(flag_tm),
    # pour chaque ligne, ts_next_tm = min des TS_START_SEQUENCE_sec des TM > ligne actuelle
    ts_next_tm = map_dbl(row_number(), function(i){
      futurs_tm <- TS_START_SEQUENCE_sec[flag_tm & row_number() > i]
      if(length(futurs_tm) == 0) Inf else min(futurs_tm)
    })
  ) %>%
  ungroup() %>%
  # filtrer les lignes dans la fenêtre
  filter(
    TS_START_SEQUENCE_sec >= ts_tm,
    TS_START_SEQUENCE_sec <= pmin(ts_tm + 300, ts_next_tm)
  ) %>%
  inner_join(matchs, by = "CD_MATCH") %>%
  rename(
    NB_SCORE_DOMICILE = NB_SCORE_DOMICILE.x,
    NB_SCORE_EXTERIEUR = NB_SCORE_EXTERIEUR.x
  )

actions_cleess <- actions_clees %>%
  # nettoyer les valeurs corrompues
  mutate(
    TS_START_SEQUENCE = trimws(TS_START_SEQUENCE),
    TS_START_SEQUENCE = ifelse(TS_START_SEQUENCE %in% c("", "NA", "--", "."), NA, TS_START_SEQUENCE),
    
    # convertir en secondes
    TS_START_SEQUENCE_sec = case_when(
      
      # format mm:ss
      str_detect(TS_START_SEQUENCE, "^\\d{1,2}:\\d{2}$") ~ {
        m <- str_split_fixed(TS_START_SEQUENCE, ":", 2)
        as.numeric(m[,1])*60 + as.numeric(m[,2])
      },
      
      # format hh:mm:ss
      str_detect(TS_START_SEQUENCE, "^\\d{1,2}:\\d{2}:\\d{2}$") ~ {
        h <- str_split_fixed(TS_START_SEQUENCE, ":", 3)
        as.numeric(h[,1])*3600 + as.numeric(h[,2])*60 + as.numeric(h[,3])
      },
      
      # format nombre pur (secondes)
      str_detect(TS_START_SEQUENCE, "^\\d+$") ~ as.numeric(TS_START_SEQUENCE),
      
      # tout autre cas → NA
      TRUE ~ NA_real_
    )
  )

tm_jeu_avant_temps_4 <- actions_cleess %>%
  arrange(CD_MATCH, ligne) %>%
  # flag pour toutes les lignes
  mutate(flag_tm = LB_RESULTAT == "TEMPS MORT") %>%
  # calcul du temps du TM
  group_by(CD_MATCH) %>%
  mutate(
    ts_tm_sec = if_else(flag_tm,
                        case_when(
                          str_detect(TS_START_SEQUENCE, "^\\d{1,2}:\\d{2}$") ~ period_to_seconds(ms(TS_START_SEQUENCE)),
                          str_detect(TS_START_SEQUENCE, "^\\d{1,2}:\\d{2}:\\d{2}$") ~ period_to_seconds(hms(TS_START_SEQUENCE)),
                          str_detect(TS_START_SEQUENCE, "^\\d+$") ~ as.numeric(TS_START_SEQUENCE),
                          TRUE ~ NA_real_
                        ),
                        NA_real_)
  ) %>%
  ungroup() %>%
  mutate(TS_START_SEQUENCE_sec = case_when(
    str_detect(TS_START_SEQUENCE, "^\\d{1,2}:\\d{2}$") ~ period_to_seconds(ms(TS_START_SEQUENCE)),
    str_detect(TS_START_SEQUENCE, "^\\d{1,2}:\\d{2}:\\d{2}$") ~ period_to_seconds(hms(TS_START_SEQUENCE)),
    str_detect(TS_START_SEQUENCE, "^\\d+$") ~ as.numeric(TS_START_SEQUENCE),
    TRUE ~ NA_real_
  )) %>%
  arrange(CD_MATCH, ligne) %>%
  group_by(CD_MATCH) %>%
  mutate(
    # id unique par TM
    id_tm_ligne = cumsum(flag_tm),
    # temps du TM précédent
    ts_prev_tm = map_dbl(row_number(), function(i){
      precedents_tm <- TS_START_SEQUENCE_sec[flag_tm & row_number() < i]
      if(length(precedents_tm) == 0) 0 else max(precedents_tm)
    }),
    # temps du TM courant pour chaque ligne (plus simple)
    ts_tm_sec = map_dbl(row_number(), function(i){
      tms_apres <- TS_START_SEQUENCE_sec[flag_tm & row_number() >= i]
      if(length(tms_apres) == 0) NA_real_ else min(tms_apres)
    })
  ) %>%
  ungroup() %>%
  # garder les lignes dans la fenêtre 5 min avant le TM courant ou jusqu'au TM précédent
  filter(
    TS_START_SEQUENCE_sec <= ts_tm_sec &
      TS_START_SEQUENCE_sec >= pmax(ts_tm_sec - 300, ts_prev_tm)
  ) %>%
  group_by(CD_MATCH, ts_tm_sec) %>%
  mutate(
    id_tm_unique = cur_group_id()
  ) %>%
  ungroup() %>%
  inner_join(matchs, by = "CD_MATCH") %>%
  rename(
    NB_SCORE_DOMICILE = NB_SCORE_DOMICILE.x,
    NB_SCORE_EXTERIEUR = NB_SCORE_EXTERIEUR.x
  ) %>%
  filter(!CD_CLUB=='')




tm_jeu_avant_temps_4<-tm_jeu_avant_temps_4 %>%
  mutate(CD_CLUB_TM = first(CD_CLUB[LB_RESULTAT == "TEMPS MORT"]))

test_serie_cluster <- tm_jeu_avant_temps_4 %>%
  filter(LB_RESULTAT_DETAIL == "BUT" | LB_RESULTAT == "TEMPS MORT") %>%
  group_by(id_tm_unique) %>%
  arrange(desc(TS_START_SEQUENCE_sec), .by_group = TRUE)

test_serie_cluster<- test_serie_cluster %>%
  mutate(CD_CLUB_TM = first(CD_CLUB[LB_RESULTAT == "TEMPS MORT"]))


test_serie_cluster <- test_serie_cluster %>%
  group_by(id_tm_unique) %>%
  arrange(TS_START_SEQUENCE_sec, .by_group = TRUE) %>%
  mutate(
    indice_serie_avant_tm = {
      
      club_tm <- first(CD_CLUB_TM)
      
      buts <- cur_data() %>%
        filter(LB_RESULTAT_DETAIL == "BUT")
      
      if (nrow(buts) == 0) {
        0
      } else {
        signes <- ifelse(buts$CD_CLUB == club_tm, 1, -1)
        r <- rle(signes)
        tail(r$values, 1) * tail(r$lengths, 1)
      }
    }
  ) %>%
  ungroup()

test_serie_cluster<-test_serie_cluster %>%
  filter(LB_RESULTAT=="TEMPS MORT")%>%
  select(id_tm_unique,
         indice_serie_avant_tm,
         ligne)

score_final_match <- actions_cleess %>%
  arrange(CD_MATCH, ligne) %>%
  group_by(CD_MATCH) %>%
  slice_tail(n = 1) %>%   # dernière action du match
  ungroup() %>%
  select(
    CD_MATCH,
    score_final_dom = NB_SCORE_DOMICILE,
    score_final_ext = NB_SCORE_EXTERIEUR
  )

resume_temps_mort_jeu_avant_temps_4<- tm_jeu_avant_temps_4 %>%
  inner_join(
    score_final_match,
    by = "CD_MATCH") %>%
  group_by(CD_MATCH, id_tm_unique) %>%
  slice_tail(n=1)%>%
  summarise(
    ligne,
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
    
    
    diff_300s_avant_TM = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      first(NB_SCORE_DOMICILE) - first(NB_SCORE_EXTERIEUR),
      first(NB_SCORE_EXTERIEUR) - first(NB_SCORE_DOMICILE)
    ),
    
    diff_TM = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      last(NB_SCORE_DOMICILE) - last(NB_SCORE_EXTERIEUR),
      last(NB_SCORE_EXTERIEUR) - last(NB_SCORE_DOMICILE)
    ),
    
    evolution_score_avant = diff_TM - diff_300s_avant_TM,
    
    score_final_equipe_tm = if_else (
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      first(score_final_dom),
      first(score_final_ext)),
    
    score_final_autre_equipe = if_else (
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      first(score_final_ext),
      first(score_final_dom)),
    
    vainqueur_final = if_else(
      score_final_dom<score_final_ext, 
      CD_CLUB_EXTERIEUR,
      if_else(
        score_final_dom>score_final_ext,
        CD_CLUB_DOMICILE,
        "Egalité"
      )
      
    ),
    
    .groups = "drop"
  ) %>%
  filter(!is.na(CD_CLUB_TM))%>%
  filter(!CD_CLUB_TM=='')



valeurs_avant <- resume_temps_mort_jeu_avant_temps_4$ligne

diff_lignes <- which(!(test_serie_cluster$ligne %in% valeurs_avant))
diff_lignes

test_serie_cluster <- test_serie_cluster[-c(944, 1160, 1490, 1556, 1611, 2698, 4230, 6089), ]
test_serie_cluster$id_tm_unique <- seq_len(nrow(test_serie_cluster))
resume_temps_mort_jeu_avant_temps_4$id_tm_unique<- seq_len(nrow(resume_temps_mort_jeu_avant_temps_4))

resume_temps_mort_jeu_avant_temps_4<-resume_temps_mort_jeu_avant_temps_4 %>%
  inner_join(test_serie_cluster, by = "id_tm_unique") %>%
  select(ligne.x,
         CD_MATCH,
         id_tm_unique,
         Temps,
         CD_CLUB_TM,
         score_equipe_TM,
         score_autre_equipe_,
         indice_serie_avant_tm,
         diff_300s_avant_TM,
         diff_TM,
         evolution_score_avant,
         score_final_equipe_tm,
         score_final_autre_equipe,
         vainqueur_final)

resume_temps_mort_jeu_temps_4<- tm_jeu_temps_4 %>%
  inner_join(
    score_final_match,
    by = "CD_MATCH") %>%
  group_by(CD_MATCH, id_tm) %>%
  summarise(
    ligne,
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
    
    
    diff_avant_TM = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      first(NB_SCORE_DOMICILE) - first(NB_SCORE_EXTERIEUR),
      first(NB_SCORE_EXTERIEUR) - first(NB_SCORE_DOMICILE)
    ),
    
    diff_300s_apres_TM = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      last(NB_SCORE_DOMICILE) - last(NB_SCORE_EXTERIEUR),
      last(NB_SCORE_EXTERIEUR) - last(NB_SCORE_DOMICILE)
    ),
    
    evolution_score = diff_300s_apres_TM - diff_avant_TM,
    
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
  filter(!is.na(CD_CLUB_TM)) %>%
  group_by(id_tm) %>%
  slice_head(n=1)%>%
  ungroup


valeurs_avant <- resume_temps_mort_jeu_avant_temps_4$ligne.x

diff_lignes <- which(!(resume_temps_mort_jeu_temps_4$ligne %in% valeurs_avant))
diff_lignes

resume_temps_mort_jeu_temps_4 <- resume_temps_mort_jeu_temps_4[-diff_lignes, ]
resume_temps_mort_jeu_temps_4$id_tm <- seq_len(nrow(resume_temps_mort_jeu_temps_4))
resume_temps_mort_jeu_avant_temps_4$id_tm_unique<- seq_len(nrow(resume_temps_mort_jeu_avant_temps_4))

table_clustering_tm<-resume_temps_mort_jeu_avant_temps_4%>%
  inner_join(resume_temps_mort_jeu_temps_4, by = c("id_tm_unique"="id_tm"))%>%
  rename(CD_MATCH=CD_MATCH.x,
         Temps=Temps.x,
         CD_CLUB_TM=CD_CLUB_TM.x,
         score_equipe_TM=score_equipe_TM.y,
         score_autre_equipe_=score_autre_equipe_.y,
         score_final_equipe_tm=score_final_equipe_tm.y,
         score_final_autre_equipe=score_final_autre_equipe.x)%>%
  select(CD_MATCH,
         id_tm_unique,
         Temps,
         CD_CLUB_TM,
         score_equipe_TM,
         score_autre_equipe_,
         indice_serie_avant_tm,
         diff_300s_avant_TM,
         diff_TM,
         diff_300s_apres_TM,
         evolution_score_avant,
         evolution_score,
         score_final_equipe_tm,
         score_final_autre_equipe,
         vainqueur_final)

write.csv(table_clustering_tm, "table_clustering_tm.csv", row.names = FALSE)

'mutate(periode_temps_mort = case_when(
    Temps >= 0  & Temps < 15 ~ "Début de match",
    Temps >= 15 & Temps < 28 ~ "Fin de première mi-temps",
    Temps >= 30 & Temps < 45 ~ "Début de seconde mi-temps",
    Temps >= 45 & Temps < 57 ~ "Fin de match",
    Temps >=28 & Temps <30 | Temps>=57 & Temps <=60 ~ "Money Time",
    TRUE ~ "Hors période"
  ))'

resume_temps_mort_momentum_negatif<-resume_temps_mort_jeu_avant_temps_4 %>%
  filter(indice_serie_avant_tm<=-4,
         CD_CLUB_TM==vainqueur_final)
