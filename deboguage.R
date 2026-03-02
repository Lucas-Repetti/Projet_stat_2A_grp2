resume_temps_mort_jeu_avant_temps %>%
  mutate(test_diff = score_equipe_TM - score_autre_equipe_ - diff_TM) %>%
  summarise(
  n_incoherences = sum(test_diff != 0, na.rm = TRUE),
  max_ecart = max(abs(test_diff), na.rm = TRUE)
  )


resume_temps_mort_jeu_avant_temps <- tm_jeu_avant_temps %>%
  
  # =========================
# Jointure scores finaux
# =========================
inner_join(score_final_match, by = "CD_MATCH") %>%
  
  # =========================
# Récupération du temps exact du TM
# =========================
group_by(CD_MATCH, id_tm_unique) %>%
  mutate(
    ts_tm = TS_START_SEQUENCE_sec[LB_RESULTAT == "TEMPS MORT"][1]
  ) %>%
  ungroup() %>%
  
  # =========================
# Calculs robustes
# =========================
group_by(CD_MATCH, id_tm_unique) %>%
  summarise(
    
    Temps = ts_tm,
    CD_CLUB_TM = first(CD_CLUB[LB_RESULTAT == "TEMPS MORT"]),
    
    # ---- SCORE AU MOMENT DU TM (dernier score <= TM)
    score_dom_TM = NB_SCORE_DOMICILE[
      which.max(ifelse(TS_START_SEQUENCE_sec <= ts_tm,
                       TS_START_SEQUENCE_sec, -Inf))
    ],
    
    score_ext_TM = NB_SCORE_EXTERIEUR[
      which.max(ifelse(TS_START_SEQUENCE_sec <= ts_tm,
                       TS_START_SEQUENCE_sec, -Inf))
    ],
    
    # ---- SCORE 180s AVANT TM
    score_dom_180 = NB_SCORE_DOMICILE[
      which.max(ifelse(TS_START_SEQUENCE_sec <= ts_tm - 180,
                       TS_START_SEQUENCE_sec, -Inf))
    ],
    
    score_ext_180 = NB_SCORE_EXTERIEUR[
      which.max(ifelse(TS_START_SEQUENCE_sec <= ts_tm - 180,
                       TS_START_SEQUENCE_sec, -Inf))
    ],
    
    # =========================
    # DIFFÉRENTIELS (logique unique)
    # =========================
    score_equipe_TM = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      score_dom_TM,
      score_ext_TM
    ),
    
    score_autre_equipe_ = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      score_ext_TM,
      score_dom_TM
    ),
    
    diff_TM = score_equipe_TM - score_autre_equipe_,
    
    diff_180s_avant_TM = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      score_dom_180 - score_ext_180,
      score_ext_180 - score_dom_180
    ),
    
    evolution_score_avant = diff_TM - diff_180s_avant_TM,
    
    # =========================
    # STATS AVANT TM
    # =========================
    buts_marques_avant = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      score_dom_TM - score_dom_180,
      score_ext_TM - score_ext_180
    ),
    
    buts_encaisses_avant = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      score_ext_TM - score_ext_180,
      score_dom_TM - score_dom_180
    ),
    
    pertes_balle_avant = sum(
      LB_RESULTAT == "PERTE DE BALLE" & CD_CLUB == CD_CLUB_TM,
      na.rm = TRUE
    ),
    
    recuperation_balle_avant = sum(
      LB_RESULTAT == "PERTE DE BALLE" & CD_CLUB != CD_CLUB_TM,
      na.rm = TRUE
    ),
    
    # =========================
    # SCORE FINAL
    # =========================
    score_final_equipe_tm = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      first(score_final_dom),
      first(score_final_ext)
    ),
    
    score_final_autre_equipe = if_else(
      CD_CLUB_TM == first(CD_CLUB_DOMICILE),
      first(score_final_ext),
      first(score_final_dom)
    ),
    
    .groups = "drop"
  ) %>%
  
  filter(!is.na(CD_CLUB_TM))





resume_temps_mort_jeu_avant_temps %>%
  mutate(test_diff = score_equipe_TM - score_autre_equipe_ - diff_TM) %>%
  summarise(
    n_incoherences = sum(test_diff != 0, na.rm = TRUE),
    max_ecart = max(abs(test_diff), na.rm = TRUE)
  )

resume_temps_mort_jeu_avant_temps %>%
  mutate(test_diff = score_equipe_TM - score_autre_equipe_ - diff_TM) %>%
  summarise(
    n_incoherences = sum(test_diff != 0, na.rm = TRUE),
    max_ecart = max(abs(test_diff), na.rm = TRUE)
  )
