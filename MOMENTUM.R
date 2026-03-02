# ============================================================
# 1️⃣ LIBRAIRIES
# ============================================================

library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(data.table)


# ============================================================
# 2️⃣ IMPORTATION DES DONNÉES
# ============================================================

df1 <- read_csv("data/FCT_MATCH_DETAILS_202109242114.csv")
df2 <- read_csv("data/DIM_MATCH_202109242114.csv")
df3 <- read_csv("data/DIM_CLUB_202109242114.csv")

df1 <- df1 %>%
  select(
    CD_MATCH,
    NB_SCORE_DOMICILE,
    NB_SCORE_EXTERIEUR,
    CD_CLUB,
    TS_START_SEQUENCE,
    TS_END_SEQUENCE,
    LB_SEQUENCE_TYPE,
    LB_RESULTAT,
    LB_RESULTAT_DETAIL
  )

df2 <- df2 %>%
  select(
    CD_MATCH,
    CD_CLUB_DOMICILE,
    CD_CLUB_EXTERIEUR
  )

df3 <- df3 %>% 
  select(
    CD_CLUB,
    LB_CLUB,
    LB_VILLE
  )

# ============================================================
# 3️⃣ CHOIX DES ACTION
# ============================================================


df <- df1 %>%
  left_join(df2, by = "CD_MATCH") %>%
  left_join(df3, by = "CD_CLUB") %>%
  mutate(
    ECART_POINT  = NB_SCORE_DOMICILE - NB_SCORE_EXTERIEUR,
    POINTS_TOTAL = NB_SCORE_DOMICILE + NB_SCORE_EXTERIEUR,
    DUREE_ACTION = TS_END_SEQUENCE - TS_START_SEQUENCE
  ) %>%
  # Création des colonnes LB_VILLE_DOMICILE et LB_VILLE_EXTERIEUR
  left_join(
    df3 %>% select(CD_CLUB, LB_VILLE) %>% rename(CD_CLUB_DOMICILE = CD_CLUB, LB_VILLE_DOMICILE = LB_VILLE),
    by = "CD_CLUB_DOMICILE"
  ) %>%
  left_join(
    df3 %>% select(CD_CLUB, LB_VILLE) %>% rename(CD_CLUB_EXTERIEUR = CD_CLUB, LB_VILLE_EXTERIEUR = LB_VILLE),
    by = "CD_CLUB_EXTERIEUR"
  ) %>%
  # Création de LB_VILLE_OTHER
  mutate(
    LB_VILLE_OTHER = if_else(LB_VILLE == LB_VILLE_DOMICILE, LB_VILLE_EXTERIEUR, LB_VILLE_DOMICILE)
  ) %>%
  arrange(CD_MATCH, TS_START_SEQUENCE)



modalites_sequence <- c(
  "ATTAQUE PLACEE", "CONTRE ATTAQUE", "ENGAGEMENT RAPIDE",
  "JET DE 7M", "MONTEE DE BALLE", "ERREUR DE RELANCE",
  "ATTAQUE PLACÉE", "TRANSITION",
  "JET FRANC", "MONTÉE DE BALLE"
)

modalites_resultat_override <- c("TEMPS MORT", "FAUTE HORS JEU")

modalites_detail <- c(
  "ARRÊT", "HORS CADRE", "PASSE INCOMPLETE", "INTERCEPTION",
  "2 MINUTES", "TIR CONTRÉ", "POTEAU", "JEU PASSIF",
  "FAUTE OFFENSIVE", "REPRISE", "PASSAGE EN FORCE",
  "BALLON SORTI", "ZONE", "MARCHÉ", "CARTON ROUGE",
  "CONTRE", "PIED", "MARCHE", "CARTON BLEU"
)

df <- df %>%
  mutate(
    LB_1 = case_when(
      LB_RESULTAT %in% modalites_resultat_override ~ LB_RESULTAT,
      LB_SEQUENCE_TYPE %in% modalites_sequence ~ LB_SEQUENCE_TYPE,
      TRUE ~ NA_character_
    ),
    
    LB_2 = case_when(
      LB_RESULTAT == "TEMPS MORT" ~ "TEMPS MORT",   # <-- ajout ici
      LB_RESULTAT_DETAIL %in% modalites_detail ~ LB_RESULTAT_DETAIL,
      TRUE ~ NA_character_
    )
  )


df <- df %>%
  mutate(
    TYPE_ACTION = case_when(
      LB_2 %in% c( "2 MINUTES", "CARTON ROUGE", "CARTON BLEU") ~ "POSITIF",
      LB_2 %in% c("ARRÊT", "HORS CADRE", "PASSE INCOMPLETE", 
                  "INTERCEPTION", "TIR CONTRÉ", "POTEAU", "JEU PASSIF", 
                  "FAUTE OFFENSIVE", "REPRISE", "PASSAGE EN FORCE",
                  "BALLON SORTI", "ZONE", "MARCHÉ",
                  "CONTRE", "PIED", "MARCHE") ~ "NEGATIF",
      TRUE ~ NA_character_
    )
  )

# Inversion pour raisonner du point de vue domicile
df <- df %>%
  mutate(
    TYPE_ACTION = case_when(
      CD_CLUB == CD_CLUB_DOMICILE ~ TYPE_ACTION,
      CD_CLUB == CD_CLUB_EXTERIEUR & TYPE_ACTION == "POSITIF" ~ "NEGATIF",
      CD_CLUB == CD_CLUB_EXTERIEUR & TYPE_ACTION == "NEGATIF" ~ "POSITIF",
      TRUE ~ TYPE_ACTION
    )
  )


# ============================================================
# 4️⃣ FONCTION CALCUL DU MOMENTUM
# ============================================================

calcul_momentum <- function(df, points_mis, profondeur_points) {
  
  seuil <- points_mis / profondeur_points
  
  df %>%
    arrange(CD_MATCH, POINTS_TOTAL) %>%
    group_by(CD_MATCH) %>%
    
    # ---- ETAT DE FORME ----
  mutate( ETAT_DE_FORME = if_else( 
    POINTS_TOTAL <= profondeur_points, 
    (ECART_POINT + POINTS_TOTAL) / (POINTS_TOTAL * 2), 
    (ECART_POINT - ECART_POINT[match(POINTS_TOTAL - profondeur_points, POINTS_TOTAL)] + profondeur_points) / (profondeur_points * 2) ),
    
    # ---- Momentum brut ----
    MOMENTUM = case_when(
      ETAT_DE_FORME >= seuil ~ as.character(CD_CLUB_DOMICILE),
      ETAT_DE_FORME <= (1 - seuil) ~ as.character(CD_CLUB_EXTERIEUR),
      TRUE ~ "NEUTRE"
    )
  ) %>%
    
    # ---- Identifier les débuts de blocs ----
  mutate(
    debut_bloc = MOMENTUM != "NEUTRE" &
      lag(MOMENTUM, default = "NEUTRE") == "NEUTRE"
  ) %>%
    
    # ---- Rétro-propagation propre ----
  mutate(
    MOMENTUM = {
      mom <- MOMENTUM
      pts <- POINTS_TOTAL
      
      for(i in which(debut_bloc)) {
        
        debut_point <- pts[i]
        valeur_mom  <- mom[i]
        
        borne_inf <- debut_point - profondeur_points +1
        
        indices_a_modifier <- which(
          pts >= borne_inf &
            pts < debut_point &
            mom == "NEUTRE"
        )
        
        mom[indices_a_modifier] <- valeur_mom
      }
      
      mom
    }
  ) %>%
    
    # ---- Règle obligatoire début de match ----
  mutate(
    MOMENTUM = if_else(
      POINTS_TOTAL < profondeur_points,
      "NEUTRE",
      MOMENTUM
    )
  ) %>%
    
    ungroup() %>%
    select(-debut_bloc)
}



df_4_4 <- calcul_momentum(df, 4, 4)

df_3_3 <- calcul_momentum(df, 3, 3)

df_3_4 <- calcul_momentum(df, 3, 4)

df_4_5 <- calcul_momentum(df, 4, 5)

df_2_2 <- calcul_momentum(df, 2, 2)

# ============================================================
# 5️⃣ ACTION CRÉATRICE
# ============================================================
find_action <- function(df_group, profondeur) {
  
  n <- nrow(df_group)
  action_crea_1 <- rep(NA_character_, n)
  action_crea_2 <- rep(NA_character_, n)
  
  # Détecter les nouveaux momentums
  nouveau_momentum <- !is.na(df_group$MOMENTUM) &
    df_group$MOMENTUM != "NEUTRE" &
    (is.na(dplyr::lag(df_group$MOMENTUM)) |
       df_group$MOMENTUM != dplyr::lag(df_group$MOMENTUM))
  
  for (i in seq_len(n)) {
    if (!nouveau_momentum[i]) next
    
    types_valides <- if (df_group$MOMENTUM[i] == df_group$CD_CLUB_DOMICILE[i]) {
      c("POSITIF", "NEUTRE")
    } else {
      c("NEGATIF", "NEUTRE")
    }
    
    # Cherche l'action créatrice en arrière
    for (j in seq(i - 1, max(1, i - profondeur), by = -1)) {
      if (!is.na(df_group$TYPE_ACTION[j]) &&
          df_group$TYPE_ACTION[j] %in% types_valides) {
        
        # On associe l'action créatrice à la ligne j, pas i
        action_crea_1[j] <- df_group$LB_1[j]
        action_crea_2[j] <- df_group$LB_2[j]
        break  # on arrête dès qu'on trouve la première
      }
    }
  }
  
  data.frame(ACTION_CREA_1 = action_crea_1,
             ACTION_CREA_2 = action_crea_2)
}



df_4_4 <- df_4_4 %>%
  group_by(CD_MATCH) %>%
  mutate(
    find_action(cur_data(), 5)
  ) %>%
  ungroup()

df_3_4 <- df_3_4 %>%
  group_by(CD_MATCH) %>%
  mutate(
    find_action(cur_data(), 5)
  ) %>%
  ungroup()

df_4_5 <- df_4_5 %>%
  group_by(CD_MATCH) %>%
  mutate(
    find_action(cur_data(), 5)
  ) %>%
  ungroup()

df_2_2 <- df_2_2 %>%
  group_by(CD_MATCH) %>%
  mutate(
    find_action(cur_data(), 5)
  ) %>%
  ungroup()

df_3_3 <- df_3_3 %>%
  group_by(CD_MATCH) %>%
  mutate(
    find_action(cur_data(), 5)
  ) %>%
  ungroup()
#########################################

compter_momentum_et_resultat <- function(df) {
  
  # 1️⃣ Compter les blocs de momentum
  momentum_counts <- df %>%
    arrange(CD_MATCH, POINTS_TOTAL) %>%
    group_by(CD_MATCH) %>%
    mutate(id_sequence = data.table::rleid(MOMENTUM)) %>%
    ungroup() %>%
    filter(MOMENTUM != "NEUTRE") %>%
    distinct(CD_MATCH, id_sequence, MOMENTUM) %>%
    count(CD_MATCH, MOMENTUM, name = "nb_momentums")
  
  # 2️⃣ Créer la base avec exactement les 2 équipes par match
  equipes_match <- df %>%
    distinct(CD_MATCH, CD_CLUB_DOMICILE, CD_CLUB_EXTERIEUR) %>%
    pivot_longer(
      cols = c(CD_CLUB_DOMICILE, CD_CLUB_EXTERIEUR),
      names_to = NULL,
      values_to = "MOMENTUM"
    )
  
  # 3️⃣ Joindre les comptes de momentum et compléter les NA par 0
  df_momentum_final <- equipes_match %>%
    left_join(momentum_counts, by = c("CD_MATCH", "MOMENTUM")) %>%
    mutate(nb_momentums = replace_na(nb_momentums, 0))
  
  # 4️⃣ Ajouter la colonne GAGNE (TRUE si l'équipe a gagné)
  df_momentum_final <- df_momentum_final %>%
    left_join(
      df %>%
        group_by(CD_MATCH) %>%
        slice_max(order_by = POINTS_TOTAL, n = 1, with_ties = FALSE) %>%
        mutate(
          WINNER = case_when(
            NB_SCORE_DOMICILE > NB_SCORE_EXTERIEUR ~ CD_CLUB_DOMICILE,
            NB_SCORE_EXTERIEUR > NB_SCORE_DOMICILE ~ CD_CLUB_EXTERIEUR,
            TRUE ~ NA_character_  # match nul
          )
        ) %>%
        select(CD_MATCH, WINNER),
      by = "CD_MATCH"
    ) %>%
    mutate(GAGNE = MOMENTUM == WINNER)
  
  # Retourner le dataframe final
  return(df_momentum_final)
}


scores_finaux <- df_4_4 %>%
  group_by(CD_MATCH) %>%
  slice_max(order_by = POINTS_TOTAL, n = 1, with_ties = FALSE) %>%  # dernière ligne du match
  select(CD_MATCH, NB_SCORE_DOMICILE, NB_SCORE_EXTERIEUR, CD_CLUB_DOMICILE, CD_CLUB_EXTERIEUR) %>%
  ungroup()

df_voir <- compter_momentum_et_resultat(df_4_4)

df_voir <- df_voir %>%
  left_join(scores_finaux, by = "CD_MATCH") %>%
  mutate(
    ECART_POINTS = if_else(
      MOMENTUM == CD_CLUB_DOMICILE,
      NB_SCORE_DOMICILE - NB_SCORE_EXTERIEUR,
      NB_SCORE_EXTERIEUR - NB_SCORE_DOMICILE
    )
  ) %>%
  select(-CD_CLUB_DOMICILE, -CD_CLUB_EXTERIEUR, -NB_SCORE_DOMICILE, -NB_SCORE_EXTERIEUR)  # facultatif, pour ne pas garder les colonnes en plus





compter_momentum_et_resultat <- function(df) {
  
  library(dplyr)
  library(tidyr)
  library(data.table)
  
  # 1️⃣ Compter les blocs de momentum
  momentum_counts <- df %>%
    arrange(CD_MATCH, POINTS_TOTAL) %>%
    group_by(CD_MATCH) %>%
    mutate(id_sequence = data.table::rleid(MOMENTUM)) %>%
    ungroup() %>%
    filter(MOMENTUM != "NEUTRE") %>%
    distinct(CD_MATCH, id_sequence, MOMENTUM) %>%
    count(CD_MATCH, MOMENTUM, name = "nb_momentums")
  
  # 2️⃣ Base avec les 2 équipes par match
  equipes_match <- df %>%
    distinct(CD_MATCH, CD_CLUB_DOMICILE, CD_CLUB_EXTERIEUR) %>%
    pivot_longer(
      cols = c(CD_CLUB_DOMICILE, CD_CLUB_EXTERIEUR),
      names_to = NULL,
      values_to = "MOMENTUM"
    )
  
  # 3️⃣ Jointure des comptes + remplacement NA par 0
  df_momentum <- equipes_match %>%
    left_join(momentum_counts, by = c("CD_MATCH", "MOMENTUM")) %>%
    mutate(nb_momentums = replace_na(nb_momentums, 0))
  
  # 4️⃣ Calcul du gagnant
  scores_finaux <- df %>%
    group_by(CD_MATCH) %>%
    slice_max(order_by = POINTS_TOTAL, n = 1, with_ties = FALSE) %>%
    mutate(
      WINNER = case_when(
        NB_SCORE_DOMICILE > NB_SCORE_EXTERIEUR ~ CD_CLUB_DOMICILE,
        NB_SCORE_EXTERIEUR > NB_SCORE_DOMICILE ~ CD_CLUB_EXTERIEUR,
        TRUE ~ NA_character_
      )
    ) %>%
    select(
      CD_MATCH,
      NB_SCORE_DOMICILE,
      NB_SCORE_EXTERIEUR,
      CD_CLUB_DOMICILE,
      CD_CLUB_EXTERIEUR,
      WINNER
    ) %>%
    ungroup()
  
  # 5️⃣ Jointure finale + calcul écart
  df_final <- df_momentum %>%
    left_join(scores_finaux, by = "CD_MATCH") %>%
    mutate(
      GAGNE = MOMENTUM == WINNER,
      ECART_POINTS = if_else(
        MOMENTUM == CD_CLUB_DOMICILE,
        NB_SCORE_DOMICILE - NB_SCORE_EXTERIEUR,
        NB_SCORE_EXTERIEUR - NB_SCORE_DOMICILE
      )
    ) %>%
    select(
      CD_MATCH,
      MOMENTUM,
      nb_momentums,
      GAGNE,
      ECART_POINTS
    )
  
  return(df_final)
}

df_voir <- compter_momentum_et_resultat(df_3_3)

# Exporter df_voir en CSV dans le dossier "data"
write.csv(df_voir, file = "data/momentum_desc_3_0.csv", row.names = FALSE)
