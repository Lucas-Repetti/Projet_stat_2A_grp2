# ============================================================
# 1️⃣ Importation des librairies
# ============================================================

library(readr)
library(dplyr)
library(lubridate)



# ============================================================
# 2️⃣ Importation des données
# ============================================================

df_match_details <- read_csv("data/FCT_MATCH_DETAILS_202109242114.csv")
df_match_info    <- read_csv("data/DIM_MATCH_202109242114.csv")
df_club          <- read_csv("data/DIM_CLUB_202109242114.csv")



# ============================================================
# 3️⃣ Préparation des bases
# ============================================================

df_match_details <- df_match_details %>%
  filter(str_starts(CD_MATCH, "FR"))

df_match_details <- df_match_details %>%
  mutate(
    x = X_RESULTAT,
    y = Y_RESULTAT
  )

df_match_details <- df_match_details %>%
  mutate(
    X_RESULTAT = as.numeric(X_RESULTAT),
    Y_RESULTAT = as.numeric(Y_RESULTAT)
  )

df_match_details <- df_match_details %>%
  mutate(
    score_difficulte = 0,
    
    # Gardien
    score_difficulte = score_difficulte + ifelse(FG_ATTAQUE_BUT_VIDE == 0, 2, 0),
    
    # Secteur
    score_difficulte = score_difficulte + ifelse(LB_RESULTAT_SECTEUR == "GARDIEN", 4, 0),
    score_difficulte = score_difficulte + ifelse(LB_RESULTAT_SECTEUR == "KUNG-FU", 4, 0),
    
    # Détail du secteur
    score_difficulte = score_difficulte + ifelse(LB_RESULTAT_SECTEUR_DETAIL %in% c("AILE GAUCHE","AILE DROITE"), 2, 0),
    score_difficulte = score_difficulte + ifelse(LB_RESULTAT_SECTEUR_DETAIL %in% c("9-6 GAUCHE","9-6 DROITE","12-9 GAUCHE","12-9 DROITE"), 1, 0),
    
    # Catégorie de tir
    score_difficulte = score_difficulte + ifelse(LB_TIR_CATEGORIE %in% c("REBOND","LOB","ROUCOULETTE"), 2, 0),
    
    # Posture
    score_difficulte = score_difficulte + ifelse(LB_TIR_POSTURE == "EN SUSPENSION", 1, 0),
    
    # Tir difficile si score >= 5
    tir_difficile = ifelse(score_difficulte >= 5 & grepl("BUT", LB_RESULTAT_DETAIL, ignore.case = TRUE), 1, 0)
  )



# ------------------------------------------------------------
# 3.1 Sélection des variables utiles
# ------------------------------------------------------------

df_match_details <- df_match_details %>%
  select(
    CD_MATCH,
    NB_SCORE_DOMICILE,
    NB_SCORE_EXTERIEUR,
    CD_CLUB,
    TS_START_SEQUENCE,
    TS_END_SEQUENCE,
    LB_SEQUENCE_TYPE,
    LB_RESULTAT,
    LB_RESULTAT_DETAIL,
    score_difficulte,
    tir_difficile,
  )

df_match_info <- df_match_info %>%
  select(
    CD_MATCH,
    CD_CLUB_DOMICILE,
    CD_CLUB_EXTERIEUR
  )

df_club <- df_club %>%
  select(
    CD_CLUB,
    LB_CLUB,
    LB_VILLE
  )



# ============================================================
# 4️⃣ Construction de la base principale
# ============================================================

# ------------------------------------------------------------
# 4.1 Fusion des tables
# ------------------------------------------------------------

df <- df_match_details %>%
  left_join(df_match_info, by = "CD_MATCH") %>%
  left_join(df_club, by = "CD_CLUB")


# ------------------------------------------------------------
# 4.2 Création des variables de base
# ------------------------------------------------------------

df <- df %>%
  mutate(
    ECART_POINT  = NB_SCORE_DOMICILE - NB_SCORE_EXTERIEUR,
    POINTS_TOTAL = NB_SCORE_DOMICILE + NB_SCORE_EXTERIEUR,
    DUREE_ACTION = TS_END_SEQUENCE - TS_START_SEQUENCE
  )


# ------------------------------------------------------------
# 4.3 Ajout des villes domicile / extérieur
# ------------------------------------------------------------

df <- df %>%
  left_join(
    df_club %>%
      select(CD_CLUB, LB_VILLE) %>%
      rename(
        CD_CLUB_DOMICILE = CD_CLUB,
        LB_VILLE_DOMICILE = LB_VILLE
      ),
    by = "CD_CLUB_DOMICILE"
  ) %>%
  left_join(
    df_club %>%
      select(CD_CLUB, LB_VILLE) %>%
      rename(
        CD_CLUB_EXTERIEUR = CD_CLUB,
        LB_VILLE_EXTERIEUR = LB_VILLE
      ),
    by = "CD_CLUB_EXTERIEUR"
  )


# ------------------------------------------------------------
# 4.4 Ville de l'équipe adverse
# ------------------------------------------------------------

df <- df %>%
  mutate(
    LB_VILLE_OTHER = if_else(
      LB_VILLE == LB_VILLE_DOMICILE,
      LB_VILLE_EXTERIEUR,
      LB_VILLE_DOMICILE
    )
  )


# ------------------------------------------------------------
# 4.5 Organisation et filtrage
# ------------------------------------------------------------

df <- df %>%
  arrange(CD_MATCH, TS_START_SEQUENCE) %>%
  filter(startsWith(CD_MATCH, "FR")) %>%
  group_by(CD_MATCH) %>%
  filter(n() > 1) %>%
  ungroup()



# ============================================================
# 5️⃣ Correction des temps morts
# ============================================================

df <- df %>%
  mutate(
    LB_SEQUENCE_TYPE = if_else(
      LB_RESULTAT == "TEMPS MORT",
      "TEMPS MORT",
      LB_SEQUENCE_TYPE
    ),
    LB_RESULTAT_DETAIL = if_else(
      LB_RESULTAT == "TEMPS MORT",
      "TEMPS MORT",
      LB_RESULTAT_DETAIL
    )
  )

df <- df %>%
  mutate(
    LB_RESULTAT_DETAIL = if_else(
      tir_difficile == 1,
      "TIR DIFFICILE",
      LB_RESULTAT_DETAIL
    )
  )


# ============================================================
# 6️⃣ Création des actions par équipe
# ============================================================

actions_equipe <- c(
  "BUT","HORS CADRE","TEMPS MORT","POTEAU","PASSAGE EN FORCE",
  "BALLON SORTI","ZONE","MARCHÉ","PASSE INCOMPLETE",
  "FAUTE OFFENSIVE","PIED","REPRISE","JEU PASSIF","MARCHE","TIR DIFFICILE"
)

actions_adversaire <- c(
  "ARRÊT","INTERCEPTION","NEUTRALISATION","TIR CONTRÉ",
  "2 MINUTES","CARTON JAUNE","CARTON ROUGE","CONTRE","CARTON BLEU"
)

df <- df %>%
  mutate(
    
    ACTION_DOMICILE = case_when(
      LB_RESULTAT_DETAIL %in% actions_equipe &
        LB_VILLE == LB_VILLE_DOMICILE ~ LB_RESULTAT_DETAIL,
      
      LB_RESULTAT_DETAIL %in% actions_adversaire &
        LB_VILLE_OTHER == LB_VILLE_DOMICILE ~ LB_RESULTAT_DETAIL,
      
      TRUE ~ NA_character_
    ),
    
    ACTION_EXTERIEUR = case_when(
      LB_RESULTAT_DETAIL %in% actions_equipe &
        LB_VILLE != LB_VILLE_DOMICILE ~ LB_RESULTAT_DETAIL,
      
      LB_RESULTAT_DETAIL %in% actions_adversaire &
        LB_VILLE_OTHER != LB_VILLE_DOMICILE ~ LB_RESULTAT_DETAIL,
      
      TRUE ~ NA_character_
    )
    
  )



# ============================================================
# 7️⃣ Type d'action (positif / négatif)
# ============================================================

actions_positives <- c(
  "BUT","TEMPS MORT","ARRÊT","INTERCEPTION","NEUTRALISATION",
  "TIR CONTRÉ","CARTON JAUNE","CONTRE","TIR DIFFICILE"
)

actions_negatives <- c(
  "HORS CADRE","POTEAU","PASSAGE EN FORCE","BALLON SORTI",
  "ZONE","MARCHÉ","PASSE INCOMPLETE","FAUTE OFFENSIVE",
  "PIED","REPRISE","JEU PASSIF","MARCHE",
  "2 MINUTES","CARTON ROUGE","CARTON BLEU"
)

df <- df %>%
  mutate(
    
    TYPE_ACTION_DOMICILE = case_when(
      ACTION_DOMICILE %in% actions_positives ~ "POSITIF",
      ACTION_DOMICILE %in% actions_negatives ~ "NEGATIF",
      TRUE ~ NA_character_
    ),
    
    TYPE_ACTION_EXTERIEUR = case_when(
      ACTION_EXTERIEUR %in% actions_positives ~ "POSITIF",
      ACTION_EXTERIEUR %in% actions_negatives ~ "NEGATIF",
      TRUE ~ NA_character_
    )
    
  )

# ============================================================
# Compléter les types d'action manquants par opposition
# ============================================================

df <- df %>%
  mutate(
    
    TYPE_ACTION_DOMICILE = case_when(
      is.na(TYPE_ACTION_DOMICILE) & TYPE_ACTION_EXTERIEUR == "POSITIF" ~ "NEGATIF",
      is.na(TYPE_ACTION_DOMICILE) & TYPE_ACTION_EXTERIEUR == "NEGATIF" ~ "POSITIF",
      TRUE ~ TYPE_ACTION_DOMICILE
    ),
    
    TYPE_ACTION_EXTERIEUR = case_when(
      is.na(TYPE_ACTION_EXTERIEUR) & TYPE_ACTION_DOMICILE == "POSITIF" ~ "NEGATIF",
      is.na(TYPE_ACTION_EXTERIEUR) & TYPE_ACTION_DOMICILE == "NEGATIF" ~ "POSITIF",
      TRUE ~ TYPE_ACTION_EXTERIEUR
    )
    
  )


# ============================================================
# 8️⃣ Calcul de l'écart entre deux points
# ============================================================
library(dplyr)

df <- df %>%
  group_by(CD_MATCH) %>%
  mutate(
    ECART_2_POINTS = sapply(1:n(), function(i) {
      if(POINTS_TOTAL[i] >= 2) {
        # chercher les lignes j dans le même match avec POINTS_TOTAL == POINTS_TOTAL[i] - 2
        j <- which(POINTS_TOTAL == POINTS_TOTAL[i] - 2 & row_number() < i)
        if(length(j) > 0) {
          j <- max(j)  # prendre la ligne la plus proche avant i
          return(abs(ECART_POINT[i] - ECART_POINT[j]))
        }
      }
      return(NA)
    })
  ) %>%
  ungroup()

# ============================================================
# 9️⃣ Identification du club dominant
# ============================================================
library(dplyr)

df <- df %>%
  arrange(CD_MATCH, row_number()) %>%
  group_by(CD_MATCH) %>%
  mutate(
    NEW_DOMINATION_1 = case_when(
      row_number() == 1 & !is.na(ECART_2_POINTS) & ECART_2_POINTS != 0 ~ TRUE,  # première ligne du match
      !is.na(ECART_2_POINTS) & ECART_2_POINTS != 0 & (ECART_2_POINTS != lag(ECART_2_POINTS) | is.na(lag(ECART_2_POINTS))) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  ungroup()

library(dplyr)

df <- df %>%
  arrange(CD_MATCH, row_number()) %>%
  group_by(CD_MATCH) %>%
  mutate(
    END_DOMINATION = case_when(
      !is.na(ECART_2_POINTS) & ECART_2_POINTS != 0 & 
        (lead(ECART_2_POINTS) != ECART_2_POINTS | is.na(lead(ECART_2_POINTS)) | lead(ECART_2_POINTS) == 0) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  ungroup()

library(dplyr)

df <- df %>%
  arrange(CD_MATCH, row_number()) %>%
  group_by(CD_MATCH) %>%
  mutate(
    CLUB_DOMINANT = if_else(
      NEW_DOMINATION_1,
      lag(CD_CLUB),      # prend le club de la ligne précédente
      NA_character_       # sinon NA
    )
  ) %>%
  ungroup()


# ============================================================
# 🔟 Propagation du club dominant
# ============================================================


library(dplyr)

df <- df %>%
  arrange(CD_MATCH, row_number()) %>%
  group_by(CD_MATCH) %>%
  mutate(
    NEW_DOMINATION_2 = FALSE  # initialisation
  ) %>%
  group_modify(~{
    dat <- .
    idx_true <- which(dat$NEW_DOMINATION_1)
    for(i in idx_true) {
      pt_i <- dat$POINTS_TOTAL[i]
      # chercher toutes les lignes avant i où POINTS_TOTAL == POINTS_TOTAL[i] - 1
      candidates <- which(dat$POINTS_TOTAL == pt_i - 1 & (1:nrow(dat)) < i)
      if(length(candidates) > 0) {
        j <- min(candidates)  # prendre la ligne la plus ancienne
        dat$NEW_DOMINATION_2[j] <- TRUE
      }
    }
    dat
  }) %>%
  ungroup()

library(dplyr)

propagate_club <- function(dat) {
  n <- nrow(dat)
  cd <- dat$CLUB_DOMINANT
  
  # Propagation vers le haut
  last_value <- NA_character_
  for(i in n:1) {
    if(!is.na(cd[i])) last_value <- cd[i]
    if(!is.na(last_value)) {
      cd[i] <- last_value
    }
    if(!is.na(dat$NEW_DOMINATION_2[i]) && dat$NEW_DOMINATION_2[i] == TRUE) {
      last_value <- NA_character_
    }
  }
  
  # Propagation vers le bas
  last_value <- NA_character_
  for(i in 1:n) {
    if(!is.na(cd[i])) last_value <- cd[i]
    if(!is.na(last_value)) {
      cd[i] <- last_value
    }
    if(!is.na(dat$END_DOMINATION[i]) && dat$END_DOMINATION[i] == TRUE) {
      last_value <- NA_character_
    }
  }
  
  dat$CLUB_DOMINANT <- cd
  dat
}

df <- df %>%
  group_by(CD_MATCH) %>%
  group_modify(~propagate_club(.)) %>%
  ungroup()


# ============================================================
# 11️⃣ Action qui crée l'écart de domination
# ============================================================
library(dplyr)

# Ajouter un index pour garder l'ordre original
df <- df %>%
  mutate(ROW_ID = row_number())

# Créer la colonne ACTION_CREA
df <- df %>%
  group_by(CD_MATCH) %>%
  arrange(ROW_ID, .by_group = TRUE) %>%
  mutate(ACTION_CREA = NA_character_) %>%
  ungroup()

# Fonction pour trouver l'action valide en remontant depuis la ligne précédente
get_action_crea <- function(df_match) {
  n <- nrow(df_match)
  
  for(i in 1:n) {
    # On ne traite que si NEW_DOMINATION_2 est TRUE et CLUB_DOMINANT n'est pas NA
    if(!is.na(df_match$NEW_DOMINATION_2[i]) && df_match$NEW_DOMINATION_2[i] == TRUE &&
       !is.na(df_match$CLUB_DOMINANT[i])) {
      
      # Déterminer si l'équipe dominante est domicile ou extérieur
      if(!is.na(df_match$CD_CLUB_DOMICILE[i]) && df_match$CLUB_DOMINANT[i] == df_match$CD_CLUB_DOMICILE[i]) {
        type_col <- "TYPE_ACTION_DOMICILE"
      } else if(!is.na(df_match$CD_CLUB_EXTERIEUR[i]) && df_match$CLUB_DOMINANT[i] == df_match$CD_CLUB_EXTERIEUR[i]) {
        type_col <- "TYPE_ACTION_EXTERIEUR"
      } else {
        type_col <- NA
      }
      
      # On commence à regarder **la ligne avant**
      j <- i - 1
      repeat {
        if(j < 1) break  # Début du match
        type_action <- df_match[[type_col]][j]
        lb <- df_match$LB_RESULTAT_DETAIL[j]
        
        if(!is.na(type_action) && type_action == "POSITIF" &&
           !is.na(lb) && !(lb %in% c("NEUTRALISATION", "BUT"))) {
          df_match$ACTION_CREA[i] <- lb
          break
        }
        
        j <- j - 1
      }
    }
  }
  return(df_match)
}

# Appliquer match par match
df <- df %>%
  group_by(CD_MATCH) %>%
  group_modify(~get_action_crea(.x)) %>%
  ungroup() %>%
  select(-ROW_ID)

########################################
df <- df %>%
  group_by(CD_MATCH) %>%
  mutate(
    
    num_domination = cumsum(
      !is.na(CLUB_DOMINANT) &
        (CLUB_DOMINANT != lag(CLUB_DOMINANT) | is.na(lag(CLUB_DOMINANT)))
    ),
    
    ID_DOMINATION = if_else(
      is.na(CLUB_DOMINANT),
      NA_character_,
      paste0(CD_MATCH, "_", num_domination)
    )
    
  ) %>%
  select(-num_domination) %>%
  ungroup()


##############################""

# ============================================================
# Table de synthèse des périodes de domination
# ============================================================

library(dplyr)

# Ajouter un index temporaire pour garder l'ordre original
df_scores <- df %>%
  mutate(ROW_ID_TMP = row_number()) %>%
  group_by(CD_MATCH) %>%
  arrange(ROW_ID_TMP, .by_group = TRUE) %>%  # on ordonne par l'ordre original
  mutate(
    SCORE_DOM_AV_LAG = lag(NB_SCORE_DOMICILE),
    SCORE_EXT_AV_LAG = lag(NB_SCORE_EXTERIEUR)
  ) %>%
  ungroup()

# Construire table_domination en utilisant les scores "avant" la domination
table_domination <- df_scores %>%
  filter(!is.na(ID_DOMINATION)) %>%
  group_by(ID_DOMINATION) %>%
  summarise(
    CD_MATCH = first(CD_MATCH),
    
    CD_CLUB_DOMICILE  = first(CD_CLUB_DOMICILE),
    CD_CLUB_EXTERIEUR = first(CD_CLUB_EXTERIEUR),
    
    CLUB_DOMINANT = first(CLUB_DOMINANT),
    
    # début / fin domination
    DEBUT_DOMINATION = first(TS_START_SEQUENCE),
    FIN_DOMINATION   = last(TS_END_SEQUENCE),
    
    # score au début de la domination → ligne avant
    SCORE_DOM_AV = first(SCORE_DOM_AV_LAG),
    SCORE_EXT_AV = first(SCORE_EXT_AV_LAG),
    
    # score à la fin de la domination
    SCORE_DOM_AP = last(NB_SCORE_DOMICILE),
    SCORE_EXT_AP = last(NB_SCORE_EXTERIEUR),
    
    # action créatrice
    ACTION_CREATRICE = first(ACTION_CREA),
    
    # tirs
    NB_TIR_DOM = sum(ACTION_DOMICILE %in% c("TIR","TIR DIFFICILE"), na.rm = TRUE),
    NB_TIR_EXT = sum(ACTION_EXTERIEUR %in% c("TIR","TIR DIFFICILE"), na.rm = TRUE),
    
    # arrêts
    NB_ARRET_DOM = sum(ACTION_DOMICILE == "ARRÊT", na.rm = TRUE),
    NB_ARRET_EXT = sum(ACTION_EXTERIEUR == "ARRÊT", na.rm = TRUE),
    
    # tirs difficiles
    NB_TIR_DIFF = sum(LB_RESULTAT_DETAIL == "TIR DIFFICILE", na.rm = TRUE),
    
    .groups = "drop"
  )

# Calcul des écarts et durée
table_domination <- table_domination %>%
  mutate(
    ECART_AV = if_else(
      CLUB_DOMINANT == CD_CLUB_DOMICILE,
      SCORE_DOM_AV - SCORE_EXT_AV,
      SCORE_EXT_AV - SCORE_DOM_AV
    ),
    ECART_AP = if_else(
      CLUB_DOMINANT == CD_CLUB_DOMICILE,
      SCORE_DOM_AP - SCORE_EXT_AP,
      SCORE_EXT_AP - SCORE_DOM_AP
    ),
    ECART_CREE = ECART_AP - ECART_AV,
    DUREE = FIN_DOMINATION - DEBUT_DOMINATION
  )

write.csv(table_domination, "table_domination.csv", row.names = FALSE)
