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
# 8️⃣ Calcul de l'écart entre deux paniers
# ============================================================

df <- df %>%
  group_by(CD_MATCH) %>%
  mutate(
    
    change_points = POINTS_TOTAL != lag(POINTS_TOTAL, default = first(POINTS_TOTAL)),
    
    ECART_2_POINTS = sapply(row_number(), function(i) {
      
      if (POINTS_TOTAL[i] < 2 || !change_points[i]) return(NA_real_)
      
      target <- POINTS_TOTAL[i] - 2
      prev_rows <- which(POINTS_TOTAL[1:(i-1)] == target)
      
      if (length(prev_rows) == 0) return(NA_real_)
      
      j <- max(prev_rows)
      
      abs(ECART_POINT[i] - ECART_POINT[j])
      
    }),
    
    ECART_2_POINTS = lead(ECART_2_POINTS)
    
  ) %>%
  ungroup() %>%
  select(-change_points)



# ============================================================
# 9️⃣ Identification du club dominant
# ============================================================

df <- df %>%
  mutate(
    CLUB_DOMINANT = if_else(
      ECART_2_POINTS == 2,
      LB_VILLE,
      NA_character_
    )
  )



# ============================================================
# 🔟 Propagation du club dominant
# ============================================================

df <- df %>%
  group_by(CD_MATCH) %>%
  mutate(
    
    CLUB_DOM_TMP = if_else(
      ECART_2_POINTS == 2,
      CLUB_DOMINANT,
      NA_character_
    ),
    
    CLUB_DOMINANT = {
      
      temp <- CLUB_DOM_TMP
      
      for(i in 2:n()) {
        
        if(is.na(temp[i]) &&
           !is.na(temp[i-1]) &&
           is.na(ECART_2_POINTS[i])) {
          
          temp[i] <- temp[i-1]
          
        }
      }
      
      temp
      
    }
    
  ) %>%
  select(-CLUB_DOM_TMP) %>%
  ungroup()



# ============================================================
# 11️⃣ Action qui crée l'écart de domination
# ============================================================

df <- df %>%
  group_by(CD_MATCH) %>%
  mutate(
    
    ACTION_CREA = sapply(row_number(), function(i) {
      
      if (is.na(ECART_2_POINTS[i]) || ECART_2_POINTS[i] != 2)
        return(NA_character_)
      
      prev_rows <- (i-1):1
      
      for(j in prev_rows){
        
        # ignorer lignes NA ou neutralisation
        if(is.na(LB_RESULTAT_DETAIL[j]) ||
           LB_RESULTAT_DETAIL[j] == "NEUTRALISATION")
          next
        
        # cas où l'équipe dominante est domicile
        if(CLUB_DOMINANT[i] == LB_VILLE_DOMICILE[i]){
          
          if(TYPE_ACTION_DOMICILE[j] == "POSITIF"){
            return(LB_RESULTAT_DETAIL[j])
          }
          
        } else {
          
          # cas où l'équipe dominante est extérieur
          if(TYPE_ACTION_EXTERIEUR[j] == "POSITIF"){
            return(LB_RESULTAT_DETAIL[j])
          }
          
        }
        
      }
      
      return(NA_character_)
      
    })
    
  ) %>%
  ungroup()


# ============================================================
# 12️⃣ Détection du vrai changement de domination
# ============================================================

df <- df %>%
  group_by(CD_MATCH) %>%
  mutate(
    
    ACTION_CREA_VRAI = if_else(
      
      (is.na(CLUB_DOMINANT) & !is.na(lag(CLUB_DOMINANT))) |
        (!is.na(CLUB_DOMINANT) &
           (is.na(lag(CLUB_DOMINANT)) |
              CLUB_DOMINANT != lag(CLUB_DOMINANT))),
      
      ACTION_CREA,
      NA_character_
      
    )
    
  ) %>%
  ungroup()

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

# ============================================================
# Table de synthèse des périodes de domination
# ============================================================

table_domination <- df %>%
  
  filter(!is.na(ID_DOMINATION)) %>%
  
  group_by(ID_DOMINATION) %>%
  
  summarise(
    
    CD_MATCH = first(CD_MATCH),
    
    LB_VILLE_DOMICILE  = first(LB_VILLE_DOMICILE),
    LB_VILLE_EXTERIEUR = first(LB_VILLE_EXTERIEUR),
    
    CLUB_DOMINANT = first(CLUB_DOMINANT),
    
    # début / fin domination
    DEBUT_DOMINATION = first(TS_START_SEQUENCE),
    FIN_DOMINATION   = last(TS_END_SEQUENCE),
    
    # score au début de la domination
    SCORE_DOM_AV = first(NB_SCORE_DOMICILE),
    SCORE_EXT_AV = first(NB_SCORE_EXTERIEUR),
    
    # score à la fin de la domination
    SCORE_DOM_AP = last(NB_SCORE_DOMICILE),
    SCORE_EXT_AP = last(NB_SCORE_EXTERIEUR),
    
    # action créatrice
    ACTION_CREATRICE = first(ACTION_CREA_VRAI),
    
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

table_domination <- table_domination %>%
  mutate(
    
    ECART_AV = if_else(
      CLUB_DOMINANT == LB_VILLE_DOMICILE,
      SCORE_DOM_AV - SCORE_EXT_AV,
      SCORE_EXT_AV - SCORE_DOM_AV
    ),
    
    ECART_AP = if_else(
      CLUB_DOMINANT == LB_VILLE_DOMICILE,
      SCORE_DOM_AP - SCORE_EXT_AP,
      SCORE_EXT_AP - SCORE_DOM_AP
    ),
    
    ECART_CREE = ECART_AP - ECART_AV
    
  )
