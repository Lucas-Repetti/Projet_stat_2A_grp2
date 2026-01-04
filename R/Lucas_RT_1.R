library(readr)
library(dplyr)

df1 <- read_csv("data/FCT_MATCH_202109242114.csv")
df2 <- read_csv("data/DIM_MATCH_202109242114.csv")

df1_sel <- df1 %>%
  select(LB_RESULTAT,
         CD_MATCH,
         NB_SCORE_DOMICILE,
         NB_SCORE_EXTERIEUR,
         CD_CLUB)

df2_sel <- df2 %>%
  select(CD_MATCH,
         CD_CLUB_DOMICILE,
         CD_CLUB_EXTERIEUR)

df_joined <- df1_sel %>%
  left_join(df2_sel, by = "CD_MATCH")


df_joined <- df_joined %>%
  group_by(CD_MATCH) %>%
  
  # Identifier la dernière ligne du match
  mutate(is_last_row = row_number() == n()) %>%
  
  # Déterminer score final et équipe gagnante
  mutate(
    NB_SCORE_FINAL_DOMICILE = if_else(
      is_last_row,
      NB_SCORE_DOMICILE,
      NA_real_
    ),
    
    NB_SCORE_FINAL_EXTERIEUR = if_else(
      is_last_row,
      NB_SCORE_EXTERIEUR,
      NA_real_
    ),
    
    CD_CLUB_GAGNANT = case_when(
      is_last_row & NB_SCORE_DOMICILE > NB_SCORE_EXTERIEUR ~ CD_CLUB_DOMICILE,
      is_last_row & NB_SCORE_EXTERIEUR > NB_SCORE_DOMICILE ~ CD_CLUB_EXTERIEUR,
      TRUE ~ NA_character_
    )
  ) %>%
  
  # Propagation à toutes les lignes du match
  fill(
    NB_SCORE_FINAL_DOMICILE,
    NB_SCORE_FINAL_EXTERIEUR,
    CD_CLUB_GAGNANT,
    .direction = "downup"
  ) %>%
  
  # Création de la variable RESULTAT
  mutate(
    RESULTAT = case_when(
      CD_CLUB == CD_CLUB_GAGNANT ~ "GAGNANT",
      TRUE ~ "PERDANT"
    )
  ) %>%
  
  ungroup() %>%
  select(-is_last_row)

###


df_match <- df_joined %>%
  distinct(
    CD_MATCH,
    CD_CLUB_GAGNANT,
    NB_SCORE_FINAL_DOMICILE,
    NB_SCORE_FINAL_EXTERIEUR,
    CD_CLUB_DOMICILE,
    CD_CLUB_EXTERIEUR
  ) %>%
  mutate(
    ECART_SCORE = abs(NB_SCORE_FINAL_DOMICILE - NB_SCORE_FINAL_EXTERIEUR)
  )


###

ratio_matchs_3pts <- df_match %>%
  summarise(
    nb_matchs_total = n(),
    nb_matchs_gagnes_3pts = sum(ECART_SCORE >= 3),
    ratio = nb_matchs_gagnes_3pts / nb_matchs_total
  )

###

library(dplyr)

# 1. Dernier temps mort par match
dernier_tm <- df_joined %>%
  filter(LB_RESULTAT == "FAUTE") %>%
  group_by(CD_MATCH) %>%
  slice_tail(n = 1) %>%   # garde la dernière action du match
  ungroup() %>%
  mutate(
    gagnant_dernier_tm = CD_CLUB == CD_CLUB_GAGNANT
  ) %>%
  select(CD_MATCH, gagnant_dernier_tm)

# 2. Ajout au dataframe match
df_match <- df_match %>%
  left_join(dernier_tm, by = "CD_MATCH")

ratio_true <- mean(df_match$gagnant_dernier_tm, na.rm = TRUE)

unique(df_joined$LB_RESULTAT)
