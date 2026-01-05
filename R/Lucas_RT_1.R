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
  mutate(ECART_POINT = NB_SCORE_DOMICILE - NB_SCORE_EXTERIEUR)

df_joined <- df_joined %>%
  mutate(POINTS_TOTAL = NB_SCORE_DOMICILE + NB_SCORE_EXTERIEUR)


library(dplyr)

df_joined <- df_joined %>%
  group_by(CD_MATCH) %>%
  arrange(POINTS_TOTAL, .by_group = TRUE) %>%
  mutate(
    NEW_POINT = POINTS_TOTAL != lag(POINTS_TOTAL, default = first(POINTS_TOTAL)),
    POINT_NUMBER = cumsum(NEW_POINT),
    ECART_4_POINTS = if_else(
      POINT_NUMBER <= 4,
      ECART_POINT,
      ECART_POINT - ECART_POINT[match(POINT_NUMBER - 4, POINT_NUMBER)]
    )
  ) %>%
  select(-NEW_POINT, -POINT_NUMBER) %>%
  ungroup()

library(dplyr)

df_joined <- df_joined %>%
  mutate(
    SENS_ECART_4_POINTS = case_when(
      ECART_4_POINTS < 0 ~ "EXTERIEUR",
      ECART_4_POINTS == 0 ~ "NEUTRE",
      ECART_4_POINTS > 0 ~ "DOMICILE"
    )
  )
library(dplyr)

df_joined <- df_joined %>%
  mutate(
    SENS_ECART_4_POINTS = case_when(
      ECART_4_POINTS == 0 ~ "NEUTRE",
      ECART_4_POINTS > 0 ~ as.character(CD_CLUB_DOMICILE),
      ECART_4_POINTS < 0 ~ as.character(CD_CLUB_EXTERIEUR)
    )
  )

library(dplyr)

df_joined <- df_joined %>%
  mutate(
    CHANGE_SENS = SENS_ECART_4_POINTS != lag(SENS_ECART_4_POINTS)
  )

df_joined <- df_joined %>%
  mutate(
    LB_RESULTAT_AVANT_CHANGEMENT = sapply(seq_len(n()), function(i) {
      
      if (is.na(CHANGE_SENS[i]) || !CHANGE_SENS[i]) {
        return(NA_character_)
      }
      
      # indices des 5 lignes précédentes max
      idx <- (i - 1):(i - 5)
      idx <- idx[idx > 0]
      
      vals <- df_joined$LB_RESULTAT[idx]
      
      res <- vals[vals != "TIR"]
      
      if (length(res) > 0) res[1] else NA_character_
    })
  ) %>%
  select(-CHANGE_SENS)

library(dplyr)

freq_lb <- df_joined %>%
  filter(!is.na(LB_RESULTAT_AVANT_CHANGEMENT)) %>%
  count(LB_RESULTAT_AVANT_CHANGEMENT)

freq_lb

freq_lb <- df_joined %>%
  filter(!is.na(LB_RESULTAT_AVANT_CHANGEMENT)) %>%
  count(LB_RESULTAT_AVANT_CHANGEMENT) %>%
  mutate(
    FREQUENCE = n / sum(n)
  )

freq_lb
