library(readr)
library(dplyr)

df <- read_csv("data/FCT_MATCH_202109242114.csv")


df_sel <- df %>%
  select(LB_RESULTAT,
         CD_MATCH,
         NB_SCORE_DOMICILE,
         NB_SCORE_EXTERIEUR,
         CD_CLUB)

compare_scores <- function(df, row_index, threshold) {
  # Extraire la ligne de départ
  initial_row <- df[row_index, ]
  cd_match <- initial_row$CD_MATCH
  
  # Somme initiale
  sum_initial <- initial_row$NB_SCORE_DOMICILE + initial_row$NB_SCORE_EXTERIEUR
  
  # Différence initiale
  diff_initial <- initial_row$NB_SCORE_DOMICILE - initial_row$NB_SCORE_EXTERIEUR
  
  last_row_index <- row_index
  
  # Parcourir les lignes suivantes
  for (i in (row_index + 1):nrow(df)) {
    if (df$CD_MATCH[i] != cd_match) {
      break
    }
    
    sum_next <- df$NB_SCORE_DOMICILE[i] + df$NB_SCORE_EXTERIEUR[i]
    
    if (abs(sum_next - sum_initial) > threshold) {
      break
    }
    
    last_row_index <- i
  }
  
  diff_last <- df$NB_SCORE_DOMICILE[last_row_index] - df$NB_SCORE_EXTERIEUR[last_row_index]
  
  return(c(diff_last, diff_initial))
}

compare_scores(df_sel, row_index = 1, threshold = 5)


