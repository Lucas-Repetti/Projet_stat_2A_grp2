library(dplyr)
library(stringr)
library(plotly)
library(ggplot2)


####### MISE EN PLACE ####### 
actions_FR <- actions_clees_details %>%
  filter(str_starts(CD_MATCH, "FR"))


actions_FR <- actions_FR %>%
  mutate(
    X_RESULTAT = as.numeric(X_RESULTAT),
    Y_RESULTAT = as.numeric(Y_RESULTAT)
  )


####### VISU TIRS ####### 

p_terrain +
  geom_point(
    data = actions_clees_details,
    aes(
      x = as.numeric(gsub(",", ".", X_RESULTAT)),
      y = as.numeric(gsub(",", ".", Y_RESULTAT))
    ),
    color = "red",
    size = 1.5,
    alpha = 0.6
  )


plot_match <- function(cd_match) {
  
  data_match <- actions_FR %>%
    dplyr::filter(CD_MATCH == cd_match) %>%
    dplyr::mutate(
      x = as.numeric(gsub(",", ".", X_RESULTAT)),
      y = as.numeric(gsub(",", ".", Y_RESULTAT)),
      CD_CLUB = as.factor(CD_CLUB)  # pour colorer par club
    )
  
  p_terrain +
    geom_point(
      data = data_match,
      aes(x = x, y = y, color = CD_CLUB),  # couleur selon club
      size = 2,
      alpha = 0.7
    ) +
    ggtitle(paste("CD_MATCH :", cd_match)) +
    scale_color_brewer(palette = "Set1")  # palette sympa pour différencier les clubs
}





plot_match_int <- function(cd_match) {
  
  data_match <- actions_FR %>%
    filter(CD_MATCH == cd_match) %>%
    mutate(
      x = as.numeric(gsub(",", ".", X_RESULTAT)),
      y = as.numeric(gsub(",", ".", Y_RESULTAT)),
      CD_CLUB = as.factor(CD_CLUB)
    ) %>%
    filter(!is.na(x), !is.na(y))  # <- supprime les points invalides
  
  if(nrow(data_match) == 0) stop("Aucune ligne trouvée ou valeurs invalides pour ce CD_MATCH")
  
  plot_ly(
    data = data_match,
    x = ~x,
    y = ~y,
    type = 'scatter',
    mode = 'markers',
    color = ~CD_CLUB,
    colors = "Set1",
    marker = list(size = 10, opacity = 0.7),
    text = ~paste(
      "Type :", LB_SEQUENCE_TYPE,
      "<br>Détail :", LB_RESULTAT_DETAIL,
      "<br>But vide :", FG_ATTAQUE_BUT_VIDE
    ),
    hoverinfo = "text"
  ) %>%
    layout(
      title = paste("CD_MATCH :", cd_match),
      xaxis = list(title = "X", range = c(0,1)),
      yaxis = list(title = "Y", range = c(0,1)),
      showlegend = TRUE
    )
}
plot_match("FR-1-H_32_25_5656")
plot_match_int("FR-1-H_32_25_5656")


####### DEF TIRS DIFF ####### 


actions_FR <- actions_FR %>%
  mutate(
    score_difficulte = 0,
    
    # Gardien
    score_difficulte = score_difficulte + ifelse(FG_ATTAQUE_BUT_VIDE == 0, 2, 0),
    
    # Secteur
    score_difficulte = score_difficulte + ifelse(LB_RESULTAT_SECTEUR == "GARDIEN", 4, 0),
    score_difficulte = score_difficulte + ifelse(LB_RESULTAT_SECTEUR == "KUNG-FU", 4, 0),
    
    # Détail du secteur
    score_difficulte = score_difficulte + ifelse(LB_RESULTAT_SECTEUR_DETAIL %in% c("AILE GAUCHE","AILE DROITE"), 2, 0),
    score_difficulte = score_difficulte + ifelse(Y_RESULTAT>0.6, 2, 0),
    score_difficulte = score_difficulte + ifelse(LB_RESULTAT_SECTEUR_DETAIL %in% c("9-6 GAUCHE","9-6 DROITE","12-9 GAUCHE","12-9 DROITE"), 1, 0),
    
    # Catégorie de tir
    score_difficulte = score_difficulte + ifelse(LB_TIR_CATEGORIE %in% c("REBOND","LOB","ROUCOULETTE"), 2, 0),
    
    # Posture
    score_difficulte = score_difficulte + ifelse(LB_TIR_POSTURE == "EN SUSPENSION", 1, 0),
    
    # Tir difficile si score >= 5
    tir_difficile = ifelse(score_difficulte >= 5 & grepl("BUT", LB_RESULTAT_DETAIL, ignore.case = TRUE), 1, 0)
  )
table(actions_FR$tir_difficile)
summary(actions_FR$score_difficulte)



####### VISU TIRS DIFF ####### 





plot_match_tirs <- function(cd_match) {
  
  data_match <- actions_FR %>%
    dplyr::filter(CD_MATCH == cd_match, tir_difficile == 1) %>%  # <- ne garder que tirs difficiles
    dplyr::mutate(
      x = as.numeric(gsub(",", ".", X_RESULTAT)),
      y = as.numeric(gsub(",", ".", Y_RESULTAT)),
      CD_CLUB = as.factor(CD_CLUB)  # pour colorer par club
    )
  
  p_terrain +
    geom_point(
      data = data_match,
      aes(x = x, y = y, color = CD_CLUB),  # couleur selon club
      size = 2,
      alpha = 0.7
    ) +
    ggtitle(paste("CD_MATCH :", cd_match, "- Tirs difficiles")) +
    scale_color_brewer(palette = "Set1")
}

plot_match_int_tirs <- function(cd_match) {
  
  data_match <- actions_FR %>%
    filter(CD_MATCH == cd_match, tir_difficile == 1) %>%  # <- ne garder que tirs difficiles
    mutate(
      x = as.numeric(gsub(",", ".", X_RESULTAT)),
      y = as.numeric(gsub(",", ".", Y_RESULTAT)),
      CD_CLUB = as.factor(CD_CLUB)
    ) %>%
    filter(!is.na(x), !is.na(y))
  
  if(nrow(data_match) == 0) stop("Aucune ligne trouvée ou valeurs invalides pour ce CD_MATCH")
  
  plot_ly(
    data = data_match,
    x = ~x,
    y = ~y,
    type = 'scatter',
    mode = 'markers',
    color = ~CD_CLUB,
    colors = "Set1",
    marker = list(size = 10, opacity = 0.7),
    text = ~paste(
      "Type :", LB_SEQUENCE_TYPE,
      "<br>Détail :", LB_RESULTAT_DETAIL,
      "<br>But vide :", FG_ATTAQUE_BUT_VIDE,
      "<br>Score difficulté :", score_difficulte
    ),
    hoverinfo = "text"
  ) %>%
    layout(
      title = paste("CD_MATCH :", cd_match, "- Tirs difficiles"),
      xaxis = list(title = "X", range = c(0,1)),
      yaxis = list(title = "Y", range = c(0,1)),
      showlegend = TRUE
    )
}


plot_match_tirs("FR-1-H_32_25_5656")
plot_match_int_tirs("FR-1-H_32_25_5656")


plot_match_tirs("FR-1-H_32_25_5658")
plot_match_int_tirs("FR-1-H_32_25_5658")
