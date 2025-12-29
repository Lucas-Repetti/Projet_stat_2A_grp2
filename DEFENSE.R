library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)


#Défense individuelle vs Défense de zone
#Défense de zone : faute regroupées dans une zone, majoritairement entre 9 et 6m
#Défense individuelle : fautes éparpillées, beaucoup de fautes loin du but, parfois proche de la ligne médiane, fautes latérales


faute <- actions_clees_details %>%
  filter(LB_RESULTAT =="FAUTE") %>%
  inner_join(
    joueurs_competition_saison,
    by = c("ID_JOUEUR_DEFENSE" = "ID_JOUEUR")
  ) %>%
  select(
    CD_MATCH,
    LB_RESULTAT,
    LB_RESULTAT_DETAIL,
    LB_RESULTAT_SECTEUR_DETAIL,
    X_RESULTAT,
    Y_RESULTAT,
    ID_JOUEUR_DEFENSE,
    CD_CLUB.y
  )



compte_fautes <- faute %>%
  group_by(CD_CLUB.y, LB_RESULTAT_SECTEUR_DETAIL) %>%
  summarise(
    n = n(),
    .groups = "drop"
  )

faute_regroupee <- faute %>%
  mutate(
    SECTEUR_GROUPE = case_when(
      LB_RESULTAT_SECTEUR_DETAIL %in% c(
        "9-6 CENTRE",
        "9-6 DROITE",
        "9-6 GAUCHE"
      ) ~ "9-6",
      
      LB_RESULTAT_SECTEUR_DETAIL %in% c(
        "12-9 CENTRE",
        "12-9 GAUCHE",
        "12-9 DROITE",
        "AILE GAUCHE",
        "AILE DROITE"
      ) ~ "AUTRES",
      
      TRUE ~ NA_character_
    )
  )
compte_fautes_sanction_groupe <- faute_regroupee %>%
  group_by(CD_CLUB.y, CD_MATCH,SECTEUR_GROUPE, LB_RESULTAT_DETAIL) %>%
  summarise(
    n = n(),
    .groups = "drop"
  )

ggplot(faute_regroupee, aes(x = LB_RESULTAT_DETAIL)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Nombre de chaque type de sanction",
       x = "Type de sanction",
       y = "Nombre") +
  theme_minimal()

#La majorité des fautes sont des neutralisations

faute_regroupee_secteur <- faute_regroupee %>%
  filter(!LB_RESULTAT_SECTEUR_DETAIL=="")


ggplot(faute_regroupee_secteur, aes(x = LB_RESULTAT_SECTEUR_DETAIL)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Représentation de la zone de la faute",
       x = "Zone de la faute",
       y = "Nombre") +
  theme_minimal()

#Beaucoup de fautes sont provoqués au 9-6 centre.

"-------------------------------------------------------------------------
Il manque 23 matchs EURO JO CHAMP 2020-2021 aux détails des actions"

matchs_fr <- matchs %>%
  filter(
    str_starts(CD_MATCH, "EURO-H_2022") |
      str_starts(CD_MATCH, "FR-1-H_32") |
      str_starts(CD_MATCH, "JO-H-2021")
  ) %>%
  distinct(CD_MATCH)

fautes_fr <- faute %>%
  distinct(CD_MATCH)

manquants_dans_fautes <- anti_join(
  matchs_fr,
  fautes_fr,
  by = "CD_MATCH"
)
"--------------------------------------------------------------------------"

compte_fautes_groupe <- faute_regroupee %>%
  group_by(CD_CLUB.y,SECTEUR_GROUPE) %>%
  summarise(
    n = n(),
    .groups = "drop"
  )


sept_metres <- actions_clees_details %>%
  filter(LB_SEQUENCE_TYPE =="JET DE 7M") %>%
  inner_join(
    joueurs_competition_saison,
    by = c("ID_JOUEUR_DEFENSE" = "ID_JOUEUR")) %>%
  select(
    CD_MATCH,
    LB_SEQUENCE_TYPE,
    ID_JOUEUR_DEFENSE,
    CD_CLUB.y
  )

compte_sept_metres <- sept_metres %>%
  group_by(CD_CLUB.y) %>%
  summarise(
    sept_metres_concedes = n(),
    .groups = "drop"
  )

#Dans notre approche, une perte de balle est forcément due à une bonne défense

recuperation_possession<-actions_clees_details %>%
  filter(LB_RESULTAT =="PERTE DE BALLE") %>%
  inner_join(
    joueurs_competition_saison,
    by = c("ID_JOUEUR_DEFENSE" = "ID_JOUEUR")) %>%
  select(
    CD_MATCH,
    LB_RESULTAT,
    ID_JOUEUR_DEFENSE,
    CD_CLUB.y
  )

compte_recuperation_possession <- recuperation_possession %>%
  group_by(CD_CLUB.y) %>%
  summarise(
    recuperation_possession = n(),
    .groups = "drop"
  )

type_defense <- compte_fautes_sanction_groupe %>%
  group_by(CD_CLUB.y) %>%
  summarise(
    matchs = n_distinct(CD_MATCH),
    fautes = sum(n),
    neuf_six = sum(n * (SECTEUR_GROUPE == "9-6"), na.rm = TRUE),
    autres = sum(n * (SECTEUR_GROUPE == "AUTRES"), na.rm = TRUE),
    ratio_autres = autres / fautes,
    deux_minutes = sum (n*(LB_RESULTAT_DETAIL=="2 MINUTES"), na.rm=TRUE),
    carton_jaune = sum (n*(LB_RESULTAT_DETAIL=="CARTON JAUNE"), na.rm=TRUE),
    neutralisation = sum (n*(LB_RESULTAT_DETAIL=="NEUTRALISATION"), na.rm=TRUE),
    deux_minutes_match = deux_minutes / matchs,
    carton_jaune_match = carton_jaune / matchs,
    neutralisation_match = neutralisation / matchs,
    
    
    .groups = "drop"
  )

type_defense<-type_defense %>%
  inner_join(compte_sept_metres, by= c("CD_CLUB.y"="CD_CLUB.y")) 

type_defense<-type_defense %>%
  inner_join(compte_recuperation_possession, by= c("CD_CLUB.y"="CD_CLUB.y"))  

type_defense <- type_defense %>%
  group_by(CD_CLUB.y) %>%
  summarise(
    matchs,
    fautes,
    neuf_six,
    autres,
    ratio_autres,
    deux_minutes,
    carton_jaune,
    neutralisation,
    recuperation_possession,
    sept_metres_concedes,
    fautes_match = fautes/matchs,
    deux_minutes_match,
    carton_jaune_match,
    neutralisation_match,
    recuperation_possession_match = recuperation_possession/matchs,
    sept_metres_concedes_match = sept_metres_concedes/matchs,
    indice_defensif = neutralisation_match+2*carton_jaune_match+6*deux_minutes_match+15*sept_metres_concedes_match,
    
    
    .groups = "drop"
  )


type_defense <- type_defense %>%
  mutate(
    TYPE_DEFENSE = case_when(
      ratio_autres < 0.20 ~ "Zone",
      ratio_autres >= 0.20 & ratio_autres <= 0.30 ~ "Zone agressive / hybride",
      ratio_autres > 0.30 ~ "Individualisée",
      TRUE ~ NA_character_
    )
  )

type_defense<-type_defense %>%
  inner_join(
    equipes_selections,
    by = c("CD_CLUB.y" = "CD_CLUB")
  ) %>%
  select(
    LB_CLUB,
    matchs,
    fautes,
    neuf_six,
    autres,
    ratio_autres,
    TYPE_DEFENSE,
    deux_minutes,
    carton_jaune,
    neutralisation,
    recuperation_possession,
    sept_metres_concedes,
    fautes_match,
    deux_minutes_match,
    carton_jaune_match,
    neutralisation_match,
    recuperation_possession_match,
    sept_metres_concedes_match,
    indice_defensif
  )

type_defense<- type_defense %>%
  mutate(ratio_autres=round(ratio_autres,2),
         fautes_match=round(fautes_match,2),
         deux_minutes_match=round(deux_minutes_match,2),
         carton_jaune_match=round(carton_jaune_match,2),
         neutralisation_match=round(neutralisation_match,2),
         recuperation_possession_match=round(recuperation_possession_match,2),
         sept_metres_concedes_match=round(sept_metres_concedes_match,2),
         indice_defensif=round(indice_defensif,2))

type_defense_indice<- type_defense %>%
  arrange(indice_defensif) %>%
  select(
    LB_CLUB,
    TYPE_DEFENSE,
    matchs,
    indice_defensif
  )

#Les équipes qui ont joué le moins de match sont avantagés et ont des indices défensifs plus faibles que les autres équipes
#Nantes a le meilleur indice du championnat français
# On a attribué un poids de 1 aux neutralisations, 2 carton jaune, 6 2minutes et 15 7mètres

type_defense_recup<-type_defense %>%
  arrange(desc(recuperation_possession_match))%>%
  select(
    LB_CLUB,
    TYPE_DEFENSE,
    matchs,
    recuperation_possession_match
  )

#Nantes est l'équipe qui récupère le plus de ballon par match

matchs_2020_2022<-stats_equipes_matchs%>%
  filter(
    str_starts(CD_MATCH, "EURO-H_2022") |
      str_starts(CD_MATCH, "FR-1-H_32") |
      str_starts(CD_MATCH, "JO-H-2021")
  )

#On exclut les 23 matchs non traités dans actions_clees_details
matchs_2020_2022<- matchs_2020_2022%>%
  anti_join(manquants_dans_fautes, by = "CD_MATCH")


buts_encaisses_par_equipe <- matchs_2020_2022 %>%
  # faire un join avec le même match mais l'autre équipe
  left_join(
    matchs_2020_2022 %>% 
      select(CD_MATCH, CD_CLUB, NB_BUT_ALL) %>%
      rename(Adversaire = CD_CLUB, Buts_ADVERSAIRE = NB_BUT_ALL),
    by = "CD_MATCH"
  ) %>%
  filter(CD_CLUB != Adversaire) %>%
  group_by(CD_CLUB) %>%
  summarise(
    total_buts_encaisses = sum(Buts_ADVERSAIRE, na.rm = TRUE)
  )

nombre_matchs_equipe <- matchs_2020_2022 %>%
  group_by(CD_CLUB) %>%
  summarise(nombre_matchs = n_distinct(CD_MATCH))

equipes_2020_2022<- buts_encaisses_par_equipe %>%
  inner_join(
    nombre_matchs_equipe,
    by = c("CD_CLUB" = "CD_CLUB")
  ) %>%
  mutate(buts_encaisses_moyen=round((total_buts_encaisses/nombre_matchs),2))%>%
  select(
    CD_CLUB,
    nombre_matchs,
    total_buts_encaisses,
    buts_encaisses_moyen
  )

equipes_2020_2022<- equipes_2020_2022 %>%
  inner_join(
    equipes_selections,
    by = c("CD_CLUB" = "CD_CLUB")
  ) %>%
  mutate(buts_encaisses_moyen=round((total_buts_encaisses/nombre_matchs),2))%>%
  select(
    LB_CLUB,
    CD_CLUB,
    nombre_matchs,
    total_buts_encaisses,
    buts_encaisses_moyen
  )

equipes_2020_2022<- equipes_2020_2022 %>%
  inner_join(
    type_defense,
    by = c("LB_CLUB" = "LB_CLUB")
  ) %>%
  mutate(buts_encaisses_moyen=round((total_buts_encaisses/nombre_matchs),2))%>%
  select(
    LB_CLUB,
    CD_CLUB,
    nombre_matchs,
    total_buts_encaisses,
    buts_encaisses_moyen,
    TYPE_DEFENSE
  )


meilleure_dfense_buts_encaisses<-equipes_2020_2022 %>%
  arrange(buts_encaisses_moyen)

#HBC NANTES A LA MEILLEURE DEFENSE SUIVI DE L'EQUIPE D'ALLEMAGNE ET AIX

compte_type_defense <- equipes_2020_2022 %>%
  group_by(TYPE_DEFENSE) %>%
  summarise(
    n = n(),
    .groups = "drop"
  )

#Il y a beaucoup de défenses agressives/hybrides (environ 68%)

ggplot(equipes_2020_2022, aes(x = TYPE_DEFENSE)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Répartition des structures de défense",
       x = "Type de défense",
       y = "Nombre") +
  theme_minimal()


indice_general <- type_defense %>%
  inner_join(equipes_2020_2022, by = "LB_CLUB") %>%
  rename(TYPE_DEFENSE = TYPE_DEFENSE.x) %>%
  mutate(
    indice_general = indice_defensif + 4 * buts_encaisses_moyen
  ) %>%
  select(
    LB_CLUB,
    CD_CLUB,
    TYPE_DEFENSE,
    matchs,
    indice_defensif,
    buts_encaisses_moyen,
    indice_general
  ) %>%
  arrange(indice_general)

# on a sélectionné un poids de 4 pour les buts encaissés puisque cela dépend du rythme de match et du niveau de l'adversaire donc ça ne représente pas à 100% l'efficacité d'une défense

indice_general_fr<-indice_general %>%
  filter(
    str_starts(CD_CLUB, "FR") ) %>%
  mutate(rang_indice=row_number())

classement_champ_fr_2021<-classement_champ_fr_2021 %>%
  mutate(rang_reel=row_number())

comparaison_classement <- classement_champ_fr_2021 %>%
  select(LB_CLUB, rang_reel) %>%
  inner_join(
    indice_general_fr %>% select(LB_CLUB, rang_indice, TYPE_DEFENSE),
    by = "LB_CLUB"
  ) %>%
  mutate(
    ecart_rang = rang_indice - rang_reel,
    abs_ecart = abs(ecart_rang)
  )

#On observe un écart important entre le classement réel et le classement selon l'indice défensif. Ainsi, la défense ne peut pas expliquée à elle seule la performance d'une équipe.


anova_defense <- aov(buts_encaisses_moyen ~ TYPE_DEFENSE, data = equipes_2020_2022)
summary(anova_defense)
shapiro.test(residuals(anova_defense)) #normalité rejetée
bartlett.test(buts_encaisses_moyen ~ TYPE_DEFENSE, data = equipes_2020_2022)
qqnorm(residuals(anova_defense))
qqline(residuals(anova_defense)) #problème de normalité

#Alternative à l'ANOVA car hypothèses non vérifiées
kruskal.test(buts_encaisses_moyen ~ TYPE_DEFENSE, data = equipes_2020_2022)
#p-value =0.4813, les buts encaissés moyens ont la même distribution quel que soit le type de défense

ggplot(equipes_2020_2022, aes(x = TYPE_DEFENSE, y = buts_encaisses_moyen)) +
  geom_boxplot(fill = "white") +
  labs(
    title = "Buts encaissés moyens selon le type de défense",
    x = "Type de défense",
    y = "Buts encaissés moyens"
  ) +
  theme_minimal()

#Les défenses agressives semblent encaisser plus de buts mais attention, représentent la plus grande part des défenses

ggplot(type_defense_recup, aes(x = TYPE_DEFENSE, y = recuperation_possession_match)) +
  geom_boxplot(fill = "white") +
  labs(
    title = "Récupérations de posssession par match selon le type de défense",
    x = "Type de défense",
    y = "Nombre moyen de récupérations de possession"
  ) +
  theme_minimal()

#Les défenses agressives semblent récupérer plus de ballons

ggplot(type_defense, aes(x = TYPE_DEFENSE, y = fautes_match)) +
  geom_boxplot(fill = "white") +
  labs(
    title = "Fautes par match selon le type de défense",
    x = "Type de défense",
    y = "Nombre moyen de fautes"
  ) +
  theme_minimal()

#Les défenses en zone font moins de fautes, ce qui colle à la littérature, tandis que les défenses individuelles font beaucoup de fautes


#Possibilité de modifier les déterminants du type de défense pour qu'il y ait moins de défenses agressives
#On pourra aussi passer au niveau individuel, et voir si la structure défensive d'une équipe est la même lors de chaue match
#Nantes paraît être la meilleure défense, on pourr faire une heatmap des fautes et des récupérations de balle
#Une fois déterminée la meilleure structure de défense, on pourra aider les coachs en indiquant quels types de joueurs doivent être utilisés dans leur système et proposer des noms, avec un clustering
#On pourra aussi observer si certains joueurs ont joué dans des structures défensives différentes (équipe vs sélection) et voir les différences