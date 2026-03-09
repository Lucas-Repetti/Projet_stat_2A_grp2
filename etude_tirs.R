library(ggplot2)
library(stringr)
library(dplyr)


actions_FR <- actions_clees_details %>%
  filter(str_starts(CD_MATCH, "FR")) %>% 
  mutate(ligne = row_number())

actions_FR <- actions_FR %>%
  mutate(
    X_CAGE = as.numeric(X_CAGE),
    Y_CAGE = as.numeric(Y_CAGE)
  )
      
lignes_sup1 <- which(actions_FR$X_CAGE > 1 | actions_FR$Y_CAGE > 1)
lignes_sup1
actions_FR <- actions_FR[-lignes_sup1, ]


ggplot(actions_FR, aes(x = X_CAGE, y = Y_CAGE)) +
  geom_point(color = "blue", size = 1) +
  labs(
    title = "Y_CAGE en fonction de X_CAGE",
    x = "X_CAGE",
    y = "Y_CAGE"
  ) +
  theme_minimal() +
  coord_fixed()                       

ggplot(subset(actions_FR, LB_RESULTAT_DETAIL == "BUT"), 
       aes(x = X_CAGE, y = Y_CAGE)) +
  geom_point(color = "red", size = 2) +
  geom_vline(xintercept = c(0.10, 0.90), color = "green", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 0.20, color = "green", linetype = "dashed", size = 1) +  # <-- ici
  coord_fixed(ratio = 1) +
  scale_x_continuous(breaks = function(x) sort(unique(c(x, 0, 1)))) +
  scale_y_continuous(breaks = function(y) sort(unique(c(y, 0, 1)))) +
  labs(title = "Y_CAGE pour LB_RESULTAT_DETAIL = BUT",
       x = "X_CAGE", y = "Y_CAGE") +
  theme_minimal()

actions_FR_TIR <- subset(actions_FR, LB_RESULTAT == "TIR")

ggplot(subset(actions_FR_TIR, LB_RESULTAT_DETAIL != "BUT"), 
       aes(x = X_CAGE*4, y = (1-Y_CAGE)*(8/3), color = LB_RESULTAT_DETAIL)) +
  geom_point(size = 2) +
  
  # lignes verticales
  geom_segment(aes(x = 0.12*4, xend = 0.12*4, y = 0.88*(8/3), yend = 0*(8/3)), color = "red", linetype = "dashed", size = 1) +
  geom_segment(aes(x = 0.07*4, xend = 0.07*4, y = 0.88*(8/3), yend = 0*(8/3)), color = "red", linetype = "dashed", size = 1) +
  geom_segment(aes(x = 0.88*4, xend = 0.88*4, y = 0.88*(8/3), yend = 0*(8/3)), color = "red", linetype = "dashed", size = 1) +
  geom_segment(aes(x = 0.93*4, xend = 0.93*4, y = 0.88*(8/3), yend = 0*(8/3)), color = "red", linetype = "dashed", size = 1) +
  
  # lignes horizontales
  geom_segment(aes(x = 0.06*4, xend = 0.94*4, y = 0.8*(8/3), yend = 0.8*(8/3)), color = "red", linetype = "dashed", size = 1) +
  geom_segment(aes(x = 0.06*4, xend = 0.94*4, y = 0.88*(8/3), yend = 0.88*(8/3)), color = "red", linetype = "dashed", size = 1) +
  
  coord_fixed(ratio = 1) +
  labs(title = "Y_CAGE pour LB_RESULTAT_DETAIL != BUT",
       x = "X_CAGE", y = "Y_CAGE", color = "Résultat") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggplot(subset(actions_FR, LB_RESULTAT_DETAIL == "BUT"), 
       aes(x = X_CAGE, y = Y_CAGE)) +
  geom_point(color = "red", size = 1) +           # points réels
  geom_density_2d(color = "blue") +               # lignes de densité
  labs(title = "Contours de densité des BUTS dans la cage",
       x = "X_CAGE",
       y = "Y_CAGE") +
  coord_fixed(ratio = 1) +
  theme_minimal()


# Préparer les données avec une variable couleur
actions_buts <- subset(actions_FR, LB_RESULTAT_DETAIL == "BUT") %>%
  mutate(couleur_point = ifelse(FG_ATTAQUE_BUT_VIDE == 1, "bleu", "rouge"))

ggplot(actions_buts, aes(x = X_CAGE, y = Y_CAGE, color = couleur_point)) +
  geom_point(size = 1) +           # points colorés selon la condition
  geom_density_2d(color = "black") +  # contours de densité
  labs(title = "Contours de densité des BUTS dans la cage",
       x = "X_CAGE",
       y = "Y_CAGE",
       color = "Type de but") +
  scale_color_manual(values = c("rouge" = "red", "bleu" = "blue")) +
  coord_fixed(ratio = 1) +
  theme_minimal()


# Préparer les données avec une catégorie “BUT VIDE” ou “BUT NON VIDE”
actions_buts <- subset(actions_FR, LB_RESULTAT_DETAIL == "BUT") %>%
  mutate(type_but = ifelse(FG_ATTAQUE_BUT_VIDE == 1, "BUT VIDE", "BUT NON VIDE"))

# Densité superposée pour Y_CAGE
ggplot(actions_buts, aes(x = Y_CAGE, fill = type_but)) +
  geom_density(alpha = 0.5) +  # alpha pour transparence
  scale_fill_manual(values = c("BUT VIDE" = "blue", "BUT NON VIDE" = "red")) +
  labs(title = "Densité de Y_CAGE : BUTS VIDE vs NON VIDE",
       x = "Y_CAGE",
       y = "Densité",
       fill = "Type de but") +
  theme_minimal()

# Densité superposée pour Y_CAGE
ggplot(actions_buts, aes(x = X_CAGE, fill = type_but)) +
  geom_density(alpha = 0.5) +  # alpha pour transparence
  scale_fill_manual(values = c("BUT VIDE" = "blue", "BUT NON VIDE" = "red")) +
  labs(title = "Densité de Y_CAGE : BUTS VIDE vs NON VIDE",
       x = "Y_CAGE",
       y = "Densité",
       fill = "Type de but") +
  theme_minimal()





















# Filtrer uniquement les tirs arrêtés et valeurs numériques valides
arrets <- actions_FR %>%
  filter(LB_RESULTAT_DETAIL == "ARRET") %>%
  mutate(
    X_NUM = as.numeric(as.character(X_CAGE)),
    Y_NUM = as.numeric(as.character(Y_CAGE))
  ) %>%
  filter(!is.na(X_NUM) & !is.na(Y_NUM))

# Vérifier qu'il reste des lignes
nrow(arrets)  # doit être > 0

# Créer les zones (5x5)
arrets <- arrets %>%
  mutate(
    zone_x = cut(X_NUM, breaks = 5, labels = FALSE),
    zone_y = cut(Y_NUM, breaks = 5, labels = FALSE)
  )

# Compter les arrêts par zone
zones_count <- arrets %>%
  group_by(zone_x, zone_y) %>%
  summarise(nb_arrets = n(), .groups = "drop") %>%
  arrange(desc(nb_arrets))

zones_count

# 4️⃣ Définir un seuil pour les "excellent arrêt" : zones avec peu d'arrêts
seuil_excellent <- quantile(zones_count$nb_arrets, 0.25)  # 25% des zones les moins arrêtées

# 5️⃣ Ajouter la colonne "excellent_arret" à actions_FR
# On fait un left_join pour récupérer nb_arrets par zone
actions_FR <- actions_FR %>%
  left_join(
    arrets %>% select(X_CAGE, Y_CAGE, zone_x, zone_y, nb_arrets = X_NUM), # on ajoutera nb_arrets plus tard
    by = c("X_CAGE", "Y_CAGE")
  )

# Puis créer "excellent_arret" avec les zones correctes
# Ici on refait le mutate sur la base des zones valides
actions_FR <- actions_FR %>%
  mutate(
    X_NUM = as.numeric(as.character(X_CAGE)),
    Y_NUM = as.numeric(as.character(Y_CAGE)),
    zone_x = cut(X_NUM, breaks = 5, labels = FALSE),
    zone_y = cut(Y_NUM, breaks = 5, labels = FALSE),
    excellent_arret = ifelse(
      LB_RESULTAT_DETAIL == "ARRET" &
        !is.na(zone_x) & !is.na(zone_y) &
        nb_arrets <= seuil_excellent,
      1, 0
    )
  )

# Vérifier le résultat
table(actions_FR$excellent_arret, useNA = "ifany")
