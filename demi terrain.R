library(ggplot2)

# -----------------------------
# Paramètres terrain et cage
# -----------------------------
x_centre <- 0.5
y_haut <- 1

largeur_cage <- 0.15
largeur_ligne <- 0.15

x_min_cage <- x_centre - largeur_cage/2
x_max_cage <- x_centre + largeur_cage/2

y_6m <- 0.7
y_9m <- 0.55
x_min_ligne <- x_centre - largeur_ligne/2
x_max_ligne <- x_centre + largeur_ligne/2

# -----------------------------
# Paramètres arcs originaux (rayon 0.3)
# -----------------------------
centre_x1 <- 0.425; centre_y1 <- 1; rayon1 <- 0.3
angles1 <- seq(-pi, -pi/2, length.out = 100)
arc1 <- data.frame(x = centre_x1 + rayon1 * cos(angles1),
                   y = centre_y1 + rayon1 * sin(angles1))

centre_x2 <- 0.575; centre_y2 <- 1; rayon2 <- 0.3
angles2 <- seq(-pi/2, pi, length.out = 100)
arc2 <- data.frame(x = centre_x2 + rayon2 * cos(angles2),
                   y = centre_y2 + rayon2 * sin(angles2))

# -----------------------------
# Nouveaux arcs pointillés (rayon 0.45)
# -----------------------------
centre_x3 <- 0.425; centre_y3 <- 1; rayon3 <- 0.45
angles3 <- seq(-pi, -pi/2, length.out = 100)
arc3 <- data.frame(x = centre_x3 + rayon3 * cos(angles3),
                   y = centre_y3 + rayon3 * sin(angles3))

centre_x4 <- 0.575; centre_y4 <- 1; rayon4 <- 0.45
angles4 <- seq(-pi/2, pi, length.out = 100)
arc4 <- data.frame(x = centre_x4 + rayon4 * cos(angles4),
                   y = centre_y4 + rayon4 * sin(angles4))

# -----------------------------
# Définition des 6 zones avec ajustement avant
# -----------------------------
zones <- data.frame(
  zone = c("Arrière gauche", "Arrière centre", "Arrière droite",
           "Avant gauche", "Avant centre", "Avant droite"),
  x_min = c(0, 0.33, 0.66, 0, 0.25, 0.75),
  x_max = c(0.33, 0.66, 1, 0.25, 0.75, 1),
  y_min = c(0, 0, 0, 0.5, 0.5, 0.5),
  y_max = c(0.5, 0.5, 0.5, 1, 1, 1)
)

# Barycentres : arrière automatique, avant personnalisés
zones$x_bar <- c((zones$x_min[1:3]+zones$x_max[1:3])/2, 0.125, 0.5, 0.875)
zones$y_bar <- c((zones$y_min[1:3]+zones$y_max[1:3])/2, 0.65, 0.525, 0.65)

# -----------------------------
# Graphique demi-terrain avec zones et barycentres ajustés
# -----------------------------
p_terrain <- ggplot() +
  # Demi-terrain (fond)
  geom_rect(xmin=0, xmax=1, ymin=0, ymax=1, fill="grey95", color="black", linewidth=1) +
  
  # Cage (sans profondeur)
  geom_segment(aes(x=x_min_cage, y=y_haut, xend=x_max_cage, yend=y_haut),
               color="red", linewidth=4) +
  
  # Ligne de 6m
  geom_segment(aes(x=x_min_ligne, y=y_6m, xend=x_max_ligne, yend=y_6m),
               color="black", linewidth=1) +
  
  # Ligne de 9m
  geom_segment(aes(x=x_min_ligne, y=y_9m, xend=x_max_ligne, yend=y_9m),
               color="black", linetype="dashed", linewidth=1) +
  
  # Arcs originaux
  geom_path(data=arc1, aes(x=x, y=y), color="black", linewidth=1) +
  geom_path(data=arc2, aes(x=x, y=y), color="black", linewidth=1) +
  
  # Nouveaux arcs pointillés
  geom_path(data=arc3, aes(x=x, y=y), color="black", linetype="dashed", linewidth=1) +
  geom_path(data=arc4, aes(x=x, y=y), color="black", linetype="dashed", linewidth=1) +
  
  # Zones
  geom_rect(data=zones,
            aes(xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max),
            fill=NA, color="blue", linewidth=0.8) +
  
  # Barycentres
  geom_point(data=zones,
             aes(x=x_bar, y=y_bar),
             color="darkblue", size=3) +
  
  # Étiquettes des zones
  geom_text(data=zones,
            aes(x=x_bar, y=y_bar, label=zone),
            color="darkblue", size=3, vjust=-1) +
  
  coord_fixed(ratio=1) +
  scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  
  labs(title="Demi-terrain avec zones avant ajustées et barycentres personnalisés") +
  theme_minimal() +
  theme(
    panel.grid=element_blank(),
    panel.border = element_rect(color="black", fill=NA, linewidth=1.5)
  )
