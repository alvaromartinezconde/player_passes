library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(ggforce)
library(ggExtra)

# Fijar directorio de trabajo

setwd("C:/Users/mcond/OneDrive/Escritorio")


# Cargar datos de trabajo y visualizar tabla de datos
full_data <- read_delim("full_data.txt", ";", 
                        locale = locale(decimal_mark = ","),
                        trim_ws = TRUE)

full_data %>% 
  
  select(Suceso, Resultado, Jugador, Equipo, Rival) %>% 
  filter(Suceso == "Pass" & Jugador == "Busquets")


# Elegir jugador a analizar
jugador_elegido <- c("Busquets")
# Elegir rival
rival_elegido <- c("Real Madrid")
# Elegir partido (1 = Real Madrid local, 0 = Real Madrid visitante)
partido_elegido <- c("0")
# Elegir seccion, ver full_data para identificar qué es sección
evento_elegido <- c("Pass")
data_player <- subset(full_data, Jugador == jugador_elegido & 
                        Rival == rival_elegido & 
                        Local == partido_elegido & 
                        Suceso == evento_elegido)

# Crear campo de fútbol con distribución de puntos correspondientes 
# a la posición (x,y) de cada evento
player_scatter_plot <- ggplot() +
  # Crear nueve de puntos, corrigiendo coordenada x (debido a fuente de datos)
  geom_point(data_player, mapping = aes(x = x1 - 58.015,
                                        y = y1,
                                        fill = factor(data_player$Resultado)),
             shape = 21,
             size = 5) +
  # Activar, si se desea, de modo que los eventos tengan color corporativo, 
  # de acuerdo a cómo se ha definido variable realmadrid_colors
  #scale_fill_manual(values = realmadrid_colors) +
  guides(fill = guide_legend(title = NULL)) +
  # Comenzar a representar campo de fútbol como combinación de sucesivos 
  # rectángulos. Las dimensiones del campo
  # vienen marcadas por la fuente de los datos
  geom_rect(mapping = aes(xmin = 0.0, xmax = 596.97, 
                          ymin = 50.5, ymax = 446.5),
            color ="green", fill = NA, alpha = 0.1) +
  geom_rect(mapping = aes(xmin = 0.0, xmax = 91.935, 
                          ymin = 128.975, ymax = 368.025), 
            color ="green", fill = NA, alpha = 0.1) +
  geom_rect(mapping = aes(xmin = 505.035, xmax = 596.97, 
                          ymin = 128.975, ymax = 368.025), 
            color ="green", fill = NA, alpha = 0.1) +
  geom_rect(mapping = aes(xmin = 0.0, xmax = 29.858, 
                          ymin = 195, ymax = 302.0), 
            color ="green", fill = NA, alpha = 0.1) +
  geom_rect(mapping = aes(xmin = 567.112, xmax = 596.97, 
                          ymin = 195, ymax = 302.0), 
            color ="green", fill = NA, alpha = 0.1) +
  
  # Continuación de la representación del campo, con línea de medio campo,
  # circulo del centro y puntos de penalti
  geom_linerange(aes(x = 298.485, ymin = 50.5, 
                     ymax = 446.5), 
                 color = "green") +
  geom_point(mapping = aes(x = 66.33, y = 248.5), 
             size = 1, color = "green") +
  geom_point(mapping = aes(x = 530.64, y = 248.5), 
             size = 1, color = "green") +
  geom_circle(mapping = aes(x0 = 298.485, 
                            y0 = 248.5, r = 52), 
              color = "green") +
  coord_fixed() +
  
  # Añadimos el semicirculo de los corners y el semicirculo de las áreas
  
  geom_arc(mapping = aes(x0 = 91.935,
                         
                         y0 = 248.5, r =45,
                         
                         start = 0,
                         
                         end = 3.1),
           
           color = "green") +
  
  geom_arc(mapping = aes(x0 = 505.035,
                         
                         y0 = 248.5, r =45,
                         
                         start = 3.1,
                         
                         end = 6.3),
           
           color = "green") +
  
  geom_arc(mapping = aes(x0 = 0,
                         
                         y0 = 50.5, r =7,
                         
                         start = 0,
                         
                         end = 1.6),
           
           color = "green") +
  
  geom_arc(mapping = aes(x0 = 0,
                         
                         y0 = 446.5, r =7,
                         
                         start = 1.55,
                         
                         end = 3.1),
           
           color = "green") +
  
  geom_arc(mapping = aes(x0 = 596.97,
                         
                         y0 = 50.5, r =7,
                         
                         start = 4.65,
                         
                         end = 6.3),
           
           color = "green") +
  
  geom_arc(mapping = aes(x0 = 596.97,
                         
                         y0 = 446.5, r =7,
                         
                         start = 3.05,
                         
                         end = 4.65),
           
           color = "green") +
  
  # Limpiar representación gráfica para que quede sólo el terreno de juego
  theme_no_axes(base.theme = theme_bw()) +
  theme(legend.position = c(0.5, 0.04),
        legend.box = "horizontal",
        legend.direction = "horizontal",
        legend.box.background = element_rect(fill = "transparent",
                                             colour = "transparent"),
        legend.text = element_text(size = 14),
        panel.border = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) +
  #        plot.margin=unit(c(-0.05,-0.05,-0.05,-0.1),"in")) +
  scale_x_continuous(limits = c(0,647.47), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,497), expand = c(0,0))

# Visualizar campo de fútbol con información representada
player_scatter_plot


# Completar gráfico con información marginal, donde se muestra 
# histograma en x e y del evento elegido.
ggExtra::ggMarginal(player_scatter_plot, 
                    x = x1 - 258.015, 
                    y = y1, 
                    type = "histogram", 
                    xparams = list(breaks = seq(0,600,50), 
                                   colour = "#00529f"),
                    yparams = list(breaks = seq(50,450,50),
                                   colour = "#00529f"))
