## https://ggplot2.tidyverse.org/reference/


library(tidyverse)
load("econAct.RData")

head(econAct)
dim(econAct)
str(econAct)

our_data <- filter(econAct, Year == "2010", Gender == "F")


## 1. Visualizacion de variables: -----------------------------------------

ggplot(data = our_data) + 
  geom_point(mapping = aes(x = Employ.agri, y = Life.Exp, color = Income))

## Ejercicio: 
## - Anade otras esteticas al grafico: color, shape, size, alpha    
## - Prueba que pasa cuando se aplican esteticas a vars discretas/continuas    
## - ¿Que pasa cuando se usan varias esteticas?
## ========================================================================

ggplot(data = our_data) +
  geom_point(mapping = aes(x = Employ.agri, y = Life.Exp, shape = Equal.remun))

ggplot(data = our_data) +
  geom_point(mapping = aes(x = Employ.agri, y = Life.Exp, color = Employ.serv))

ggplot(data = our_data) +
  geom_point(mapping = aes(x = Employ.agri, y = Life.Exp, size = Income))
# no tiene sentido utilizar variable categorica como size, excepto si la variable es ordinal

ggplot(data = our_data) +
  geom_point(mapping = aes(x = Employ.agri, y = Life.Exp, alpha = Income))
# no tiene sentido utilizar variable categorica como alpha, excepto si la variable es ordinal

ggplot(data = our_data) +
  geom_point(mapping = aes(x = Employ.agri, y = Life.Exp, color = Equal.remun, alpha = Income))

ggplot(data = our_data) +
  geom_point(mapping = aes(x = Employ.agri, y = Life.Exp, color = Equal.remun, size = Income)) # dos esteticas dos paletas


## 2. Esteticas en funcion del tipo de variable: ---------------------

ggplot(data = econAct) + 
  geom_point(mapping = aes(x = Employ.agri, y = Life.Exp, color = Year))

ggplot(data = econAct) + 
  geom_point(mapping = aes(x = Employ.agri, y = Life.Exp, color = factor(Year)))
##


## 3. Mapping vs. Setting ---------------------------------------------

ggplot(data = our_data) +
  geom_point(aes(x = Employ.agri, y = Life.Exp, color = 'blue'))

ggplot(data = our_data) +
  geom_point(aes(x = Employ.agri, y = Life.Exp), color = 'blue')

## Ejercicio: 
## - En el grafico anterior, haz que todos los puntos con Employ.serv < 70
##   se dibujen de un color y los que tengan Employ.serv >= 70 de otro.
## ================================================================

ggplot(data = our_data) + 
  geom_point(aes(x = Employ.agri, y = Life.Exp, color = ifelse(Employ.serv < 70, 'blue', 'red')))

ggplot(data = our_data) + 
  geom_point(aes(x = Employ.agri, y = Life.Exp, color = Employ.serv < 70))
##


## 4. Otros Objetos geometricos: ---------------------

ggplot(data = our_data) + 
  geom_point(mapping = aes(x = Employ.agri, y = Life.Exp))

ggplot(data = our_data) + 
  geom_smooth(mapping = aes(x = Employ.agri, y = Life.Exp))

ggplot(data = our_data) + 
  geom_point(mapping = aes(x = Employ.agri, y = Life.Exp)) + 
  geom_smooth(mapping = aes(x = Employ.agri, y = Life.Exp))

ggplot(data = our_data) +
  geom_boxplot(mapping = aes(x = Income, y = Employ.serv, fill = Income), show.legend = F)

ggplot(data = our_data) +
  geom_histogram(aes(x = Life.Exp, fill = Income))

ggplot(data = our_data) +
  geom_density(aes(x = Life.Exp), alpha = 0.7, fill = "dodgerblue", color = "blue")

ggplot(data = our_data) + 
  geom_bar(aes(x = Income))


## 5. Global vs. local --------------------------

## Mappings y datos incluidos en `ggplot()` se aplicaran globalmente a todas las capas.

ggplot(data = our_data, aes(x = Employ.agri, y = Life.Exp)) +
  geom_point() +
  geom_smooth()

## Mappings y datos incluidos en una funcion `geom_()` sobreescribira las condiciones globales unicamente para esta capa.

ggplot(data = our_data, aes(x = Employ.agri, y = Life.Exp)) + 
  geom_point(aes(color = Income)) + 
  geom_smooth()

ggplot(data = our_data, aes(x = Employ.agri, y = Life.Exp)) + 
  geom_point(aes(color = Income)) + 
  geom_smooth(aes(color = Income))

## stat, position
p <- ggplot(our_data, aes(x = Region, fill = Income))
p + geom_bar(position='stack') # total de los valores (apilados) 
p + geom_bar(position='dodge')
p + geom_bar(position='identity') # juntar todos los valores
p + geom_bar(position='fill') # porcentaje
##


## 6. Facetas: -----------------------------------------------------
## facet_wrap(): facetas en funcion de una sola variable discreta        
## facet_grid(): funciones en funcion de 2 variables filas ~ columnas (. para no separar)       


our_data2 <- filter(econAct, Gender ==  "F")
ncountries <- length(unique(our_data2$Country))
some_countries <- unique(our_data2$Country)[sample(1:ncountries, 12)]
our_data3 <- our_data2[which(our_data2$Country %in% some_countries), ]


g <- ggplot(data = our_data3, 
            mapping = aes(x = factor(Year), y = Life.Exp, color = Region)) +
  geom_point() +
  geom_line(aes(group = Country))

g + facet_wrap(~ Country)


our_data4 <- econAct[which(econAct$Country %in% some_countries), ]

g2 <- ggplot(data = our_data4, 
             mapping = aes(x = Year,y = Life.Exp, color = Region)) +
  geom_point() +
  geom_line(aes(group = Country))
g2 + facet_grid(Gender ~ Region)


## Ejercicio: 
## - ¿Que pasa cuando se utiliza una variable continua?
## - Comprobad que pasa cuando se utiliza "." en vez de una de las variables 
##   en la formula dentro de facet_grid().       
## =========================================================================

ggplot(data = our_data) + 
  geom_point(aes(x = Employ.serv, y = Life.Exp)) + 
  facet_grid(Income ~ .)

ggplot(data = our_data) + 
  geom_point(aes(x = Employ.serv, y = Life.Exp)) + 
  facet_grid(. ~ Income)
##


## Ejercicio: 
## - Hacer un grafico con boxplots de la variable Employ.serv (aes(y)) 
##   para cada valor de Gender (aes(x)). 
## - Dividir el grafico anterior por los niveles de la variable Income (Paradoja de Simpson)
##   colorea boxplots segun el genero, pon otros labels.
## - Transformar el grafico anterior en uno de violin. 
## ========================================================

econAct010 <- filter(econAct, Year == 2010)

ggplot(econAct010) + 
  geom_boxplot(aes(x = Gender, y = Employ.serv))

ggplot(econAct010) + 
  geom_boxplot(aes(x = Gender, y = Employ.indu, fill = Gender))+
  labs(x = NULL) +
  facet_grid(. ~ Income)

ggplot(econAct010) + 
  geom_violin(aes(x = Gender, y = Employ.serv, scale = 'area', fill = Gender)) +
  labs(x = NULL) +
  facet_grid(. ~ Income)
# all violins have the same area (before trimming the tails)


## Ejercicio: 
## - Hacer un grafico de lineas de la variable Life.Exp (aes(y)) en funcion de Year (aes(x))
## - Mostrar el grafico anterior para cada nivel de la variable Region (Quitar la leyenda!)
## ========================================================

our_data5 <- filter(econAct, Gender == "F")
ggplot(our_data5) + 
  geom_line(aes(x = Year, y = Life.Exp, group = Country.iso3))

ggplot(our_data5) + 
  geom_line(aes(x = Year, y = Life.Exp, group = Country.iso3, color = Region), show.legend = FALSE) +
  facet_grid(. ~ Region)
##


## 7. Etiquetas, titulos, leyendas... ------------------


g <- ggplot(our_data3) +
  aes(x = factor(Year), y = Life.Exp, color = Region) +
  geom_point() +
  geom_line(aes(group = Country))

g + labs(x = "Year", 
         y = "Esperanza de vida", 
         title = "Nuestro gr??fico", 
         caption = "RLadiesBIO")


## 8. Coords -------------------

g + coord_flip()
g + coord_polar()


## 9. Scales -------------------

g + scale_color_manual(values = c("peru", "dodgerblue3", "plum", "firebrick3", "springgreen4"))
g + scale_y_log10()
g + scale_x_discrete(breaks = seq(2010, 2018, 3))

## Pregunta: ¿como resaltar (poner en cursiva/negrita) un unico elemento de la leyenda?
g + 
  scale_color_manual("REGION", values = 2:5,
                     labels = c("Asia del Este & Pacifico", 
                                "Europa & Asia Central",
                                "Latinoamerica & Caribe",
                                expression(bold("Africa subsahariana")))) + # importante: levels(factor(our_data3$Region))
  theme_minimal()


## 10. Temas -------------------

g + theme_bw() # black white
g + theme_dark()
g + theme_gray()
g + theme_light()
g + theme_minimal()

g + theme_minimal() + theme(text = element_text(family = "Palatino"))

library(ggthemes)
# Economist theme
g + theme_economist()


rladiesBIO_theme <- theme_bw() +
  theme(
    axis.text = element_text(colour = "white"),
    axis.text.x = element_text(angle = 90),
    text = element_text(family = "Palatino", size = 14),
    plot.background = element_rect(fill="#88398a", colour=NA),
    panel.border = element_rect(colour = 'grey80'), 
    panel.grid.minor = element_blank()
  )
theme_set(rladiesBIO_theme)

g
g + theme(legend.position = 'bottom')

## 11. Guardar graficos ---------------------

ggsave(
  filename = "my_plot.png",
  plot = my_plot,
  width = 10,
  height = 8,
  dpi = 100,
  device = "png"
)


## 12. Mapas ------------------------------
# http://rstudio-pubs-static.s3.amazonaws.com/2795_901030c4ef944c7797f39bcdac099d74.html
library(maptools)

data(wrld_simpl)
is(wrld_simpl)
head(wrld_simpl@data)
# fortify sirve con objetos maps, sp, etc
world_ggmap <- fortify(wrld_simpl, region = "ISO3")
head(world_ggmap)
ggplot(data = world_ggmap, aes(x = long, y = lat, group = group)) + geom_polygon()
ggplot(our_data) +
  geom_map(aes(map_id = Country.iso3, fill = Employ.agri), map = world_ggmap) +
  expand_limits(x = world_ggmap$long, y = world_ggmap$lat) +
  scale_fill_continuous(type = "viridis") +
  theme_minimal()


## 13. Graficos animados ------------------------------

library(gganimate)
library(gapminder)
g_hra <- ggplot(gapminder) +
  aes(x = gdpPercap, y = lifeExp, size = pop, color = country) +
  geom_point() +
  guides(color = FALSE, size = FALSE) +
  scale_x_log10(
    breaks = c(10^3, 10^4, 10^5),
    labels = c("1k", "10k", "100k")) +
  scale_color_manual(values = gapminder::country_colors) +
  scale_size(range = c(0.5, 12)) +
  labs(
    x = "GDP per capita",
    y = "Life Expectancy") +
  theme_minimal(18, base_family = "Fira Sans") +
  theme(
    plot.background = element_rect("#FAFAFA", color = NA),
    strip.text = element_text(size = 16, face = "bold"),
    panel.border = element_rect(fill = NA, color = "grey40"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  transition_states(
    year, 1, 0
  ) +
  ggtitle("{closest_state}")

# g_hra


## 14. Minard -----------------------------------------

library(HistData)
library(gridExtra)

data("Minard.troops")
head(Minard.troops)

plot_troops <- ggplot(Minard.troops, aes(long, lat)) +
  geom_path(aes(size = survivors, color = direction, group = group))

plot_both <- plot_troops + 
  geom_text(aes(label = city), size = 4, data = Minard.cities)

plot_polished <- plot_both +
  scale_size(range = c(0, 12),
             breaks = c(10000, 20000, 30000),
             labels = c("10,000", "20,000", "30,000")) + 
  scale_color_manual(values = c("tan", "grey50")) +
  labs(title = "Map of Napoleon's Russian campaign of 1812",
       x = NULL,
       y = NULL)

grid.arrange(plot_troops, plot_both, plot_polished, nrow=3, ncol=1, widths=12)


## 15. Plotly ------------------------------

library(plotly)
ggplotly(g)

# http://www.ggplot2-exts.org/gallery/
