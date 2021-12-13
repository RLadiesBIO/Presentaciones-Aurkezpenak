
#Directorio de trabajo

setwd("E:/UPVEHU/2021/2021_09_29_R_Ladies/Taller/")

#Librerias necesarias instalar
#install.packages("SailoR")#Sailor
#install.packages("plotrix") #Taylor
#install.packages("openair") #Taylor
#install.packages("hydroGOF") #RMSE


#Cargar librerias 
library("SailoR") 
#library("plotrix") #Taylor
library("openair") #Taylor
library("hydroGOF") #RMSE


#Diagrama de Taylor
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cargar datos de velocidad de viento observados
# y de 4 modelos diferentes
load("data_vel.RData")

#Clase
class(data_vel)

#Forma de los datos
head(data_vel)

#Resumen de los datos
summary(data_vel)

#Diagrama de Taylor 
TaylorDiagram(data_vel, obs = "obs_value", 
              mod = "mod_value",group="mod_name",
              main="Modulo de viento",
              key.title="Modelos",
              xlim=c(-0.1,10),
              ylim=c(-0.1,10))

#Para guardar el gráfico
jpeg("Taylor.jpg")
  TaylorDiagram(data_vel, obs = "obs_value", 
                mod = "mod_value",group="mod_name",
                main="Modulo de viento",
                key.title="Modelos",
                xlim=c(-0.1,10),
                ylim=c(-0.1,10))
dev.off()

#Estadisticos de la referencia y el modelo1
n1<-which(data_vel$mod_name=="MOD1")
OBS1<-data_vel[n1,"obs_value"]
MOD1<-data_vel[n1,"mod_value"]

#Desviación Estandar
sd(OBS1)
sd(MOD1)

#Correlación
cor(OBS1,MOD1)

#RMSE
rmse(OBS1,MOD1)

obs_mean<-mean(OBS1)
mod_mean<-mean(MOD1)
rmse(OBS1-obs_mean,MOD1-mod_mean)


#Taylor U y V
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("data_U.RData")
load("data_V.RData")

#Clase
class(data_U)
class(data_V)

#Nombres
names(data_U)
names(data_V)

#Taylor U
TaylorDiagram(data_U, obs = "obs_value", 
              mod = "mod_value",group="mod_name",
              main="Componente U de viento",
              key.title="Modelos",
              xlim=c(-0.1,10),
              ylim=c(-0.1,10))


#png
png("Taylor_U.png")
  TaylorDiagram(data_U, obs = "obs_value", 
              mod = "mod_value",group="mod_name",
              main="Componente U de viento",
              key.title="Modelos",
              xlim=c(-0.1,10),
              ylim=c(-0.1,10))
dev.off()


#Taylor V
TaylorDiagram(data_V, obs = "obs_value", 
              mod = "mod_value",group="mod_name",
              main="Componente V de viento",
              key.title="Modelos",
              xlim=c(-0.1,16),
              ylim=c(-0.1,16))

#png
png("Taylor_V.png")
TaylorDiagram(data_V, obs = "obs_value", 
              mod = "mod_value",group="mod_name",
              main="Componente V de viento",
              key.title="Modelos",
              xlim=c(-0.1,16),
              ylim=c(-0.1,16))
dev.off()


#Diagramas de Taylor U y V contradictorios
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load("data_AB_U.RData")
load("data_AB_V.RData")

#Class
class(data_AB_U)
class(data_AB_V)

#Names
names(data_AB_U)
names(data_AB_V)


#Taylor U
TaylorDiagram(data_AB_U, obs = "obs_value", 
              mod = "mod_value",group="mod_name",
              main="Componente U de viento",
              key.title="Modelos",
              cols=c("orange","grey50"))


#png
png("Taylor_AB_U.png")
TaylorDiagram(data_AB_U, obs = "obs_value", 
              mod = "mod_value",group="mod_name",
              main="Componente U de viento",
              key.title="Modelos",
              cols=c("orange","grey50"))
dev.off()

#Taylor V
TaylorDiagram(data_AB_V, obs = "obs_value", 
              mod = "mod_value",group="mod_name",
              main="Componente V de viento",
              key.title="Modelos",
              cols=c("orange","grey50"))

#png
png("Taylor_AB_V.png")
TaylorDiagram(data_AB_V, obs = "obs_value", 
              mod = "mod_value",group="mod_name",
              main="Componente V de viento",
              key.title="Modelos",
              cols=c("orange","grey50"))
dev.off()


#SailoR
#~~~~~~~~~~~~~~~~~~~~~~~~
#Diagrama U y V contradictorios
#Modelos en Synthetic$mod
load("Synthetic_AB.RData")
names(Synthetic_AB)

#Clase
class(Synthetic_AB$ref)
class(Synthetic_AB$mod)

#Nombres
names(Synthetic_AB$ref)
names(Synthetic_AB$mod)

head(Synthetic_AB$ref)
#head(Synthetic_AB$mod)

#Dimensiones
dim(Synthetic_AB$ref)
dim(Synthetic_AB$mod)

#Centrado
p <- SailoR.Plot(Synthetic_AB$ref,
                 Synthetic_AB$mod,
                 ColourList=c("orange","grey50"),
                 sfactor=1,
                 docenter=TRUE,
                 xlim=c(-20,35),
                 ylim=c(-15,15),
                 xlab="Componente U",
                 ylab="Componente V",
                 plotmain="Viento MOD2A y MOD2B",
                 plotRMSElegend=FALSE,
                 plotscalelegend=TRUE,
                 RMSE_legend_Rounding=1,
                 RMSE_legend_units = " m/s",
                 referenceName="Reference")
png("Sailor_MOD2AB.png")
  p
dev.off()


#Sin centrar
p <- SailoR.Plot(Synthetic_AB$ref,
                 Synthetic_AB$mod,
                 ColourList=c("orange","grey50"),
                 sfactor=1,
                 docenter=FALSE,
                 xlim=c(-20,35),
                 ylim=c(-15,15),
                 xlab="Componente U",
                 ylab="Componente V",
                 plotmain="Viento MOD2A y MOD2B",
                 plotRMSElegend=FALSE,
                 plotscalelegend=TRUE,
                 RMSE_legend_Rounding=1,
                 RMSE_legend_units = " m/s",
                 referenceName="Reference")

png("Sailor_MOD2AB_SinCentrar.png")
p
dev.off()


#Indices
Indices <- SailoR.Indices(Synthetic_AB$ref,
                     Synthetic_AB$mod)

names(Indices$MOD2A)
sqrt(sum(Indices$MOD2A$meanV^2))
Indices$MOD2A$sdVx
Indices$MOD2A$sdVy
Indices$MOD2A$thetavu

sqrt(sum(Indices$MOD2B$meanV^2))
Indices$MOD2B$sdVx
Indices$MOD2B$sdVy
Indices$MOD2B$thetavu

#Tabla Indices
Tabla<-SailoR.Table(Indices,round_digits = 1)
Tabla

#Tabal Latex:
#library(xtable)
#oInfo<-xtable(Tabla)
#print(oInfo,type="latex",file="table.tex")


#Mas ejemplos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data(Synthetic)
names(Synthetic)

#Parametros
ref<-Synthetic$ref
mod<-Synthetic$mod
Uxlim=c(0,15)
Uylim=c(-25,10)
Uxlab<-"Ux (m/s)"
Uylab<-"Uy (m/s)"
plotmain<-"Reference and synthetic models"
sfactor<-1


p<-SailoR.Plot(ref,mod,ColourList=NULL, sfactor, docenter=TRUE,
            Uxlim, Uylim, Uxlab, Uylab, plotmain, plotlegend=TRUE,
            Ensembles=TRUE, plotRMSElegend=FALSE,
            plotscalelegend=TRUE, RMSE_legend_Rounding=2,
            RMSE_legend_units = " m/s")
p

png("Synthetic.png")
  p
dev.off()


