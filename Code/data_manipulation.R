
# Paquetes ----------------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)

library(readxl)

# Lectura bases de datos --------------------------------------------------
setwd("~/Documents/itam/Tesis")

# Manipulacion de datos ----------------------------------------------------------------------

#Sorteo: Aventurate

an <- 4:16
aventurate <- c()
for(i in 1:13){
  aventurate[i] <- paste("AVENTURATE", an[i], sep = " ")
}

#Para el sorteo aventurate entre cada sorteo hay una distancia de 4 meses

#Sorteo: Educativo

an <- 20:38
educativo <- c()
for(i in 1:19){
  educativo[i] <- paste("EDUCATIVO", an[i], sep = " ")
}

#Para el sorteo aventurare entre cada emision de sorteo hay una distancia de 3 meses

#Sorteo: Mi Sueno

an <- 3:16
sueno <- c()
for (i in 1:14){
  sueno[i] <- paste("MI SUENO", an[i], sep = " ")
}

#Para el sorteo mi sueno entre cada emision de sorteo hay una distancia de 3 meses

#Sorteo: Tradicional

an <- 200:208
tradicional <- c()
for(i in 1:9){
  tradicional[i] <- paste("TRADICIONAL", an[i], sep = " ")
}

#Para el sorteo mi sueno entre cada emision de sorteo hay una distancia de 3 meses

# Raw data ----------------------------------------------------------------

venta_hist <- read_excel("Data/datos.xlsx",sheet = "venta_hist")
v.hist <- venta_hist
colnames(v.hist) <- c("Estado", "Oficina", "Sorteo", "Total_Ventas", "Fecha")
as.tibble(v.hist)
head(v.hist)

estados <- read_excel("Data/datos.xlsx", sheet = "Estados")


#SORTEO: AVENTURATE
df.aventurate <- v.hist %>% 
  filter(Sorteo %in% aventurate) %>% 
  select(Estado, Sorteo, Total_Ventas, Fecha) %>% 
  group_by(Sorteo, Estado) %>% 
  mutate(Venta_Estado = sum(Total_Ventas)) %>% 
  distinct(Estado, .keep_all = TRUE) %>% 
  select(Estado, Sorteo, Fecha, Venta_Estado) %>% 
  left_join(estados) %>% 
  arrange(Fecha, ID)

write_excel_csv(df.aventurate, path = "Output/df_aventurate.csv")

pib_percap_aventurate <- read_excel("Data/datos.xlsx", sheet = "pib_percap_aven")

df.aventurate <- df.aventurate %>% 
  left_join(pib_percap_aventurate, by = c("Fecha", "ID", "Estado")) %>% 
  filter(Venta_Estado != 0)
  
df.aventurate <- df.aventurate[-(280:391),]  



View(df.aventurate)

#SORTEO: EDUCATIVO

pib_percap_educ <- read_excel("Data/datos.xlsx", sheet = "pib_percap_educ")

df.educativo <- v.hist %>% 
  filter(Sorteo %in% educativo) %>% 
  select(Estado, Sorteo, Total_Ventas, Fecha) %>% 
  group_by(Sorteo, Estado) %>% 
  mutate(Venta_Estado = sum(Total_Ventas)) %>% 
  distinct(Estado, .keep_all = TRUE) %>% 
  select(Estado, Sorteo, Fecha, Venta_Estado) %>% 
  left_join(estados) %>% 
  arrange(Fecha, ID) %>% 
  left_join(pib_percap_educ, by = c("Fecha", "ID", "Estado")) %>% #Para poner este left_join tuve que exportar la tabla y acomodar 
  filter(Venta_Estado != 0)

View(df.educativo)

df.educativo <- df.educativo[-(435:574),]

#df.educativo <- write_excel_csv(df.educativo, path = "Output/df_educativo.csv")

#SORTEO: MI SUENO

df.sueno <- v.hist %>% 
  filter(Sorteo %in% sueno) %>% 
  select(Estado, Sorteo, Total_Ventas, Fecha) %>% 
  group_by(Sorteo, Estado) %>% 
  mutate(Venta_Estado = sum(Total_Ventas)) %>% 
  distinct(Estado, .keep_all = TRUE) %>% 
  select(Estado, Sorteo, Fecha, Venta_Estado) %>% 
  left_join(estados) %>% 
  arrange(Fecha, ID)

write_excel_csv(df.sueno, path = "Output/df_sueno.csv")

pib_percap_sueno <- read_excel("Data/datos.xlsx", sheet = "pib_percap_suen")

df.sueno <- df.sueno %>% 
  left_join(pib_percap_sueno, by = c("Fecha", "ID", "Estado")) %>% 
  filter(Venta_Estado != 0)

df.sueno <- df.sueno[-(341:424),]

View(df.sueno)

#SORTEO: TRADICIONAL

df.tradicional <- v.hist %>% 
  filter(Sorteo %in% tradicional) %>% 
  select(Estado, Sorteo, Total_Ventas, Fecha) %>% 
  group_by(Sorteo, Estado) %>% 
  mutate(Venta_Estado = sum(Total_Ventas)) %>% 
  distinct(Estado, .keep_all = TRUE) %>% 
  select(Estado, Sorteo, Fecha, Venta_Estado) %>% 
  left_join(estados) %>% 
  arrange(Fecha, ID)

write_excel_csv(df.tradicional, path = "Output/df_tradicional.csv")

pib_percap_trad <- read_excel("Data/datos.xlsx", sheet = "pib_percap_trad")

df.tradicional <- df.tradicional %>% 
  left_join(pib_percap_trad, by = c("Fecha", "ID", "Estado")) %>% 
  filter(Venta_Estado != 0)

df.tradicional <- df.tradicional[-(215:270),]

View(df.tradicional)

#AGREGADO: TODOS LOS SORTEOS

df.global <- rbind(df.aventurate, df.educativo, df.sueno, df.tradicional)

length(df.aventurate$Estado)
length(df.educativo$Estado)
length(df.sueno$Estado)
length(df.tradicional$Estado)


df.global$Pib_perCap_aven[280:713] <- df.global$Pib_perCap_educ[280:713]
df.global$Pib_perCap_aven[714:1053] <- df.global$Pib_perCap_suen[714:1053]
df.global$Pib_perCap_aven[1054:1267] <- df.global$Pib_perCap_trad[1054:1267]

df.global$Pib_aven[280:713] <- df.global$Pib_educ[280:713]
df.global$Pib_aven[714:1053] <- df.global$Pib_suen[714:1053]
df.global$Pib_aven[1054:1267] <- df.global$Pib_trad[1054:1267]

df.global <- df.global %>% 
  select(Estado, Sorteo, Fecha, Venta_Estado, ID, Pib_perCap_aven, Pib_aven)

colnames(df.global) <- c("Estado", "Sorteo", "Fecha", "Venta_Estado", "ID", "Pib_perCap", "Pib")

df.global <- df.global %>% 
  arrange(Fecha, ID) %>% 
  filter(Pib_perCap != 0)

View(df.global)

