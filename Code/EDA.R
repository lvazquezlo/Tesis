options(scipen=999) 
# paquetes ----------------------------------------------------------------

if (!require("DataExplorer")) install.packages("DataExplorer")
library(DataExplorer)
if (!require("ggcorrplot")) install.packages("ggcorrplot")
library(ggcorrplot)
if (!require("patchwork")) install.packages("patchwork")
library(patchwork)


# Analisis Exploratorio de Datos ------------------------------------------

df.aventurate$ID <- factor(df.aventurate$ID)
df.educativo$ID <- factor(df.educativo$ID)
df.sueno$ID <- factor(df.sueno$ID)
df.tradicional$ID <- factor(df.tradicional$ID)
df.global$ID <- factor(df.global$ID)

#Boxplots
theme_set(theme_bw())
p.aventurate <- ggplot(data = df.aventurate, aes(x = ID, y = log(Venta_Estado))) + geom_boxplot() + 
  geom_dotplot(binaxis='y',
               stackdir='center',
               dotsize = .18,
               fill="red") + 
  labs(title = "Boletos vendidos por Estado",
       subtitle = "Sorteo: Aventurate",
       x = "Estado",
       y = "ln(# Boletos)")
p.aventurate

p.educ <- ggplot(data = df.educativo, aes(x = ID, y = log(Venta_Estado))) + geom_boxplot() + 
  geom_dotplot(binaxis='y',
               stackdir='center',
               dotsize = .18,
               fill="red") + 
  labs(
       subtitle = "Sorteo: Educativo",
       x = "Estado",
       y = "ln(# Boletos)")
p.educ

p.sueno <- ggplot(data = df.sueno, aes(x = ID, y = log(Venta_Estado))) + geom_boxplot() + 
  geom_dotplot(binaxis='y',
               stackdir='center',
               dotsize = .18,
               fill="red") + 
  labs(
       subtitle = "Sorteo: Mi Sueño",
       x = "Estado",
       y = "ln(# Boletos)")

p.sueno

p.trad <- ggplot(data = df.tradicional, aes(x = ID, y = log(Venta_Estado))) + geom_boxplot() + 
  geom_dotplot(binaxis='y',
               stackdir='center',
               dotsize = .18,
               fill="red") + 
  labs(
       subtitle = "Sorteo: Tradicional",
       x = "Estado",
       y = "ln(# Boletos)")

p.trad

p.global <- ggplot(data = df.global, aes(x = ID, y = log(Venta_Estado))) + geom_boxplot() + 
  geom_dotplot(binaxis='y',
               stackdir='center',
               dotsize = .18,
               fill="red") + 
  labs(title = "Boletos vendidos por Estado",
       subtitle = "Todos los sorteos",
       x = "Estado",
       y = "ln(# Boletos)")
p.global

(p.aventurate/p.educ)
(p.sueno/p.trad)

#Grafica para ver si hay relacion
p1.aventurate <- ggplot(data = df.aventurate, aes(x = log(Pib_perCap_aven), y = log(Venta_Estado))) +
  geom_point() + geom_smooth(method = "lm", se = F) +
  labs(title = "Boletos Vendidos vs PIB per capita",
       subtitle = "Sorteo: Aventurate",
       y = "ln(# Boletos)",
       x = "ln(PIB per capita)")
p1.aventurate

p1.educ <- ggplot(data = df.educativo, aes(x = log(Pib_perCap_educ), y = log(Venta_Estado))) +
  geom_point() + geom_smooth(method = "lm", se = F) +
  labs(
       subtitle = "Sorteo: Educativo",
       y = "ln(# Boletos)",
       x = "ln(PIB per capita)")
p1.educ

p1.sueno <- ggplot(data = df.sueno, aes(x = log(Pib_perCap_suen), y = log(Venta_Estado))) +
  geom_point() + geom_smooth(method = "lm", se = F) +
  labs(
       subtitle = "Sorteo: Mi Sueño",
       y = "ln(# Boletos)",
       x = "ln(PIB per capita)")
p1.sueno

p1.trad <- ggplot(data = df.tradicional, aes(x = log(Pib_perCap_trad), y = log(Venta_Estado))) +
  geom_point() + geom_smooth(method = "lm", se = F) +
  labs(
       subtitle = "Sorteo: Tradicional",
       y = "ln(# Boletos)",
       x = "ln(PIB per capita)")
p1.trad

p1.glob <- ggplot(data = df.global, aes(x = log(Pib_perCap), y = log(Venta_Estado))) +
  geom_point() + geom_smooth(method = "lm", se = F) +
  labs(title = "Boletos Vendidos vs PIB per capita",
       subtitle = "Todos los sorteos agregados",
       y = "ln(# Boletos)",
       x = "ln(PIB per capita)")
p1.glob

(p1.aventurate/p1.educ)
(p1.sueno/p1.trad)

p.pibPc <- ggplot(data = df.global, aes(x = ID, y = log(Pib_perCap))) + geom_boxplot() +
  labs(title = "PIB per capita por Estado",
       y = "ln(PIB pc)",
       x = "Estado")
p.pibPc

p.pib <- ggplot(data = df.global, aes(x = ID, y = log(Pib))) + geom_boxplot() +
  labs(title = "PIB por Estado",
       y = "ln(PIB)",
       x = "Estado")

p.pib


