library(tidyverse)
library(stargazer)
library(ggfortify)
library(lmtest)
library(MASS)

setwd("~/Documents/itam/Tesis")


# Modelo ------------------------------------------------------------------

# SORTEO: AVENTURATE

mod0.aventurate <- lm(Venta_Estado ~ Pib_perCap_aven, data = df.aventurate)
summary(mod0.aventurate)

mod1.aventurate <- lm(Venta_Estado ~ Pib_perCap_aven + I(Pib_perCap_aven^2), data = df.aventurate)
summary(mod1.aventurate)


    # Aplicamos logaritmo
modlog0.aventurate <- lm(log(Venta_Estado) ~ log(Pib_perCap_aven), data = df.aventurate)
autoplot(modlog0.aventurate)

# SORTEO: EDUCATIVO

mod0.educativo <- lm(Venta_Estado ~ Pib_perCap_educ, data = df.educativo)
summary(mod0.educativo)

mod1.educativo <- lm(Venta_Estado ~ Pib_perCap_educ + I(Pib_perCap_educ^2), data = df.educativo)
summary(mod1.educativo)

    #Aplicamos logaritmos

modlog0.educativo <- lm(log(Venta_Estado) ~ log(Pib_perCap_educ), data = df.educativo)
summary(modlog0.educativo)
autoplot(modlog0.educativo)

# SORTEO: MI SUENO

mod0.sueno <- lm(Venta_Estado ~ Pib_perCap_suen , data = df.sueno)
summary(mod0.sueno)

mod1.sueno <- lm(Venta_Estado ~ Pib_perCap_suen + I(Pib_perCap_suen^2) , data = df.sueno)
summary(mod1.sueno)


    #Aplicamos logaritmos

modlog0.sueno <- lm(log(Venta_Estado) ~ log(Pib_perCap_suen), data = df.sueno)
summary(modlog0.sueno)
autoplot(modlog0.sueno)


# SORTEO: TRADICIONAL

mod0.tradicional <- lm(Venta_Estado ~ Pib_perCap_trad, data = df.tradicional)
summary(mod0.tradicional)

mod1.tradicional <- lm(Venta_Estado ~ Pib_perCap_trad + I(Pib_perCap_trad^2), data = df.tradicional)
summary(mod1.tradicional)


    #Aplicamos logaritmos
modlog0.tradicional <- lm(log(Venta_Estado) ~ log(Pib_perCap_trad), data = df.tradicional)
summary(modlog0.tradicional)
autoplot(modlog0.tradicional)


# MODELO PARA SORTEOS AGREGADOS

mod0.global <- lm(Venta_Estado ~ Pib_perCap, data = df.global)
summary(mod0.global)

mod1.global <- lm(Venta_Estado ~ Pib_perCap + I(Pib_perCap^2), data = df.global)
summary(mod1.global)

    #Aplicamos logaritmos
modlog0.global <- lm(log(Venta_Estado) ~ log(Pib_perCap), data = df.global)
summary(modlog0.global)
autoplot(modlog0.global)


#summary(lm(Venta_Estado ~ Pib_perCap_trad + I(Pib_perCap_trad^2), data = df.tradicional))

#Resumen de las regresiones 

stargazer::stargazer(mod0.aventurate,mod0.educativo,
                     mod0.sueno, mod0.tradicional, type = "text",
                     covariate.labels=c("Pib per capita","Pib per capita",
                                        "Pib per capita","Pib per capita"),
                     out="Output/models.txt")

stargazer::stargazer(mod0.aventurate,mod0.educativo,
                     mod0.sueno, mod0.tradicional, type = "latex",
                     covariate.labels=c("Pib per capita","Pib per capita",
                                        "Pib per capita","Pib per capita"))






stargazer::stargazer(mod1.aventurate, mod1.educativo, type = "latex",
                     covariate.labels = c("Pib per capita","Pib per cap2","Pib per capita","Pib per cap2"))

stargazer::stargazer(mod1.sueno, mod1.tradicional, type = "latex",
                     covariate.labels = c("Pib per capita","Pib per cap2","Pib per capita","Pib per cap2"))




stargazer::stargazer(modlog0.aventurate,modlog0.educativo, modlog0.sueno, modlog0.tradicional, type = "text", 
                     covariate.labels=c("log Pib per capita","log Pib per capita","log Pib per capita"),
                     out = "Output/logmodels.txt")

stargazer::stargazer(modlog0.aventurate,modlog0.educativo, type = "latex", 
                     covariate.labels=c("log Pib per capita","log Pib per capita"))

stargazer::stargazer(modlog0.sueno, modlog0.tradicional, type = "latex", 
                     covariate.labels=c("log Pib per capita","log Pib per capita"))

stargazer::stargazer(mod0.global, mod1.global, mod2.global, modlog0.global, type = "text",
                     out = "Output/modelAg.txt")


autoplot(mod1.educativo)
boxcox(mod1.aventurate)
