
#SORTEO: AVENTURATE

mod2.aventurate <- lm(Venta_Estado ~ Pib_aven, data = df.aventurate)
summary(mod2.aventurate)

mod3.aventurate <- lm(Venta_Estado ~ Pib_aven + I(Pib_aven^2), data = df.aventurate)
summary(mod3.aventurate)

    #Aplicamos logaritmos

modlog1.aventurate <- lm(log(Venta_Estado) ~ log(Pib_aven), data = df.aventurate)
summary(modlog1.aventurate)


#SORTEO: EDUCATIVO

mod2.educativo <- lm(Venta_Estado ~ Pib_educ, data = df.educativo)
summary(mod2.educativo)

mod3.educativo <- lm(Venta_Estado ~ Pib_educ + I(Pib_educ^2), data = df.educativo)
summary(mod3.educativo)

    #Aplicamos logaritmos
modlog1.educativo <- lm(log(Venta_Estado) ~ log(Pib_educ), data = df.educativo)
summary(modlog1.educativo)


#SORTEO: MI SUENO

mod2.sueno <- lm(Venta_Estado ~ Pib_suen , data = df.sueno)
summary(mod2.sueno)

mod3.sueno <- lm(Venta_Estado ~ Pib_suen + I(Pib_suen^2), data = df.sueno)
summary(mod3.sueno)

    #Aplicamos logaritmos 

modlog1.sueno <- lm(log(Venta_Estado) ~ log(Pib_suen) , data = df.sueno)
summary(modlog1.sueno)


#SORTEO: TRADICIONAL

mod2.tradicional <- lm(Venta_Estado ~ Pib_trad, data = df.tradicional)
summary(mod2.tradicional)

mod3.tradicional <- lm(Venta_Estado ~ Pib_trad + I(Pib_trad^2), data = df.tradicional)
summary(mod3.tradicional)

    #Aplicamos logaritmos 
modlog1.tradicional <- lm(log(Venta_Estado) ~ log(Pib_trad), data = df.tradicional)
summary(modlog1.tradicional)

# MODELO PARA SORTEOS AGREGADOS

mod2.global <- lm(Venta_Estado ~ Pib, data = df.global)
summary(mod2.global)

mod3.global <- lm(Venta_Estado ~ Pib + I(Pib^2), data = df.global)
summary(mod3.global)

    #Aplicamos logaritmos
modlog1.global <- lm(log(Venta_Estado) ~ log(Pib), data = df.global)
summary(modlog1.global)


# Resumen de los modelos --------------------------------------------------

stargazer::stargazer(mod2.aventurate, mod2.educativo, mod2.sueno, mod2.tradicional , type = "text",
                     covariate.labels=c("Pib","Pib",
                                        "Pib","Pib"),
                     out="Output/modelsPibGen.txt")

stargazer::stargazer(mod3.aventurate, mod3.educativo, mod3.sueno, mod3.tradicional, type = "text",
                     covariate.labels = c("Pib","Pib2","Pib","Pib2",
                                          "Pib","Pib2","Pib","Pib2"),
                     out = "Output/models1PibGen.txt")

stargazer::stargazer(modlog1.aventurate,modlog1.educativo, modlog1.sueno, modlog1.tradicional, type = "text", 
                     covariate.labels=c("log Pib","log Pib","log Pib", "log Pib"),
                     out = "Output/logmodels1.txt")
