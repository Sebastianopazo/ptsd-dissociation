####3: Mediational Models###################################################################

####### Proyect II: Test 4 mediational models:
#II.1: TQ_t0 (traumatic load) -> PDEQ_t0 (dissociation) -> PCL_t1 (PTSD 1 month after)  
#II.2: TQ_t0 (traumatic load) -> PDEQ_t0 (dissociation) -> BDI_t1 (depression 1 month after)
#II.3: TQ_t0 (traumatic load) -> PDI_t0 (peritraumatic distress) -> PCL_t1 (PTSD 1 month after)  
#II.4: TQ_t0 (traumatic load) -> PDI_t0 (peritraumatic distress) -> BDI_t1 (depression 1 month after)
# For each model also test direct path from TQ to outcome 1 month after

## Regresions only with interest variables 
#All are significant

#Dissociation predicts PTSD
a1 <- lm(formula = PCL_t1 ~ PDEQ_t0, data = my.data)
a1
summary(a1)

#Dissociation predicts depression
a2 <- lm(formula = BDI_t1 ~ PDEQ_t0, data = my.data)
a2
summary(a2)

#Peritraumatic distress predicts PTSD
a3 <- lm(formula = PCL_t1 ~ PDI_t0, data = my.data)
a3
summary(a3)

#Peritraumatic distress predicts depression
a4 <- lm(formula = BDI_t1 ~ PDI_t0, data = my.data)
a4
summary(a4)

#Traumatic load predicts dissociation 
a5 <- lm(formula = PDEQ_t0 ~ TQ_t0, data = my.data)
a5
summary(a5)

#Traumatic load predicts peritraumatic distress 
a6 <- lm(formula = PDI_t0 ~ TQ_t0, data = my.data)
a6
summary(a6)


## Multiple regressions adding control variables: 
#Edad
#Sexo*: sex  
#CIDI_A8_basal: years of education
#MSPSS_t0: percived social support   
#rama*: intervention (PAP; Psicoeducation)- only when predicting T1 variable  
#All regressions are significant

#Dissociation predicts PTSD
a11 <- lm(formula = PCL_t1 ~ PDEQ_t0 + Edad + Sexo + CIDI_A8_basal + rama + MSPSS_t0, data = my.data)
a11
summary(a11)

#Dissociation predicts depression
a12 <- lm(formula = BDI_t1 ~ PDEQ_t0 + Edad + Sexo + CIDI_A8_basal + rama + MSPSS_t0, data = my.data)
a12
summary(a12)

#Peritraumatic distress predicts PTSD
a13 <- lm(formula = PCL_t1 ~ PDI_t0 + Edad + Sexo + CIDI_A8_basal + rama + MSPSS_t0, data = my.data)
a13
summary(a13)

#Peritraumatic distress predicts depression
a14 <- lm(formula = BDI_t1 ~ PDI_t0 + Edad + Sexo + CIDI_A8_basal + rama + MSPSS_t0, data = my.data)
a14
summary(a14)

#Traumatic load predicts dissociation 
a15 <- lm(formula = PDEQ_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + MSPSS_t0, data = my.data)
a15
summary(a15)

#Traumatic load predicts peritraumatic distress 
a16 <- lm(formula = PDI_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + MSPSS_t0, data = my.data)
a16
summary(a16)



#II.1: TQ_t0 (traumatic load) -> PDEQ_t0 (dissociation) -> PCL_t1 (PTSD 1 month after) 

##Following advice of P Cumsille on 161125 and using program "mediation"
library(mediation) #Cada vez que se inicia una sesión para cargar el paquete

med.fit<-lm(PDEQ_t0~TQ_t0)   #med.fit<-lm(M ~X) - Ecuación de predicción del mediador (M) por la variables independiente (X)

out.fit<-glm(PCL_t1~TQ_t0+PDEQ_t0) #out.fit<-glm(D~ X+M) - Ecuación de predicción de la variable dependiente (D) por la variables independiente (X) y mediadora

med.out<-mediate(med.fit,out.fit, treat="X", sims=10000, mediator="M", robustSE=TRUE) #Para obtener los coeficientes de medicación con con significación estimada con 10000 bootstraps

summary(med.out) #Para ver el output con los coeficientes








