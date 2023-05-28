###### NOTES ####
  
# This syntax was created by Paula Errazuriz starting on Sept 2016 to work with the data of 
  #Rodrigo Figueroa. Also worked with Paula Cortes from CIGIDEN. Received statistical help from Alejandro, 
  #P Cumsille, and E Krommuler
# Original data file was sent by Paula Cortes on 160516. I saved it as: pacientes_excell p cortes 160516

## Before importing file to R
# Prepared excell to import into R: erased simbol after N and erased $;
# Changed "perdido" por NA
# Changed #NULL! for NA
# Changed 999 for NA
# Changed " " (empty cell) for NA
# Transformed excell into CSV
# Continued preparind data in CSV
# Erased values nombre_pac to preserve confidentiality of subjets
# Since I could not fix the outlier (82L) in R, I changed it the CSV. Change the value 16
  #to 11, because the highest value in TQ_to was 10. Also for education (CIDI_A8_basal) change value of (165AL) 
  #from 175 to 18, and of (191AL) from 145 to 15, because I assumed they were decimal.
# Saved as file 161103
# On 170124 continued cleaning file. For variable tipo_trauma changed NA for space. Also erased column 
  #"nombre_pac"

###### CODE BOOK ####
### Variables of interest (selected for theoretical reasons) 

## Numeric variables
# Nreclutado col 1
# Edad - col 4
# CIDI_A8_basal: years of education - col 38
# BDI_t0: Depression t0 (symptoms last 7 days) - col 16         
# BDI_t1: Depression t1 (symptoms last 7 days) - col 17     
# PCL_t0: PTSD score t0 (symptoms last month; inespecific, not realated to 1 event) - col 18            
# PCL_t1: PTSD score t1 (symptoms last month; inespecific, not realated to 1 event) - col 19       
# PDEQ_t0: dissociation during last traumatic event - col 21           
# PDI_t0: peritraumatic distress during the last traumatic event col 22
# TQ_t0: traumatic load (number of traumatic events) - col 23   
# MSPSS_t0: percived social support - col 20 

## Factor variables
# Sexo*: sex - col 5          
# tipo_trauma*: trauma type - col 6      
# rama*: intervention (PAP; Psicoeducation) - col 8              
# CIDI_K_t1*: presence of PTSD at t1? (TEPT- TEPT+) - col 15

###### INSTALL AND CALL PACKAGES #####
  
## Install packages to work on this project (not sure if all are needed)
  #call last the packages that will need most to deal with "masking" 
#install.packages("psych")
#install.packages("MASS")
#install.packages("Matrix")
#install.packages("mvtnorm")
#install.packages("sandwich")
#install.packages("chron")
#install.packages("Rcpp")
#install.packages("stringi")
#install.packages("data.table")
#install.packages("mediation")

library(psych)
library(MASS)
#library(Matrix)
#library(mvtnorm)
#library(sandwich)
#library(chron)
#library(Rcpp)
#library(stringi)
#library(data.table)
library(mediation)

###### READ DATA (run always) ####

## Set work directory
#getwd()

# PC laptop Dell
#setwd("C:/Users/Paula Errazuriz/Dropbox/1 PROYECTOS_INVESTIGACION/desastres_naturales/paper factores riesgo PTSD Rodrigo")
# MacBook Air
setwd("/Users/Paula/Dropbox/1 PROYECTOS_INVESTIGACION/desastres_naturales/paper factores riesgo PTSD Rodrigo")





# Open Data :)
mydata <- read.csv("161103 pacientes_excell_para_r.csv",
                                      header = TRUE, sep = ";")

#####no estoy ocupando la ultima version por lo que explico a continuacion




######## Ahora me lee todo como una variable. La ultima vez guarde de excel a csv en PC y me las le??a todas.
# Ver tb con el Ale cuales de los pasos son fundamentales para leer los datos y pegarlos en esta parte y no la
# siguiente








# Convert character variables to factors 
read.table

###### UNDERSTANDING DATA ####

str(mydata) # Summary of data
head(mydata,1) #See first row of data

# Describe all variables (n, mean, sd, median, trimmed (mean discarding high and low values))
library(psych)
describe(mydata)

# List factor levels for factor variables and frecuency of values
table(mydata$Sexo)
table(mydata$tipo_trauma)
table(mydata$rama)
table(mydata$CIDI_K_t1)

# Describe each numeric Variable (mean, sd, median, trimmed, mad, min, max, range,
  #skew, kurtosis (posible outliers if greater than 3), se)
describe(mydata$Edad)#age
describe(mydata$BDI_t0)#Depression t0
describe(mydata$BDI_t1)#Depression t1
describe(mydata$PCL_t0)#PTSD score t0
describe(mydata$PCL_t1)# PTSD score t1
describe(mydata$CIDI_A8_basal)#years of education
describe(mydata$MSPSS_t0)#percived social support?
describe(mydata$PDEQ_t0)#dissociation after trauma score
describe(mydata$PDI_t0)#traumatic stress after trauma
describe(mydata$TQ_t0)#trauma history (number of traumatic events?)

# other descriptives
describe(mydata, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1,
         type=3,check=TRUE,fast=NULL,quant=NULL,IQR=FALSE)
describeData(mydata,head=4,tail=4)
describeBy(mydata,mydata$Sexo,skew=FALSE,ranges=FALSE)

# Plot 1 variable
plot(mydata$Edad)#age
plot(mydata$BDI_t0)#Depression t0  
plot(mydata$PCL_t0)#PTSD score t0  
plot(mydata$BDI_t1)#Depression t1 
plot(mydata$PCL_t1)# PTSD score t1  
plot(mydata$CIDI_A8_basal)#years of education
plot(mydata$MSPSS_t0)#percived social support?
plot(mydata$PDEQ_t0)#dissociation after trauma score  
plot(mydata$PDI_t0)#traumatic stress after trauma  
plot(mydata$TQ_t0)#trauma history (number of traumatic events?)  

# Histogram
hist(mydata$Edad)#age
hist(mydata$BDI_t0)#Depression t0  
hist(mydata$PCL_t0)#PTSD score t0  
hist(mydata$BDI_t1)#Depression t1 
hist(mydata$PCL_t1)# PTSD score t1  
hist(mydata$CIDI_A8_basal)#years of education
hist(mydata$MSPSS_t0)#percived social support?
hist(mydata$PDEQ_t0)#dissociation after trauma score  
hist(mydata$PDI_t0)#traumatic stress after trauma  
hist(mydata$TQ_t0)#trauma history (number of traumatic events?) 

# Plot 2 varibles (each independent variable with the dependent variable 
  #(TQ_t0  trauma history (number of traumatic events?)))
plot(mydata$TQ_t0,mydata$Edad)#age
plot(mydata$TQ_t0,mydata$BDI_t0)#Depression t0  
plot(mydata$TQ_t0,mydata$PCL_t0)#PTSD score t0  
plot(mydata$TQ_t0,mydata$BDI_t1)#Depression t1 
plot(mydata$TQ_t0,mydata$PCL_t1)# PTSD score t1  
plot(mydata$TQ_t0,mydata$CIDI_A8_basal)#years of education
plot(mydata$TQ_t0,mydata$MSPSS_t0)#percived social support?
plot(mydata$TQ_t0,mydata$PDEQ_t0)#dissociation after trauma score  
plot(mydata$TQ_t0,mydata$PDI_t0)#traumatic stress after trauma  

#Also plotted variables that could have high correlation
plot(mydata$Edad,mydata$CIDI_A8_basal)

###### SUBSET DATA ####

# Select code book variables & data from patients who have t0 data
mydata <- subset(mydata, BDI_t0 != is.na(1) | PCL_t0 != is.na(1),
select=c("Nreclutado","Sexo", "Edad", "CIDI_A8_basal",
                          "tipo_trauma", "PDEQ_t0", "PDI_t0", "TQ_t0",
                          "MSPSS_t0", "BDI_t0", "PCL_t0",  "BDI_t1", "PCL_t1", 
                          "CIDI_K_t1", "rama"))

# Select code book variables & data from patients who have t0 and t1 data
# Data for ptsd paper w Rodrigo 170124
mydata <- subset(mydata, TQ_t0 != is.na(1) & PDEQ_t0 != is.na(1) & PCL_t1 != is.na(1),    
                 select=c("Nreclutado","Sexo", "Edad", "CIDI_A8_basal",
                        "tipo_trauma", "PDEQ_t0", "PDI_t0", "TQ_t0",
                         "MSPSS_t0", "BDI_t0", "PCL_t0",  "BDI_t1", "PCL_t1", 
                         "CIDI_K_t1", "rama", "TQ1_t0", "TQ2_t0", "TQ3_t0", 
                        "TQ4_t0", "TQ5_t0", "TQ6_t0", "TQ7_t0", "TQ8_t0", 
                        "TQ9_t0", "TQ10_t0", "TQ11_t0", "TQ12_t0", "TQ13_t0", 
                        "TQ14_t0", "TQ15_t0", "TQ16_t0", "TQ17_t0", "TQ18_t0"))


###### PREPARING DATA (run always)!!!!!!!!!!!!!!!!!!!!!!ERROR ####
  #Detecto outlier 57L y no lo puedo cambiar####

###Univariate Outliers

# Deciding cut-off point of z=3.29 for outliers
outl <- 3.29

# Transforming to z-scores
ztemp <- scale(mydata$Edad, center=TRUE, scale = TRUE)

# Finding data for a numerical variable that IS an autlier 
ind.o <- which(abs(ztemp) > outl)

# Repeating last 2 steps for all other numerical variables
  #For variables with outliers, continue with steps to fix outliers
  #Need to add consecutive number after each variable created (ztemp and ind.o)
ztemp1 <- scale(mydata$BDI_t0, center=TRUE, scale = TRUE)
ind.o1 <- which(abs(ztemp1) > outl)
ztemp2 <- scale(mydata$PCL_t0, center=TRUE, scale = TRUE)
ind.o2 <- which(abs(ztemp2) > outl)
ztemp3 <- scale(mydata$PCL_t1, center=TRUE, scale = TRUE)
ind.o3 <- which(abs(ztemp3) > outl)
ztemp4 <- scale(mydata$CIDI_A8_basal, center=TRUE, scale = TRUE)
ind.o4 <- which(abs(ztemp4) > outl)
ztemp5 <- scale(mydata$MSPSS_t0, center=TRUE, scale = TRUE)
ind.o5 <- which(abs(ztemp5) > outl)
ztemp6 <- scale(mydata$BDI_t1, center=TRUE, scale = TRUE)
ind.o6 <- which(abs(ztemp6) > outl)
ztemp7 <- scale(mydata$PDEQ_t0, center=TRUE, scale = TRUE)
ind.o7 <- which(abs(ztemp7) > outl)
ztemp8 <- scale(mydata$PDI_t0, center=TRUE, scale = TRUE)
ind.o8 <- which(abs(ztemp8) > outl)
ztemp9 <- scale(mydata$TQ_t0, center=TRUE, scale = TRUE)
ind.o9 <- which(abs(ztemp9) > outl)

## For variables with outliers
# Finding data for each numerical variable that is NOT an autlier 
ind.no9 <- which(abs(ztemp9) <= outl)

# Detecting highest (or lowest) value before outlier
max.d <- max(mydata$TQ_t0[ind.no9])
min.d <- min(mydata$TQ_t0[ind.no9])

# Changing highest (or lowest) value before outlier
#!!!!!!!!!!!!!!Not working. Don?t know if it doesn`t make change or doesn?t save it
mydata$TQ_t0[which(mydata$TQ_t0 > max.d)] <- max.d+1 
mydata$TQ_t0[which(mydata$TQ_t0 < min.d)] <- min.d+1 

# Repeat outlier detection to confirm fixed outliers
ztemp10 <- scale(mydata$TQ_t0, center=TRUE, scale = TRUE)
ind.o10 <- which(abs(ztemp10) > outl)
ind.no10 <- which(abs(ztemp10) <= outl)

## For variables with outliers
# Finding data for each numerical variable that is NOT an autlier 
ind.no9 <- which(abs(ztemp9) <= outl)
ind.no9 <- which(abs(ztemp9) <= outl)

#Or change outliers by hand considering: 	Outliers, with z-scores larger than ? 3.29, 
    #will be transformed by replacing their raw score value with the next highest or 
    #lowest value as recommended by Tabachnick and Fidell (2001). 
  #reassign depth values under 10 to zero
  #df$depth[df$depth<10] <- 0

###### DATA ANALYSES ####

##### T-test ####
# run independent t-test to assess significant differences in categorical variables

# independent 2-group t-test
t.test(mydata$PDEQ_t0 ~ mydata$Sexo) # where y is numeric and x is a binary factor
t.test(mydata$PDEQ_t0 ~ mydata$rama) # where y is numeric and x is a binary factor
t.test(mydata$PCL_t1 ~ mydata$Sexo) # where y is numeric and x is a binary factor
t.test(mydata$PCL_t1 ~ mydata$rama) # where y is numeric and x is a binary factor


str(data1)
cor.test(as.matrix(data1),as.matrix(data1),
         alternative = c("two.sided"),
         method = c("pearson"),
         exact = NULL, conf.level = 0.95, continuity = FALSE)

### Correlation between PTSD symptoms and other variables

     
cor(mydata$PCL_t1, mydata$PDEQ_t0)



##### Multiple regressions ####
  # Traumatic load as predictor of mental health after a new traumatic event and after 1 month (time 1)

#### Multiple Regressions I
# In the case of dependent variables measured at time 1 will control for intervention (rama)

# Depression t0
r1 <- lm(formula = BDI_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal, data = mydata)
r1
summary(r1)

# Depression t1
r2 <- lm(formula = BDI_t1 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + rama, data = mydata)
r2
summary(r2)

# PTSD score t0 
r3 <- lm(formula = PCL_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal, data = mydata)
r3
summary(r3)

# PTSD score t1   
r4 <- lm(formula = PCL_t1 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + rama, data = mydata)
r4
summary(r4)

#dissociation after trauma t0
r5 <- lm(formula = PDEQ_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal, data = mydata)
r5
summary(r5)

# traumatic stress after trauma t0
r6 <- lm(formula = PDI_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal, data = mydata)
r6
summary(r6)

#### Multiple Regressions II
  #(without education because may be too related to trauma history)
  # In the case of dependent variables measured at time 1 will control for intervention (rama)
  # I decided to use Multiple regressions 1 to keep education as control variable 

# Depression t0
r11 <- lm(formula = BDI_t0 ~ TQ_t0 + Edad + Sexo, data = mydata)
r11
summary(r11)

# Depression t1
r12 <- lm(formula = BDI_t1 ~ TQ_t0 + Edad + Sexo + rama, data = mydata)
r12
summary(r12)

# PTSD score t0 
r13 <- lm(formula = PCL_t0 ~ TQ_t0 + Edad + Sexo, data = mydata)
r13
summary(r13)

# PTSD score t1   
r14 <- lm(formula = PCL_t1 ~ TQ_t0 + Edad + Sexo + rama, data = mydata)
r14
summary(r14)

# Dissociation after trauma t0
r15 <- lm(formula = PDEQ_t0 ~ TQ_t0 + Edad + Sexo, data = mydata)
r15
summary(r15)

# Traumatic stress after trauma t0

r16 <- lm(formula = PDI_t0 ~ TQ_t0 + Edad + Sexo, data = mydata)
r16
summary(r16)
#### Multiple Regressions III (final) ####
  # With education - as regressions 1- and adding MSPSS_t0 (percived social support)  
  # In the case of dependent variables measured at time 1 will control for intervention (rama)
  # Since social support is an important predictor, decided to use multiple regressions 3

# Depression t0
#attach(mydata2)
r21 <- lm(formula = BDI_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + MSPSS_t0, data = mydata)
r21
summary(r21)

# Depression t1
r22 <- lm(formula = BDI_t1 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + rama + MSPSS_t0, data = mydata)
r22
summary(r22)

# PTSD score t0 
r23 <- lm(formula = PCL_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + MSPSS_t0, data = mydata)
r23
summary(r23)

# PTSD score t1   
r24 <- lm(formula = PCL_t1 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + rama + MSPSS_t0, data = mydata)
r24
summary(r24)

#dissociation after trauma t0
r25 <- lm(formula = PDEQ_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + MSPSS_t0, data = mydata)
r25
summary(r25)

# traumatic stress after trauma t0
r26 <- lm(formula = PDI_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + MSPSS_t0, data = mydata)
r26
summary(r26)     

#### Other Multiple Regressions ####
# Adds to multiple regressions III dissociation and traumatic stress as predictor variables 
  # for PTSD and Depression T1

# Depression t1
r32 <- lm(formula = BDI_t1 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + rama + MSPSS_t0 + PDEQ_t0 + PDI_t0, data = mydata)
r32
summary(r32)

# PTSD score t1   
r34 <- lm(formula = PCL_t1 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + rama + MSPSS_t0  + PDEQ_t0 + PDI_t0, data = mydata)
r34
summary(r34)


##### Mediational Models ####
# RESULTS: none of the mediations were significant
## Mediational models:
# I: TQ_t0 (traumatic load) -> PDEQ_t0 (dissociation) -> PCL_t1 (PTSD 1 month after)  

# II: TQ_t0 (traumatic load) -> PDEQ_t0 (dissociation) -> BDI_t1 (depression 1 month after)
# III: TQ_t0 (traumatic load) -> PDI_t0 (peritraumatic distress) -> PCL_t1 (PTSD 1 month after)  
# IV: TQ_t0 (traumatic load) -> PDI_t0 (peritraumatic distress) -> BDI_t1 (depression 1 month after)
##Regresions with variables in mediational models ####
#All are significant

#Dissociation predicts PTSD
a1 <- lm(formula = PCL_t1 ~ PDEQ_t0, data = mydata)
a1
summary(a1)

#Dissociation predicts depression
a2 <- lm(formula = BDI_t1 ~ PDEQ_t0, data = mydata)
a2
summary(a2)

#Peritraumatic distress predicts PTSD
a3 <- lm(formula = PCL_t1 ~ PDI_t0, data = mydata)
a3
summary(a3)

#Peritraumatic distress predicts depression
a4 <- lm(formula = BDI_t1 ~ PDI_t0, data = mydata)
a4
summary(a4)

#Traumatic load predicts dissociation 
a5 <- lm(formula = PDEQ_t0 ~ TQ_t0, data = mydata)
a5
summary(a5)

#Traumatic load predicts peritraumatic distress 
a6 <- lm(formula = PDI_t0 ~ TQ_t0, data = mydata)
a6
summary(a6)
##Regressions adding control variables: ####
#Edad
#Sexo*: sex  
#CIDI_A8_basal: years of education
#MSPSS_t0: percived social support   
#rama*: intervention (PAP; Psicoeducation)- only when predicting T1 variable  
#All regressions are significant

#Dissociation predicts PTSD
a11 <- lm(formula = PCL_t1 ~ PDEQ_t0 + Edad + Sexo + CIDI_A8_basal + rama + MSPSS_t0, data = mydata)
a11
summary(a11)

#Dissociation predicts depression
a12 <- lm(formula = BDI_t1 ~ PDEQ_t0 + Edad + Sexo + CIDI_A8_basal + rama + MSPSS_t0, data = mydata)
a12
summary(a12)

#Peritraumatic distress predicts PTSD
a13 <- lm(formula = PCL_t1 ~ PDI_t0 + Edad + Sexo + CIDI_A8_basal + rama + MSPSS_t0, data = mydata)
a13
summary(a13)

#Peritraumatic distress predicts depression
a14 <- lm(formula = BDI_t1 ~ PDI_t0 + Edad + Sexo + CIDI_A8_basal + rama + MSPSS_t0, data = mydata)
a14
summary(a14)

#Traumatic load predicts dissociation 
a15 <- lm(formula = PDEQ_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + MSPSS_t0, data = mydata)
a15
summary(a15)

#Traumatic load predicts peritraumatic distress 
a16 <- lm(formula = PDI_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + MSPSS_t0, data = mydata)
a16
summary(a16)
#### Mediational Model I ####
  #I: TQ_t0 (traumatic load) -> PDEQ_t0 (dissociation) -> PCL_t1 (PTSD 1 month after) 

# Select data using subset function (to drop cases with missing values)
data_m1 <- subset(mydata, TQ_t0 != is.na(1) & PDEQ_t0 != is.na(1) & PCL_t1 != is.na(1), 
                  select=c("Nreclutado","TQ_t0", "PDEQ_t0", "PCL_t1"))

library(mediation)
med.fit<-lm(data_m1$PDEQ_t0~data_m1$TQ_t0)   #med.fit<-lm(M ~X) - Ecuaci?n de predicci?n del mediador (M) por la variables independiente (X)
summary(med.fit)
out.fit<-glm(data_m1$PCL_t1~data_m1$TQ_t0+data_m1$PDEQ_t0) #out.fit<-glm(D~ X+M) - Ecuaci?n de predicci?n de la variable dependiente (D) por la variables independiente (X) y mediadora
summary(out.fit)
med.out<-mediate(med.fit,out.fit, treat="data_m1$TQ_t0", sims=10000, mediator="data_m1$PDEQ_t0", robustSE=TRUE,  dropobs = TRUE) #Para obtener los coeficientes de medicaci?n con con significaci?n estimada con 10000 bootstraps
summary(med.out) 

#### Mediational Model II ####
  # II: TQ_t0 (traumatic load) -> PDEQ_t0 (dissociation) -> BDI_t1 (depression 1 month after)

# Select data using subset function (to drop cases with missing values)
data_m2 <- subset(mydata, TQ_t0 != is.na(1) & PDEQ_t0 != is.na(1) & BDI_t1 != is.na(1), 
                  select=c("Nreclutado","TQ_t0", "PDEQ_t0", "BDI_t1"))

library(mediation)
med.fit<-lm(data_m2$PDEQ_t0~data_m2$TQ_t0)   #med.fit<-lm(M ~X) - Ecuaci?n de predicci?n del mediador (M) por la variables independiente (X)
summary(med.fit)
out.fit<-glm(data_m2$BDI_t1~data_m2$TQ_t0+data_m2$PDEQ_t0) #out.fit<-glm(D~ X+M) - Ecuaci?n de predicci?n de la variable dependiente (D) por la variables independiente (X) y mediadora
summary(out.fit)
med.out<-mediate(med.fit,out.fit, treat="data_m2$TQ_t0", sims=10000, mediator="data_m2$PDEQ_t0", robustSE=TRUE,  dropobs = TRUE) #Para obtener los coeficientes de medicaci?n con con significaci?n estimada con 10000 bootstraps
summary(med.out) 
#### Mediational Model III ####
  # III: TQ_t0 (traumatic load) -> PDI_t0 (peritraumatic distress) -> PCL_t1 (PTSD 1 month after)  

data_m3 <- subset(mydata, TQ_t0 != is.na(1) & PDI_t0 != is.na(1) & PCL_t1 != is.na(1), 
                  select=c("Nreclutado","TQ_t0", "PDI_t0", "PCL_t1"))

library(mediation)
med.fit<-lm(data_m3$PDI_t0~data_m3$TQ_t0)   #med.fit<-lm(M ~X) - Ecuaci?n de predicci?n del mediador (M) por la variables independiente (X)
summary(med.fit)
out.fit<-glm(data_m3$PCL_t1~data_m3$TQ_t0+data_m3$PDI_t0) #out.fit<-glm(D~ X+M) - Ecuaci?n de predicci?n de la variable dependiente (D) por la variables independiente (X) y mediadora
summary(out.fit)
med.out<-mediate(med.fit,out.fit, treat="data_m3$TQ_t0", sims=10000, mediator="data_m3$PDI_t0", robustSE=TRUE,  dropobs = TRUE) #Para obtener los coeficientes de medicaci?n con con significaci?n estimada con 10000 bootstraps
summary(med.out) 
#### Mediational Model IV ####

  # IV: TQ_t0 (traumatic load) -> PDI_t0 (peritraumatic distress) -> BDI_t1 (depression 1 month after)

data_m4 <- subset(mydata, TQ_t0 != is.na(1) & PDI_t0 != is.na(1) & BDI_t1 != is.na(1), 
                  select=c("Nreclutado","TQ_t0", "PDI_t0", "BDI_t1"))

library(mediation)
med.fit<-lm(data_m4$PDI_t0~data_m4$TQ_t0)   #med.fit<-lm(M ~X) - Ecuaci?n de predicci?n del mediador (M) por la variables independiente (X)
summary(med.fit)
out.fit<-glm(data_m4$BDI_t1~data_m4$TQ_t0+data_m4$PDI_t0) #out.fit<-glm(D~ X+M) - Ecuaci?n de predicci?n de la variable dependiente (D) por la variables independiente (X) y mediadora
summary(out.fit)
med.out<-mediate(med.fit,out.fit, treat="data_m4$TQ_t0", sims=10000, mediator="data_m4$PDI_t0", robustSE=TRUE,  dropobs = TRUE) #Para obtener los coeficientes de medicaci?n con con significaci?n estimada con 10000 bootstraps
summary(med.out) 

#### Other Mediational Models ####
#V: CIDI_A8_basal (education) -> PDEQ_t0 (dissociation) -> PCL_t1 (PTSD 1 month after) 

# Select data using subset function (to drop cases with missing values)
data_m5 <- subset(mydata, CIDI_A8_basal != is.na(1) & PDEQ_t0 != is.na(1) & PCL_t1 != is.na(1), 
                  select=c("Nreclutado","CIDI_A8_basal", "PDEQ_t0", "PCL_t1"))

library(mediation)
med.fit<-lm(data_m1$CIDI_A8_basal~data_m1$TQ_t0)   #med.fit<-lm(M ~X) - Ecuaci?n de predicci?n del mediador (M) por la variables independiente (X)
summary(med.fit)
out.fit<-glm(data_m1$PCL_t1~data_m1$TQ_t0+data_m1$PDEQ_t0) #out.fit<-glm(D~ X+M) - Ecuaci?n de predicci?n de la variable dependiente (D) por la variables independiente (X) y mediadora
summary(out.fit)
med.out<-mediate(med.fit,out.fit, treat="data_m1$TQ_t0", sims=10000, mediator="data_m1$PDEQ_t0", robustSE=TRUE,  dropobs = TRUE) #Para obtener los coeficientes de medicaci?n con con significaci?n estimada con 10000 bootstraps
summary(med.out) 

#####End of Code####


