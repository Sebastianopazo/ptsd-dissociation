###NOTES######################################################################################################

# This syntax was created by Paula Errázuriz starting on Sept 2016 to work with the data of 
  #Rodrigo Figueroa. Also worked with Paula Cortes from CIGIDEN
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
#Saved as file 161103


###CODE BOOK######################################################################################################

### Variables of interest (selected for theoretical reasons)

## Numeric variables
# Edad
# CIDI_A8_basal: years of education
# BDI_t0: Depression t0 (symptoms last 7 days)          
# BDI_t1: Depression t1 (symptoms last 7 days)     
# PCL_t0: PTSD score t0 (symptoms last month; inespecific, not realated to 1 event)            
# PCL_t1: PTSD score t1 (symptoms last month; inespecific, not realated to 1 event)       
# PDEQ_t0: dissociation during last traumatic event           
# PDI_t0: peritraumatic distress during the last traumatic event   

# TQ_t0: traumatic load (number of traumatic events)   
# MSPSS_t0: percived social support   

## Factor variables
# Sexo*: sex              
# tipo_trauma*: trauma tipe       
# rama*: intervention (PAP; Psicoeducation)              
# CIDI_K_t1*: presence of PTSD at t1? (TEPT- TEPT+) 


###PACKAGES AND DATA IMPORT#######################################################################################

# Call packages installed to work on this (not sure if all are needed)
library(psych)
# Automatically load psych package and other functions 
.First <- function(x) {library(psych)}
library(ctv)
library(GPArotation)

# Set work directory
setwd("C:/Users/Paula Errazuriz/Dropbox/1 PROYECTOS_INVESTIGACION/desastres_naturales/paper factores riesgo PTSD Rodrigo")

# Open Data :)
my.data <- read.csv("161103 pacientes_excell_para_r.csv",
                                      header = TRUE, sep = ";")
# See data
head(my.data,1)
# to see specific data [file,colum]- if left blank show all colum or file


###UNDERSTANDING AND PREPARING DATA######################################################################################################

# Summary of data
str(my.data)

# Describe all variables (n, mean, sd, median, trimmed (mean discarding high and low values))
describe(my.data)

# Convert character variables to factors (unless the as.is= argument is specified)
read.table

# List factor levels for factor variables and frecuency of values
table(my.data$Sexo)
table(my.data$tipo_trauma)
table(my.data$rama)
table(my.data$CIDI_K_t1)

# Describe each numeric Variable (mean, sd, median, trimmed, mad, min, max, range, 
  #skew, kurtosis (posible outliers if greater than 3), se)
describe(my.data$Edad)#age
describe(my.data$BDI_t0)#Depression t0  
describe(my.data$BDI_t1)#Depression t1 
describe(my.data$PCL_t0)#PTSD score t0  
describe(my.data$PCL_t1)# PTSD score t1  
describe(my.data$CIDI_A8_basal)#years of education
describe(my.data$MSPSS_t0)#percived social support?
describe(my.data$PDEQ_t0)#dissociation after trauma score  
describe(my.data$PDI_t0)#traumatic stress after trauma  
describe(my.data$TQ_t0)#trauma history (number of traumatic events?)   

# other descriptives
describe(mydata, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1,
         type=3,check=TRUE,fast=NULL,quant=NULL,IQR=FALSE)
describeData(my.data,head=4,tail=4)
describeBy(my.data,my.data$Sexo,skew=FALSE,ranges=FALSE)

# Plot 1 variable
plot(my.data$Edad)#age
plot(my.data$BDI_t0)#Depression t0  
plot(my.data$PCL_t0)#PTSD score t0  
plot(my.data$BDI_t1)#Depression t1 
plot(my.data$PCL_t1)# PTSD score t1  
plot(my.data$CIDI_A8_basal)#years of education
plot(my.data$MSPSS_t0)#percived social support?
plot(my.data$PDEQ_t0)#dissociation after trauma score  
plot(my.data$PDI_t0)#traumatic stress after trauma  
plot(my.data$TQ_t0)#trauma history (number of traumatic events?)  

# Histogram
hist(my.data$Edad)#age
hist(my.data$BDI_t0)#Depression t0  
hist(my.data$PCL_t0)#PTSD score t0  
hist(my.data$BDI_t1)#Depression t1 
hist(my.data$PCL_t1)# PTSD score t1  
hist(my.data$CIDI_A8_basal)#years of education
hist(my.data$MSPSS_t0)#percived social support?
hist(my.data$PDEQ_t0)#dissociation after trauma score  
hist(my.data$PDI_t0)#traumatic stress after trauma  
hist(my.data$TQ_t0)#trauma history (number of traumatic events?) 

# Plot 2 varibles (each independent variable with the dependent variable 
  #(TQ_t0  trauma history (number of traumatic events?)))
plot(my.data$TQ_t0,my.data$Edad)#age
plot(my.data$TQ_t0,my.data$BDI_t0)#Depression t0  
plot(my.data$TQ_t0,my.data$PCL_t0)#PTSD score t0  
plot(my.data$TQ_t0,my.data$BDI_t1)#Depression t1 
plot(my.data$TQ_t0,my.data$PCL_t1)# PTSD score t1  
plot(my.data$TQ_t0,my.data$CIDI_A8_basal)#years of education
plot(my.data$TQ_t0,my.data$MSPSS_t0)#percived social support?
plot(my.data$TQ_t0,my.data$PDEQ_t0)#dissociation after trauma score  
plot(my.data$TQ_t0,my.data$PDI_t0)#traumatic stress after trauma  

#Also plotted variables that could have high correlation
plot(my.data$Edad,my.data$CIDI_A8_basal)

#### Univariate Outliers

# Deciding cut-off point of z=3.29 for outliers
outl <- 3.29

# Transforming to z-scores
ztemp <- scale(my.data$Edad, center=TRUE, scale = TRUE)

# Finding data for a numerical variable that IS an autlier 
ind.o <- which(abs(ztemp) > outl)

# Repeating last 2 steps for all other numerical variables
  #For variables with outliers, continue with steps to fix outliers
  #Need to add consecutive number after each variable created (ztemp and ind.o)
ztemp1 <- scale(my.data$BDI_t0, center=TRUE, scale = TRUE)
ind.o1 <- which(abs(ztemp1) > outl)
ztemp2 <- scale(my.data$PCL_t0, center=TRUE, scale = TRUE)
ind.o2 <- which(abs(ztemp2) > outl)
ztemp3 <- scale(my.data$PCL_t1, center=TRUE, scale = TRUE)
ind.o3 <- which(abs(ztemp3) > outl)
ztemp4 <- scale(my.data$CIDI_A8_basal), center=TRUE, scale = TRUE)
ind.o4 <- which(abs(ztemp4) > outl)
ztemp5 <- scale(my.data$MSPSS_t0, center=TRUE, scale = TRUE)
ind.o5 <- which(abs(ztemp5) > outl)
ztemp6 <- scale(my.data$BDI_t1), center=TRUE, scale = TRUE)
ind.o6 <- which(abs(ztemp6) > outl)
ztemp7 <- scale(my.data$PDEQ_t0, center=TRUE, scale = TRUE)
ind.o7 <- which(abs(ztemp7) > outl)
ztemp8 <- scale(my.data$PDI_t0, center=TRUE, scale = TRUE)
ind.o8 <- which(abs(ztemp8) > outl)
ztemp9 <- scale(my.data$TQ_t0, center=TRUE, scale = TRUE)
ind.o9 <- which(abs(ztemp9) > outl)

## For variables with outliers
# Finding data for each numerical variable that is NOT an autlier 
ind.no9 <- which(abs(ztemp9) <= outl)

# Detecting highest (or lowest) value before outlier
max.d <- max(my.data$TQ_t0[ind.no9])
min.d <- min(my.data$TQ_t0[ind.no9])

# Changing highest (or lowest) value before outlier
#!!!!!!!!!!!!!!Not working. Don´t know if it doesn`t make change or doesn´t save it
my.data$TQ_t0[which(my.data$TQ_t0 > max.d)] <- max.d+1 
my.data$TQ_t0[which(my.data$TQ_t0 < min.d)] <- min.d+1 

# Repeat outlier detection to confirm fixed outliers
ztemp10 <- scale(my.data$TQ_t0, center=TRUE, scale = TRUE)
ind.o10 <- which(abs(ztemp10) > outl)
ind.no10 <- which(abs(ztemp10) <= outl)

## For variables with outliers
# Finding data for each numerical variable that is NOT an autlier 
ind.no9 <- which(abs(ztemp9) <= outl)
ind.no9 <- which(abs(ztemp9) <= outl)

#Or change outliers by hand considering: 	Outliers, with z-scores larger than ± 3.29, 
    #will be transformed by replacing their raw score value with the next highest or 
    #lowest value as recommended by Tabachnick and Fidell (2001). 
  #reassign depth values under 10 to zero
  #df$depth[df$depth<10] <- 0

#### Example of how to create a variable (by Alejandro)
total.BDI <- my.data$BDI14_t0 + my.data$BDI15_t0
str(total.BDI)
total.BDI <- my.data$BDI14_t0 + my.data$BDI15_t0

which( colnames(my.data)=="BDI1_t0")

total.BDI <- rowSums(my.data[,44:64])

str(resultado)

str(total.BDI)


###RESEARCH QUESTIONS################################################################################################

####### Proyect I: traumatic load as predictor of mental health after
#new traumatic event and after 1 month (time 1)

### Multiple Regressions 1
# In the case of dependent variables measured at time 1 will control for intervention (rama)

# Depression t0
r1 <- lm(formula = BDI1_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal, data = my.data)
r1
summary(r1)

# Depression t1
r2 <- lm(formula = BDI1_t1 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + rama, data = my.data)
r2
summary(r2)

# PTSD score t0 
r3 <- lm(formula = PCL_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal, data = my.data)
r3
summary(r3)

# PTSD score t1   
r4 <- lm(formula = PCL_t1 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + rama, data = my.data)
r4
summary(r4)

#dissociation after trauma t0
r5 <- lm(formula = PDEQ_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal, data = my.data)
r5
summary(r5)

# traumatic stress after trauma t0
r6 <- lm(formula = PDI_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal, data = my.data)
r6
summary(r6)

 

### Multiple Regressions 2 (without education because may be too related to 
  #trauma history)
# In the case of dependent variables measured at time 1 will control for intervention (rama)
# I decided to use Multiple regressions 1 to keep education as control variable 
  
# Depression t0
r11 <- lm(formula = BDI1_t0 ~ TQ_t0 + Edad + Sexo, data = my.data)
r11
summary(r11)

# Depression t1
r12 <- lm(formula = BDI1_t1 ~ TQ_t0 + Edad + Sexo + rama, data = my.data)
r12
summary(r12)

# PTSD score t0 
r13 <- lm(formula = PCL_t0 ~ TQ_t0 + Edad + Sexo, data = my.data)
r13
summary(r13)

# PTSD score t1   
r14 <- lm(formula = PCL_t1 ~ TQ_t0 + Edad + Sexo + rama, data = my.data)
r14
summary(r14)

# Dissociation after trauma t0
r15 <- lm(formula = PDEQ_t0 ~ TQ_t0 + Edad + Sexo, data = my.data)
r15
summary(r15)

# Traumatic stress after trauma t0

r16 <- lm(formula = PDI_t0 ~ TQ_t0 + Edad + Sexo, data = my.data)
r16
summary(r16)
         

### Multiple Regressions 3 (with education - as regressions 1- and adding MSPSS_t0 
  #(percived social support)  
# In the case of dependent variables measured at time 1 will control for intervention (rama)
# Since social support is an important predictor, decided to use multiple regressions 3

# Depression t0
r21 <- lm(formula = BDI1_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + MSPSS_t0, data = my.data)
r21
summary(r21)

# Depression t1
r22 <- lm(formula = BDI1_t1 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + rama + MSPSS_t0, data = my.data)
r22
summary(r22)

# PTSD score t0 
r23 <- lm(formula = PCL_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + MSPSS_t0, data = my.data)
r23
summary(r23)

# PTSD score t1   
r24 <- lm(formula = PCL_t1 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + rama + MSPSS_t0, data = my.data)
r24
summary(r24)

#dissociation after trauma t0
r25 <- lm(formula = PDEQ_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + MSPSS_t0, data = my.data)
r25
summary(r25)

# traumatic stress after trauma t0
r26 <- lm(formula = PDI_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + MSPSS_t0, data = my.data)
r26
summary(r26)                  
                  

##################################################################################################

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


####### Proyect III: Test 2 moderational models:
#III.1: PDEQ_t0 (dissociation) moderates TQ_t0 (traumatic load) -> PCL_t1 (PTSD 1 month after)  
#III.2: PDEQ_t0 (dissociation) moderates TQ_t0 (traumatic load) -> BDI_t1 (depression 1 month after)




