####1: R and data preparation####
#Always need to run this syntax (and only this) before any of the ones that follow

###NOTES####
  
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

###CODE BOOK####

### Variables of interest (selected for theoretical reasons)

##To now column number of colum 
#which( colnames(mydata)=="Edad" )
## Numeric variables
# Edad - colnames 4
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

###PACKAGES AND DATA IMPORT#######################################################################################
----
  
## installing/loading the package:
#if(!require(installr)) {
#install.packages("installr"); require(installr)} #load / install+load installr

## using the package:
#updateR() # this will start the updating process of your R installation.  
  #It will check for newer versions, and if one is available, will guide you 
  #through the decisions you'd need to make.

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
#library(MASS)
#library(Matrix)
#library(mvtnorm)
#library(sandwich)
#library(chron)
#library(Rcpp)
#library(stringi)
#library(data.table)
library(mediation)


## Automatically load psych package and other functions 
#.First <- function(x) {library(psych)}
#library(ctv)
#library(GPArotation)

# Set work directory
setwd("C:/Users/Paula Errazuriz/Dropbox/1 PROYECTOS_INVESTIGACION/desastres_naturales/paper factores riesgo PTSD Rodrigo")

# Open Data :)
mydata <- read.csv("161103 pacientes_excell_para_r.csv",
                                      header = TRUE, sep = ";")
# See data
head(mydata,1)
# to see specific data [file,colum]- if left blank show all colum or file

###UNDERSTANDING AND PREPARING DATA######################################################################################################

# Summary of data
str(mydata)

# Describe all variables (n, mean, sd, median, trimmed (mean discarding high and low values))
library(psych)
describe(mydata)

# Convert character variables to factors (unless the as.is= argument is specified)
read.table

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

#### Univariate Outliers

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
#!!!!!!!!!!!!!!Not working. Don´t know if it doesn`t make change or doesn´t save it
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

#Or change outliers by hand considering: 	Outliers, with z-scores larger than ± 3.29, 
    #will be transformed by replacing their raw score value with the next highest or 
    #lowest value as recommended by Tabachnick and Fidell (2001). 
  #reassign depth values under 10 to zero
  #df$depth[df$depth<10] <- 0

##########End of Code#############################################################################

