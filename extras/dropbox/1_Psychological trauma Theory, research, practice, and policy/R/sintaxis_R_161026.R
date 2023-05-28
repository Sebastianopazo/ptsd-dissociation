########TO DO

### Resources
# Info about R: http://www.statmethods.net/index.html
# Quick R (answer to questions) http://www.statmethods.net/index.html
# To learn R basics https://www.openintro.org/stat/labs.php?stat_lab_software=R
# Intro to R and R Studio https://docs.google.com/document/d/1c1jgPrHXtkIv3cuJ_f7Y-Oy2lvo0sFTplHfyD-z8Qi4/edit
# Intro to data https://docs.google.com/document/d/1EW_9L7fM8KHFO-m_isZwxLfDsE74MlZNLacPjwBOOb8/edit
# Psych package http://personality-project.org/r/r.guide.html

# Markdown - to export syntaxis to word

##############################################################################

# Call packages installed to work on this (not sure if all are needed)
library(psych)
# Automatically load psych package and other functions 
.First <- function(x) {library(psych)}
library(ctv)
library(GPArotation)

## Before importing file to R
# Prepared excell to import into R: erased simbol after N and erased $;
# Changed "perdido" por NA
# Changed #NULL! for NA
# Changed 999 for NA
# Changed " " (empty cell) for NA
# Transformed excell into CSV
# Continued preparind data in CSV
# Erased values nombre_pac to preserve confidentiality of subjets

# Set work directory
setwd("C:/Users/Paula Errazuriz/Dropbox/1 PROYECTOS_INVESTIGACION/desastres_naturales/paper factores riesgo PTSD Rodrigo")

# Open Data :)
my.data <- read.csv("161024 pacientes_excell_para_r.csv",
                   header = TRUE, sep = ";")

# See data
head(my.data,1)
# to see specific data [file,colum]- if left blank show all colum or file

#### Proyect: past traumatic experiences as predictor of mental health after
     #new traumatic event

### Variables of interest

## Numeric variables
# Edad
# CIDI_A8_basal: years of education
# BDI_t0: Depression t0          
# BDI_t1: Depression t1          
# PCL_t0: PTSD score t0            
# PCL_t1: PTSD score t1             
# MSPSS_t0: percived social support?          
# PDEQ_t0: dissociation after trauma score           
# PDI_t0: traumatic stress after trauma            
# TQ_t0: trauma history (number of traumatic events?)   

## Factor variables
# Sexo*: sex              
# tipo_trauma*: trauma tipe       
# rama*: intervention (PAP; Psicoeducation)              
# CIDI_K_t1*: presence of PTSD at t1? (TEPT- TEPT+) 

### Understanding and preparing data in R

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

#### Outliers
### !!!!!!!!!! To do: 
## Found but cant fix: univariate outliers
  # CIDI_A8_basal  #years of education. Colum 38, file 164 and 190)
  # TQ_t0  #trauma history (number of traumatic events?). 
## Identify and fix bivariate outliers 
  # One highest TQ_t0 is creating bivariate outliers

outl <- 3.29
#Transormar a Z scores

ztemp <- scale(my.data$Edad, center=TRUE, scale = TRUE)

#Indices que cumplen con mi criterio
ind.o <- which(abs(ztemp) > outl)
ind.no <- which(abs(ztemp) <= outl)

#Sacando los outliers, los min/max
max.d <- max(my.data$Edad[ind.no])
min.d <- min(my.data$Edad[ind.no])

my.data$Edad[which(my.data$Edad > max.d)] <- max.d+1 
my.data$Edad[which(my.data$Edad > max.d)] <- max.d+1 


#Or change outliers by hand considering: 	Outliers, with z-scores larger than ± 3.29, 
    #will be transformed by replacing their raw score value with the next highest or 
    #lowest value as recommended by Tabachnick and Fidell (2001). 
  #reassign depth values under 10 to zero
  #df$depth[df$depth<10] <- 0


####crear variable (invento)
total.BDI <- my.data$BDI14_t0 + my.data$BDI15_t0
str(total.BDI)
total.BDI <- my.data$BDI14_t0 + my.data$BDI15_t0

which( colnames(my.data)=="BDI1_t0")

total.BDI <- rowSums(my.data[,44:64])



str(resultado)

str(total.BDI)


### Multiple Regression
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
#*****Controlling for age, sex and years of education, the number of previous 
  #traumatic experiences significantly predicts PTSD symtomatology right after
  #a traumatic event
r3 <- lm(formula = PCL_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal, data = my.data)
r3
summary(r3)

# PTSD score t1   
#*****Controlling for age, sex and years of education, and intervention, 
  #the number of previous traumatic experiences significantly predicts PTSD 
  #symtomatology right after a traumatic event
r4 <- lm(formula = PCL_t1 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + rama, data = my.data)
r4
summary(r4)

#dissociation after trauma score           
r5 <- lm(formula = PDEQ_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal, data = my.data)
r5
summary(r5)

# traumatic stress after trauma  
#*****Controlling for age, sex and years of education, the number of previous 
  #traumatic experiences significantly predicts traumatic stress right after a 
  #traumatic event
 
r6 <- lm(formula = PDI_t0 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal, data = my.data)
r6
summary(r6)

# presence of PTSD at t1? (TEPT- TEPT+) 
###!!!!!Can`t do. Probably because dependent variable is a factor variable
r7 <- lm(formula = CIDI_K_t1 ~ TQ_t0 + Edad + Sexo + CIDI_A8_basal + rama, data = my.data)
r7
summary(r7)




######################################################################################


#EJERCICIOS


###############Dont know how to use


######### Following all steps of psych package
#### http://personality-project.org/r/psych/vignettes/overview.pdf

# Install psych package
install.packages("psych")
library(psych)


## Install other recommended packages
install.packages("ctv")
library(ctv)

# install.packages("Psychometrics") ## could not install, not available for R version 3.3.1

install.packages("GPArotation")
library(GPArotation)
install.packages("mnormt") 
library(mnormt)
# install.packages("Rgraphviz") ## could not install 

## Imput data from clipboard ## could not do it, so imported it below
# read.clipboard.csv
# my.data <- read.clipboard

# Set work directory
setwd("C:/Users/Paula Errazuriz/Dropbox/1 PROYECTOS_INVESTIGACION/desastres_naturales/paper factores riesgo PTSD Rodrigo")

# Open Data :)
my.data <- read.csv("161018 pacientes_excell_para_r.csv",
                   header = TRUE, sep = ";")

# Describe data
describe(my.data) #basic descriptive statistics
headTail(my.data)


## Basic descriptive statistics by a grouping variable 
# All variables by Sexo
describeBy(my.data,my.data$Sexo,skew=FALSE,ranges=FALSE)
# All variables by presence or not of TEPT
describeBy(my.data,my.data$CIDI_K_t1,skew=FALSE,ranges=FALSE)

## The output from the describeBy function can be forced into a matrix form for easy analysis by other programs. 
# describeBy can group by several grouping variables at the same time
sa.mat <- describeBy(my.data,list(my.data$Sexo,my.data$CIDI_K_t1), skew=FALSE,ranges=FALSE,mat=TRUE)
headTail(sa.mat)
summary(sa.mat)

## Outliers !!!! Can`t do with my data
# Outlier detection using outlier
png( 'outlier.png' )
#d2 <- outlier(sat.act)
d2 <- outlier(my.data)
sat.d2 <- data.frame(sat.act,d2)
pairs.panels(sat.d2,bg=c("yellow","blue")[(d2 > 25)+1],pch=21)
dev.off()

### Data cleaning using scrub !!! Can´t do with my data
  #Consider a data set of 10 rows of 12 columns with values from 1 - 120. All values of columns
  #3 - 5 that are less than 30, 40, or 50 respectively, or greater than 70 in any of the three
  #columns will be replaced with NA. In addition, any value exactly equal to 45 will be set
  #to NA. (max and isvalue are set to one value here, but they could be a different value for
  #every column).
x <- matrix(1:120,ncol=10,byrow=TRUE)
colnames(x) <- paste('V',1:10,sep='')
new.x <- scrub(x,3:5,min=c(30,40,50),max=70,isvalue=45,newvalue=NA)
new.x


### Recoding categorical variables into dummy coded variables
# Form "dummy codes" (binary variables) for each category using dummy.code. 
# To analyze these dummy coded variables use biserial or point biserial (regular Pearson r) 
  # to show effect sizes and may be plotted in e.g., spider plots

new <- dummy.code(my.data$Sexo)
new.sat <- data.frame(new,my.data)
##round(cor(new.sat,use="pairwise"),2) !!!no me resulta

new <- dummy.code(my.data$tipo_trauma)
new.sat <- data.frame(new,my.data)

new <- dummy.code(my.data$rama)
new <- dummy.code(my.data$CIDI_K_t1)
new <- dummy.code(my.data$CIDI_A1_basal)
new <- dummy.code(my.data$CIDI_A4_basal)
new <- dummy.code(my.data$CIDI_A4A_basal)
new <- dummy.code(my.data$CIDI_A4B_basal)
new <- dummy.code(my.data$CIDI_A7_basal)
new <- dummy.code(my.data$CIDI_A7A_basal)
new <- dummy.code(my.data$CIDI_A9_basal)
new <- dummy.code(my.data$CIDI_A11_basal)
new <- dummy.code(my.data$filter_)

describe(my.data) 
describeBy(my.data,my.data$Sexo,skew=FALSE,ranges=FALSE)

### Descriptive graphics
# png( 'pairspanels.png' )
sat.d2 <- data.frame(sat.act,d2) #combine the d2 statistics from before with the sat.act data.frame
pairs.panels(sat.d2,bg=c("yellow","blue")[(d2 > 25)+1],pch=21)
dev.off()



