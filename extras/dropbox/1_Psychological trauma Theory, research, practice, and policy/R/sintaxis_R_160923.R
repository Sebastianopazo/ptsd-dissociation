########TO DO
#Change 999 to missing value

# Info about R: http://www.statmethods.net/index.html

# Call packages installed to work on this (not sure if all are needed)
library(psych)
library(ctv)
library(GPArotation)

# Prepared excell to import into R: erased simbol after N and erased $;
# Changed "perdido" por NA; changed #NULL! for NA
# Transformed excell into CSV

# Set work directory
setwd("C:/Users/Paula Errazuriz/Dropbox/1 PROYECTOS_INVESTIGACION/desastres_naturales/paper factores riesgo PTSD Rodrigo")

# Open Data :)
mydata <- read.csv("160906 pacientes_excell_para_r.csv",
                   header = TRUE, sep = ";")

# See data
head(mydata,1)
# to see specific data [file,colum]- if left blank show all colum or file
#hola <- mydata[2,]
#fit

table(mydata$rama)
table(mydata$CIDI_K_t1)
table(mydata$filter_)
table(mydata$PCL_t0)
table(mydata$MSPSS_t0)
table(mydata$PDEQ_t0)
table(mydata$PDI_t0)
table(mydata$TQ_t0)
table(mydata$EVA2_t0)
table(mydata$HAM_D_t1)


?table
str(mydata)

##  Convert character variables to factors 
# unless the as.is= argument is specified
read.table









## Describe Variables all variables
library(psych)
describe(mydata)
# item name ,item number, nvalid, mean, sd, 
# median, mad, min, max, skew, kurtosis, se

library(psych)
describe(BDI_t0)
describe.by(Edad, Sexo)
mean(edad)

# basic descriptive statistics by a grouping variable
describeBy(mydata,mydata$Sexo,skew=FALSE,ranges=FALSE)

# other descriptives
describe(mydata, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1,
         type=3,check=TRUE,fast=NULL,quant=NULL,IQR=FALSE)
describeData(mydata,head=4,tail=4)


# list levels of factors in Factor Variables in mydata
levels(mydata$Sexo)
levels(mydata$tipo_trauma)
levels(mydata$tronco)
levels(mydata$rama)

library(psych)
data(sat.act)
describe(mydata) #basic descriptive statistics

### Cleaning data
## Changing 999 (missing data) to NA
# reassign depth values under 10 to zero
df$depth[df$depth<10] <- 0





## Multiple Regression

library(psych)
lm(formula = BDI1_t1 ~ Edad + Sexo, data = mydata)

library(psych)
lm(formula = BDI1_t1 ~ Edad + Sexo, data = mydata)


        




###############EJERCICIOS


###############Dont know how to use

library(psych)
lm(formula = BDI1_t1 ~ Edad + Sexo, data = mydata)
summary(fit) # show results !!!!Doesn`t work as I expect it










