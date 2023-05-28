########TO DO


# Info about R: http://www.statmethods.net/index.html

# Call packages installed to work on this (not sure if all are needed)
library(psych)
# Automatically load psych package and other functions 
.First <- function(x) {library(psych)}
library(ctv)
library(GPArotation)

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
head(mydata,1)
# to see specific data [file,colum]- if left blank show all colum or file
hola <- my.data[2,]
fit

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

str(mydata)

##  Convert character variables to factors 
# unless the as.is= argument is specified
read.table


## Describe Variables all variables
library(psych)
describe(my.data)
# item name ,item number, nvalid, mean, sd, 
# median, mad, min, max, skew, kurtosis, se

library(psych)
# describe(BDI_t0)
# describe(Sexo)
# describe.by(Edad, Sexo)
mean(Edad)

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
# Don´t know how to display results in the other direction to read it easier

library(psych)
lm(formula = BDI1_t1 ~ Edad + Sexo, data = my.data)
#lm(formula = BDI_t0 ~ Edad + Sexo + tipo_trauma + CIDI_K_t1 + CIDI_A1_basal + CIDI_A3_basal + CIDI_A4_basal + CIDI_A4A_basal + CIDI_A4B_basal + CIDI_A7_basal + CIDI_A7A_basal + CIDI_A7B_basal + CIDI_A9_basal + CIDI_A10_basal + CIDI_A11_basal, data = my.data)


lm(formula = BDI_t0 ~ Edad + Sexo + PCL_t0 + PCL_t1 + MSPSS_t0 + PDEQ_t0 + PDI_t0 + TQ_t0 + EVA2_t0 + HAM_D_t1, data = mydata)

## Regresiones múltiples con número de experiencias traumáticas (TQ variable TQ_t0) en la historia se podría usar como predictor de otras variables como:
# BDI (depresión al comienzo  BDI_t0  BDI_t1)
# to add p-values
  ## (summary(lm(formula = BDI1_t0 ~ Edad + Sexo + TQ_t0, data = my.data))
# mat.regress(BDI1_t0, Edad, Sexo, TQ_t0,my.data)
# mat.regress(my.data,c("BDI1_t0", "Edad","Sexo"))
# mat.regress(BDI1_t0, TQ_t0,my.data, z=NULL,n.obs=NULL)

# PCL-C (estrés post-traumático?  PCL_t0  PCL_t1)
# PEDQ - nivel de disociación
# recuperación del evento (entre baseline y post, controlando por tipo de tratamiento)

?`psych-package`



        




###############EJERCICIOS


###############Dont know how to use

library(psych)
lm(formula = BDI1_t1 ~ Edad + Sexo, data = mydata)
summary(fit) # show results !!!!Doesn`t work as I expect it




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



