### Resources
# Info about R: http://www.statmethods.net/index.html
# Quick R (answer to questions) http://www.statmethods.net/index.html
# To learn R basics https://www.openintro.org/stat/labs.php?stat_lab_software=R
# Intro to R and R Studio https://docs.google.com/document/d/1c1jgPrHXtkIv3cuJ_f7Y-Oy2lvo0sFTplHfyD-z8Qi4/edit
# Intro to data https://docs.google.com/document/d/1EW_9L7fM8KHFO-m_isZwxLfDsE74MlZNLacPjwBOOb8/edit
# Psych package http://personality-project.org/r/r.guide.html

# Markdown - to export syntaxis to word


#####USEFULL COMMANDS

to see unique observations for a variable 
#length(unique(mydata$Nreclutado)) #patients who participated in study
#length(unique(mydata$Ninvitado)) #all invited patients


####crear variable (invento Ale para ejemplo)

# Set work directory
setwd("C:/Users/Paula Errazuriz/Dropbox/1 PROYECTOS_INVESTIGACION/desastres_naturales/paper factores riesgo PTSD Rodrigo")

# Open Data :)
mydata <- read.csv("161103 pacientes_excell_para_r.csv",
                    header = TRUE, sep = ";")

total.BDI <- mydata$BDI14_t0 + mydata$BDI15_t0
str(total.BDI)
total.BDI <- mydata$BDI14_t0 + mydata$BDI15_t0

which( colnames(mydata)=="BDI1_t0")

total.BDI <- rowSums(mydata[,44:64])

str(resultado)

str(total.BDI)


##To now column number of colum 
which( colnames(mydata)=="Edad" )


#### Example of how to create a variable (by Alejandro)
which( colnames(mydata)=="BDI1_t0")
total.BDI <- rowSums(mydata[,44:64])
str(total.BDI)


#Installing/loading the R package:
if(!require(installr)) {
install.packages("installr"); require(installr)} #load / install+load installr

## Update R:
updateR() # this will start the updating process of your R installation.  
#It will check for newer versions, and if one is available, will guide you 
#through the decisions you'd need to make.


###########EJERCICIOS


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
# mydata <- read.clipboard

# Set work directory
setwd("C:/Users/Paula Errazuriz/Dropbox/1 PROYECTOS_INVESTIGACION/desastres_naturales/paper factores riesgo PTSD Rodrigo")

# Open Data :)
mydata <- read.csv("161018 pacientes_excell_para_r.csv",
                    header = TRUE, sep = ";")

# Describe data
describe(mydata) #basic descriptive statistics
headTail(mydata)


## Basic descriptive statistics by a grouping variable 
# All variables by Sexo
describeBy(mydata,mydata$Sexo,skew=FALSE,ranges=FALSE)
# All variables by presence or not of TEPT
describeBy(mydata,mydata$CIDI_K_t1,skew=FALSE,ranges=FALSE)

## The output from the describeBy function can be forced into a matrix form for easy analysis by other programs. 
# describeBy can group by several grouping variables at the same time
sa.mat <- describeBy(mydata,list(mydata$Sexo,mydata$CIDI_K_t1), skew=FALSE,ranges=FALSE,mat=TRUE)
headTail(sa.mat)
summary(sa.mat)

## Outliers !!!! Can`t do with my data
# Outlier detection using outlier
png( 'outlier.png' )
#d2 <- outlier(sat.act)
d2 <- outlier(mydata)
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

new <- dummy.code(mydata$Sexo)
new.sat <- data.frame(new,mydata)
##round(cor(new.sat,use="pairwise"),2) !!!no me resulta

new <- dummy.code(mydata$tipo_trauma)
new.sat <- data.frame(new,mydata)

new <- dummy.code(mydata$rama)
new <- dummy.code(mydata$CIDI_K_t1)
new <- dummy.code(mydata$CIDI_A1_basal)
new <- dummy.code(mydata$CIDI_A4_basal)
new <- dummy.code(mydata$CIDI_A4A_basal)
new <- dummy.code(mydata$CIDI_A4B_basal)
new <- dummy.code(mydata$CIDI_A7_basal)
new <- dummy.code(mydata$CIDI_A7A_basal)
new <- dummy.code(mydata$CIDI_A9_basal)
new <- dummy.code(mydata$CIDI_A11_basal)
new <- dummy.code(mydata$filter_)

describe(mydata) 
describeBy(mydata,mydata$Sexo,skew=FALSE,ranges=FALSE)

### Descriptive graphics
# png( 'pairspanels.png' )
sat.d2 <- data.frame(sat.act,d2) #combine the d2 statistics from before with the sat.act data.frame
pairs.panels(sat.d2,bg=c("yellow","blue")[(d2 > 25)+1],pch=21)
dev.off()

mean(mydata$Edad)
cor(1:2,)

