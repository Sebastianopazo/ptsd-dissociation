## Info about R (recomended by Alejandro Zahler): http://www.statmethods.net/index.html

## Configure working directory
setwd("C:/Users/Paula Errazuriz/Dropbox/1 PROYECTOS_INVESTIGACION/desastres_naturales/paper factores riesgo PTSD Rodrigo")

## Open data :)
mydata <- read.csv("160906 pacientes_excell_para_r.csv",
                   header = TRUE, sep = ";")
## See all variables
str(df2)

## Describe Variables
library(psych)
describe(mydata)
# item name ,item number, nvalid, mean, sd, 
# median, mad, min, max, skew, kurtosis, se



