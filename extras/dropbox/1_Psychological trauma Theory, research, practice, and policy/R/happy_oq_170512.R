###### NOTES ####
  
# This syntax was created by Paula Errazuriz starting on May 2017 to work with the data of FONDECYT Inicio 2011. 
# Original data file was created by Paula Errazuriz and saved as sigal_all_variables_2.xlsx

## Before importing file to R
# Changed   (empty cell) for NA
# Transformed excell into CSV
# Saved as file happy_170512.csv


###### COMPLETE CODE BOOK ####
##Therapist???s demographic variables start with a t and are followed by the same name as patients??? variables.

#pacien; patient id
#terape; therapist id
#grupo; feedback group (0=no feedback; 1=raw oq; 2=raw wai; 3=raw oq and wai; 4=oq processed- Lambert??s report)  
#sesion; session in which data was collected
#fecha; date data was collected 
#antes1; (before 0; after 1); if OQ was administered before or after session 
#antes2; (before 0; after 1); if wai was administered before or after session 
#oq*; answer to oq items
#oq; oq score (scored with formula provided in manual) ??? greater score is greater psychological 
#oqascore; oq score according to oq analyst output
#oqacolor; warning system produced by oq analyst: nothing= NA (first time oq is collected); 1=Red. The patient is 
 #deviating from the expected response to treatment. They are not on track to realize substantial benefit from treatment;
 #2=Yellow. The rate of change the patient is making is less than expected. This patient may end up with no significant 
 #benefit from therapy; 3=Green. Although the patient has not yet recovered his/her progress appears to be on track; 
 #4=White. The patient is functioning in the normal range. Consider termination.
#oqablue; don??t know what this variable means. Was included in oq analyst output (0=True; 1=False).
#n_track: not on track. Patients who were ever identified at any point during the course of treatment as failing to 
 #progress as expected (single or multiple yellow/red (2 or 1) color- coded messages) formed the not-on-track treatment 
 #condition (1=not on track), whereas those progressing as expected throughout treatment (only white or green color-coded 
 #messages, 4 or 3) comprised the on-track treatment group (0= on track).
#oqdif; difference between score of first and last oq of each client
#oq_oc_d: Deteriorated - Worsened by at least 10 points on the OQ-30 from pre- to post treatment. 
#oq_oc_nc: No change - Improved less than 10 points and worsened by less than 10 points on the OQ-30. 
#oq_oc_rc: Reliable change - Improved by at least 10 points on the OQ-30 but did not pass the cutoff between 
 #dysfunctional and functional populations (44). 
#oq_oc_csc: Clinically significant change - Improved by at least 10 points on the OQ-30 and passed the cutoff between 
 #dysfunctional and functional populations (44). 
#high_foq: first oq over percentile 50
#high_foq2: first oq 1SD over mean 
#high_foq3: first oq over cutoff score of 44
#feliz; life satisfaction (happiness score ??? question from work value survey)
#felizdif; change in happiness during treatment
#iat*; answer to each wai item
#iat; wai score. Can go from 12 to 84 score.  Greater score, better alliance.
#iatdif; change in wai during treatment
#eao*; eao*; answer to each self-concealment question 
#eao; self-concealment score. Fluctuates between 10 y 50. Greater score more self-concealment.
#high_eao; eao over percentile 50 (score of 30.49)
#high_eao2; eao 1 standard deviation above average (score 36.5)
#ep*; ep*; demographic information
#age; ep1; age
#male0; ep2 (0=male 1=female 2=transsexual); gender
#natio; ep3a (0=Chilean; 1=other - ep3b); nationality
#indig1; ep4a (0=no   1=yes); indigenous
#marria1; ep5 (0=single 1=married 2=divorced 3=widow); marital status
#sexua0; ep6 (0=heterosexual   1=homosexual  2=bisexual   3=doesn??t know); orientaci??n sexual
#job1; ep7a (1=yes); has paid job
#study1; ep7b (1=yes); studies
#unemp1; ep7c (1=yes; 1=marked); unemployed
#housew1; ep7d (1=yes); homemaker
#retir1; ep7e (1=yes); retired
#income; ep8 (0=less; 1=250; 2=500; 3=750; 4=1000; 5=1250; 6=1500; 7=1750; 8=more than 2000); family monthly income in 
 #Chilean pesos
#dincome; income in dollars
#nhome; ep9; number of persons at home- including patient
#relig; ep10 (0=cato; 1=evan; 2=judi; 3=musu; 4=morm; 5=espi;6=budi; 7=orto; 8=feba; 9=test; 10=ning; 11=otra); religion
#edu: ep11 (0=nunc; 1=b??si; 2=meci; 3=met??; 4=t??cn; 5=prof; 6=post; 7=mag??; 8=doct) education
#aedu: years of education
#thera; ep12 (0=never; 1=1; 2=2; 3=more than 2 times); number of times in previous psychotherapy
#psiq1; ep13 (0=no; 1=yes); seeing psychiatrist
#meds1; ep14 (0=no; 1=yes); taking psychiatric medication
#hosp1; ep15 (0=no; 1=yes); has been hospitalized for psychiatric reasons
#auge1; ep16 (0=no; 1=yes); therapy paid by government
#cost; ep17; how much pays for each session
#expect; ep18; number of sessions client thinks treatment will last
#asis; Percentage of assistance to therapy sessions.  Registro de asistencia semanal (0= Sin hora agendada; 1= Asiste; 
 #2= Suspende     3=No llega; 4=Psicomedica o terapeuta anula hora; 5=Asisti??, pero no se encuest??; 6=No hay informaci??n); 
 #porcentaje de asistencia que se calcula sumando las sesiones en que asiste (1 y 5) y divide por el n??mero total de 
 #sesiones a las que podr??a haber asistido (1, 2, 3 y 5).
#term; Type of therapy termination  (0=No information; 1=for therapeutic reasons; 2=abandoned treatment; 3=other --->)
#t_ep15: if therapist uses questionnaire (0=never; 1=sometimes; 2=frequently or always)
#t_ep15b: dummy for t_ep15 (0= never; 1=sometimes, frequently or always)
#t_feedb: therapists used our feedback during the study either always or occasionally (1=yes)
#t_ep16: Do you think collecting data by questionnaires can have a positive effect on treatment? (0=no; 1=yes)
#t_ep17: theoretical orientation (greater number, more they identify with): 17a (analytical)	17b (behavioral)	17c  
 #(cognitive)	17d (humanistic)	17e (systems theory)
#t_ep18: Do you consider your practice as integrative/eclectic? 0= not at all; 5=very much
#t_ep19: How many years have you practiced psychotherapy?
#t_ep20: Are you a licensed psychologist in Chile?

###### VARIABLES OF INTEREST ####

##Therapist???s demographic variables start with a t and are followed by the same name as patients??? variables.
#pacien; patient id
#terape; therapist id
#grupo; feedback group (0=no feedback; 1=raw oq; 2=raw wai; 3=raw oq and wai; 4=oq processed- Lambert??s report)  
#sesion; session in which data was collected
#oq*; answer to oq items
#oq; oq score (scored with formula provided in manual) ??? greater score is greater psychological 
#oqdif; difference between score of first and last oq of each client
#feliz; life satisfaction (happiness score ??? question from work value survey)
#felizdif; change in happiness during treatment
#iat; wai score. Can go from 12 to 84 score.  Greater score, better alliance.
#iatdif; change in wai during treatment
#ep*; ep*; demographic information
#age; ep1; age
#male0; ep2 (0=male 1=female 2=transsexual); gender
#marria1; ep5 (0=single 1=married 2=divorced 3=widow); marital status
#dincome; income in dollars
#aedu: years of education
#thera; ep12 (0=never; 1=1; 2=2; 3=more than 2 times); number of times in previous psychotherapy
#psiq1; ep13 (0=no; 1=yes); seeing psychiatrist
#meds1; ep14 (0=no; 1=yes); taking psychiatric medication
#hosp1; ep15 (0=no; 1=yes); has been hospitalized for psychiatric reasons
#t_ep19: How many years have you practiced psychotherapy?

###### INSTALL AND CALL PACKAGES #####
  
## Install packages to work on this project (not sure if all are needed)
  #call last the packages that will need most to deal with masking 
#install.packages(psych)
#install.packages(MASS)
#install.packages(Matrix)
#install.packages(mvtnorm)
#install.packages(sandwich)
#install.packages(chron)
#install.packages(Rcpp)
#install.packages(stringi)
#install.packages(data.table)

library(psych)
library(MASS)
#library(Matrix)
#library(mvtnorm)
#library(sandwich)
#library(chron)
#library(Rcpp)
#library(stringi)
#library(data.table)


###### READ DATA (run always) ####

## Set work directory
getwd()

# MacBook Air
setwd("/Users/Paula/Dropbox/1 PROYECTOS_INVESTIGACION/felicidad_psicoterapia")

# Open Data :)
mydata <- read.csv("happy_170512.csv", 
                   header = TRUE, sep = ",")

# Convert character variables to factors 
read.table

###### SUBSET DATA ####

# Select variable of interest

mydata <- subset(mydata, select=c("pacien", "terape", "grupo", "sesion", "oq1", "oq2", "oq3", "oq4", "oq5", "oq6", "oq7", 
                                  "oq8", "oq9", "oq10", "oq11", "oq12", "oq13", "oq14", "oq15", "oq16", "oq17", "oq18", 
                                  "oq19", "oq20", "oq21", "oq22", "oq23", "oq24", "oq25", "oq26", "oq27", "oq28", "oq29",
                                  "oq30", "oq", "oqdif", "feliz", "felizdif", "iat", "iatdif", "age", "male0", "marria1", 
                                  "dincome", "aedu", "thera", "psiq1", "meds1", "hosp1"))   
                                  
   
###### UNDERSTANDING DATA ####

str(mydata) # Summary of data
head(mydata,1) #See first row of data

# Describe all variables (n, mean, sd, median, trimmed (mean discarding high and low values))
library(psych)
describe(mydata)

# List factor levels for factor variables and frecuency of values
#table(mydata$Sexo)


# Describe each numeric Variable (mean, sd, median, trimmed, mad, min, max, range,
  #skew, kurtosis (posible outliers if greater than 3), se)
describe(mydata$oq)
describe(mydata$oqdif)
describe(mydata$feliz)
describe(mydata$felizdif)

# other descriptives
describe(mydata, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1,
         type=3,check=TRUE,fast=NULL,quant=NULL,IQR=FALSE)
describeData(mydata,head=4,tail=4)
#describeBy(mydata,mydata$Sexo,skew=FALSE,ranges=FALSE)

# Plot 1 variable
plot(mydata$oq)
plot(mydata$oqdif)
plot(mydata$feliz)
plot(mydata$felizdif)

# Histogram
#hist(mydata$Edad)#age
hist(mydata$oq)
hist(mydata$oqdif)
hist(mydata$feliz)
hist(mydata$felizdif)

# Plot 2 varibles 
plot(mydata$oq,mydata$feliz)
plot(mydata$oqdif,mydata$felizdif)


###### DATA ANALYSES ####

##### Multiple regressions all data ####
  # Traumatic load as predictor of mental health after a new traumatic event and after 1 month (time 1)

#### Multiple Regressions 

# Happiness
r1 <- lm(formula = feliz ~ oq1 + oq2 + oq3 + oq4 + oq5 + oq6 + oq7 + oq8 + oq9 + oq10 + oq11 + oq12 + oq13 + oq14 + oq15 + oq16 
         + oq17 + oq18 + oq19 + oq20 + oq21 + oq22 + oq23 + oq24 + oq25 + oq26 + oq27 + oq28 + oq29 + oq30, data = mydata)
r1
summary(r1)
#Result: What predicts happiness is: guilt, not getting along wel with friends and acquentinces, expecting something bad to happen,
 #feeling nervous, and feeling sad.

# Changes in happiness
r2 <- lm(formula = felizdif ~ oq1 + oq2 + oq3 + oq4 + oq5 + oq6 + oq7 + oq8 + oq9 + oq10 + oq11 + oq12 + oq13 + oq14 + oq15 + oq16 
         + oq17 + oq18 + oq19 + oq20 + oq21 + oq22 + oq23 + oq24 + oq25 + oq26 + oq27 + oq28 + oq29 + oq30, data = mydata)
r2
summary(r2)
#Result: What predicts changes in happiness is: feeling useless, worry about family problems, feeling lonely, being a happy person, 
 #drug consumption, stomach pain, work/study performance, feeling that somthing bad will happen

###### SUBSET DATA 2 ####

# Select data depending on changes in happiness 

mydata1 <- subset(mydata, felizdif =1, select=c("pacien", "terape", "grupo", "sesion", "oq1", "oq2", "oq3", "oq4", "oq5", "oq6", "oq7", 
"oq8", "oq9", "oq10", "oq11", "oq12", "oq13", "oq14", "oq15", "oq16", "oq17", "oq18", 
"oq19", "oq20", "oq21", "oq22", "oq23", "oq24", "oq25", "oq26", "oq27", "oq28", "oq29",
"oq30", "oq", "oqdif", "feliz", "felizdif", "iat", "iatdif", "age", "male0", "marria1", 
"dincome", "aedu", "thera", "psiq1", "meds1", "hosp1"))   

#mydata <- subset(mydata, BDI_t0 != is.na(1) | PCL_t0 != is.na(1), 
#select=c("Nreclutado","Sexo", "Edad", "CIDI_A8_basal",
"tipo_trauma", "PDEQ_t0", "PDI_t0", "TQ_t0",
"MSPSS_t0", "BDI_t0", "PCL_t0",  "BDI_t1", "PCL_t1", 
"CIDI_K_t1", "rama"))


#####End of Code####


