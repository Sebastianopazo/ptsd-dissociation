# TO DO ####
# Need to be able to do analyses with one observation for each client with variable oneperpacien
# For each group:
 # Mean and SD of OQ first session and OQ last session. (need variable last_oq, first_oq). 
 # Percentage of patients that have 1 (Red) for the variable oqacolor. This means they are not on track to realize substantial 
   #benefit from treatment. (need variable oqacolor)
 # Percentge of patients that have 1 in oq_oc_rc: Reliable change - Improved by at least 10 points on the OQ-30 but did not pass 
   #the cutoff between dysfunctional and functional populations (44). 
 # Percentge of patients that have 1 in oq_oc_csc: Clinically significant change - Improved by at least 10 points on the OQ-30 and 
   #passed the cutoff between dysfunctional and functional populations (44). 


###### NOTES ####
  
# This syntax was created by Paula Errazuriz starting on June 2017 to work with the data of FONDECYT Inicio 2011 to do some
 #follow up analyses asked for a review at JCCP.
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
#oqdif; difference between score of first and last oq of each client (session 1 - last session)
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
#felizdif; difference between score of first and last feliz of each client (session 1 - last session)
#vfelizdif; level of change in happines: group 1 (decrease), 2 (no change), 3 (increase)
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

###### CREATE VARIABLES #### 

# Create nsesion (number of session that we have data for in each client). Code created by Alejandro Zahler
#Generamos la tabla con frecuencias
t1 <- table(mydata$pacien)
t2 <- as.data.frame(t1)
#Generamos la funcion de lookup
#Match(mydata$pacien,t2$Var1)
nsesion <- t2$Freq[match(mydata$pacien,t2$Var1)]
mydata$nsesion <- nsesion
head(mydata)

###### ANALYSES FOR OQ TABLE ####

## Select variable of interest
mydata <- subset(mydata, select=c("pacien", "terape", "grupo", "nsesion", "sesion", "oq1", "oq2", "oq3", "oq4", "oq5", "oq6", "oq7", 
                                  "oq8", "oq9", "oq10", "oq11", "oq12", "oq13", "oq14", "oq15", "oq16", "oq17", "oq18", 
                                  "oq19", "oq20", "oq21", "oq22", "oq23", "oq24", "oq25", "oq26", "oq27", "oq28", "oq29",
                                  "oq30", "oq", "oqdif", "feliz", "felizdif", "iat", "iatdif", "age", "male0", "marria1", 
                                  "dincome", "aedu", "thera", "psiq1", "meds1", "hosp1"))   



### Analyses by Alejandro for table that includes for each condition the average and standard deviation for: a) OQ before treatment,
 #b) OQ after treatment, and c) Difference in OQ before and after treatment. The table also includes the percentage of patients 
 #in each condition that during treatment were: a) ???Not on Track???, b) deteriorated, c) had no change, d) had reliable change, and 
 #e) had clinically significant change.

#Filtramos las observaciones de interes
newdata <- mydata[which(mydata$oneperpacien == 1), ]
newdata$az <- 1

#Seleccionamos variables de interes
vars <- c("grupo","first_oq", "last_oq", "oqdif","n_track",
          "oq_oc_d", "oq_oc_nc","oq_oc_rc","oq_oc_csc", "az")

newdata <- newdata[vars]

g_mean  <- aggregate(newdata, by=list(newdata$grupo), FUN=mean)
g_sd    <- aggregate(newdata, by=list(newdata$grupo), FUN=sd)
g_sum   <-aggregate(newdata, by=list(newdata$grupo), FUN=sum)

# % ntrack
g_sum$p_ntrack   <- g_sum$n_track/g_sum$az
g_sum$p_oq_oc_d   <- g_sum$oq_oc_d/g_sum$az
g_sum$p_oq_oc_nc  <- g_sum$oq_oc_nc/g_sum$az
g_sum$p_oq_oc_rc <- g_sum$oq_oc_rc/g_sum$az
g_sum$p_oq_oc_csc <- g_sum$oq_oc_csc/g_sum$az
g_sum$mean_first_oq <- g_mean$first_oq
g_sum$mean_last_oq <- g_mean$last_oq
g_sum$mean_oq_dif <- g_mean$oqdif
g_sum$sd_first_oq <- g_sd$first_oq
g_sum$sd_last_oq <- g_sd$last_oq
g_sum$sd_oqdif <- g_sd$oqdif

write.csv(g_sum,"para_paula.csv")


#####End of Code####

