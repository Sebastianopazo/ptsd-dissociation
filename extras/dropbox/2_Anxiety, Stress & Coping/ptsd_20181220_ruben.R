################################################################################
## Title: Dissociation and PSTD
## Original script by Paula Errazuriz (2016/04 - 2018/04). Worked with Paula
##  Cortes from CIGIDEN. Received statistical help from Alejandro, P Cumsille
##  and E Kromuller
## Modified script by Rubén Díaz 
## Date of creation: 20181212
## Last modified: 20181217
## Data by Rodrigo Figueroa.
################################################################################

#### 0 NOTES ####
## Project structure
# This R script is thought to be located within an RStudio project with the 
#   following folder structure:
#   - assets: Containing all the data and information about the project.
#   - output: Folder where the output from the script (plots and tables) will be
#           stored.
#   - raw_data: Containing the raw data base.
## Data
# Data was sent to RDiaz by PErrazuriz on 20181211 as a zip file "BB.DD.PFA.zip"
#   containing the database "1808131054_PAP" in xlsx, dat, sav, csv, and dta 
#   file formats.
# GNU PSPP was used to extract the data dictionary from "1808131054_PAP.sav".
# LibreOffice Calc was used to prepare "1808131054_PAP.xlsx" as follows:
#   - Substitute"#NULL!" and " " (empty cell) with NA
#   - Recode values from "t0_provider" to preserve confidentiality of subjects.
#       Names of providers were replaced by numeric codes.
#   - Export data to CSV

#### 1 CODE BOOK ####
### Variables of interest (selected for theoretical reasons) 

## Numeric variables
# t0_code (previously Nreclutado) col 1
# t0_age (previously Edad) - col 4
# t0_years_educ (previously CIDI_A8_basal): years of education - col 38
# t0_bdi (previously BDI_t0): Depression t0 (symptoms last 7 days) - col 16         
# t1_bdi_tot (previously BDI_t1): Depression t1 (symptoms last 7 days) - col 17     
# t0_pcl_tot (previously PCL_t0): PTSD score t0 (symptoms last month; 
#   inespecific, not realated to 1 event) - col 18            
# t1_pcl_tot (previously PCL_t1): PTSD score t1 (symptoms last month; 
#   inespecific, not realated to 1 event) - col 19       
# t0_pdeq (previously PDEQ_t0): dissociation during last traumatic event - col 21           
# t0_pdi (previously PDI_t0): peritraumatic distress during the last traumatic event col 22
# t0_tq (previously TQ_t0): traumatic load (number of traumatic events) - col 23   
# t0_mspss (previously MSPSS_t0): percived social support - col 20 

## Factor variables
# t0_sex (previously Sexo*): sex - col 5          
# t0_trauma_type (previously tipo_trauma*): trauma type - col 6      
# t0_tx (previously rama*): intervention (PAP; Psicoeducation) - col 8              
# t1_dx_tept (previously CIDI_K_t1*): presence of PTSD at t1? (TEPT- TEPT+) - col 15

#### 2 INSTALL AND CALL PACKAGES ####
## Install packages to work on this project
#install.packages("rprojroot")
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
#install.packages("polycor")

## Call last the packages that will need most to deal with "masking" 
library(rprojroot)
library(psych)
library(polycor)
library(MASS)
#library(Matrix)
#library(mvtnorm)
#library(sandwich)
#library(chron)
#library(Rcpp)
#library(stringi)
#library(data.table)
library(mediation)

## Custom functions
# Function to print numeric results (2 digit)
printNum <- function(x, digits = 2){
  formatC(x, digits = digits, format = "f")  
}

# Function to print p values (3 digits)
printP <- function(x){
  sub("0.",".", format.pval(x, eps = .01, digits = 2))
}

# Function to print statistics for descriptive tables
descTab <- function(x){
  data.frame(Mean = printNum(mean(x, na.rm = T), digits = 1),
             SD = printNum(sd(x, na.rm = T), digits = 1))}

descTabFac <- function(x, group = NULL){
  if (is.null(group)) {
    tab1 <- table(x)
    tab2 <- printNum(100*prop.table(tab1), digits = 1)
    d <- cbind(tab1, tab2)
    return(d)
  }
  tab1 <- table(x, group)
  tab2 <- printNum(100*prop.table(tab1, margin  = 2), digits = 1)
  d <- cbind(tab1, tab2)
  d <- (d[, c(1, 3, 2, 4)])
  return( rbind(c(rep(NA, 4), printP(chisq.test(x, group, simulate.p.value = T, 
                                                B = 10000)$'p.value')),
                cbind(d, "p-value" = NA)))
}

# Function to print baseline descriptive statistics
descTable <- function(data = mydata){
  list(
    wholesample = list(n = nrow(data),
                       "Age (years)" = descTab(data$t0_age),
                       "Education (years)" = descTab(data$t0_years_educ),
                       "Post-traumatic stress symptoms" = descTab(data$t0_pcl_tot),
                       "Depressive symptoms" = descTab(data$t0_bdi),
                       "Traumatic Load" = descTab(data$t0_tq),
                       "Perceived social support" = descTab(data$t0_mspss),
                       Gender = descTabFac(data$t0_sex),
                       "Civil status" = descTabFac(data$t0_civil_status),
                       "Type of trauma" = descTabFac(data$t0_trauma_type)
    ),
    byGroup = list(n = table(data$t0_tx),
                   "numeric" = rbind(
                     "Age (years)" = 
                       data.frame(lapply(c("PE", "PFA"), 
                                         function(x){with(data[data$t0_tx == x,],
                                                          (descTab(t0_age)))}),
                                  "p-value" = printP(t.test(data$t0_age ~ 
                                                              data$t0_tx)$'p.value')),
                     "Education (years)" = 
                       data.frame(lapply(c("PE", "PFA"), 
                                         function(x){with(data[data$t0_tx == x,],
                                                          (descTab(t0_years_educ)))}),
                                  "p-value" = printP(t.test(data$t0_years_educ ~ 
                                                              data$t0_tx)$'p.value')),
                     "Post-traumatic stress symptoms" = 
                       data.frame(lapply(c("PE", "PFA"), 
                                         function(x){with(data[data$t0_tx == x,],
                                                          (descTab(t0_pcl_tot)))}),
                                  "p-value" = printP(t.test(data$t0_pcl_tot ~ 
                                                              data$t0_tx)$'p.value')),
                     "Depressive symptoms" = 
                       data.frame(lapply(c("PE", "PFA"), 
                                         function(x){with(data[data$t0_tx == x,],
                                                          (descTab(t0_bdi)))}),
                                  "p-value" = printP(t.test(data$t0_bdi ~ 
                                                              data$t0_tx)$'p.value')),
                     "Traumatic Load" = 
                       data.frame(lapply(c("PE", "PFA"), 
                                         function(x){with(data[data$t0_tx == x,],
                                                          (descTab(t0_tq)))}),
                                  "p-value" = printP(t.test(data$t0_tq ~ 
                                                              data$t0_tx)$'p.value')),
                     "Perceived social support" = 
                       data.frame(lapply(c("PE", "PFA"), 
                                         function(x){with(data[data$t0_tx == x,],
                                                          (descTab(t0_mspss)))}),
                                  "p-value" = printP(t.test(data$t0_mspss ~ 
                                                              data$t0_tx)$'p.value'))),
                   "factor" = list(Gender = descTabFac(data$t0_sex, data$t0_tx),
                                   "Civil status" = descTabFac(data$t0_civil_status, 
                                                               data$t0_tx),
                                   "Type of trauma" = descTabFac(data$t0_trauma_type, 
                                                                 data$t0_tx))))
}


# Function to detect and replace univariate outliers
detectOutlier <- function(var, cutoffPoint = 3.29){
  ztemp <- scale(var, center = T, scale  = T) #Transform variable to Z score
  indOut <- which(abs(ztemp) > cutoffPoint) # Index of Outliers
  indNoOut <- which(abs(ztemp) <= cutoffPoint) # Index of Not Outliers
  results = list("Index of outliers" = indOut, "n outliers" = length(indOut))
  # if outliers are detected, replace values with highest (or lowest) value 
  #   before outlier
  if (length(indOut) > 0 ) {
    maxD <- max(var[indNoOut])
    minD <- min(var[indNoOut])
    repOut <- var
    repOut[which(repOut > maxD)] <- maxD + 1
    repOut[which(repOut < minD)] <- minD - 1 
    results$'Variable without outliers' = repOut
  }
  return(results)
}


## Set random seed (for reproducibility purposes)
set.seed(565119902)

#### 3 READ DATA (run always) ####
## Set working directory
setwd(find_rstudio_root_file())

## Read data
mydata <- read.csv("raw_data/data.csv", header = TRUE, sep = ",")

## Prepare data (set appropiate data type, assign labels where needed)

mydata[c(1, 6)] <- lapply(mydata[c(1, 6)], factor)
mydata$t0_code <- factor(mydata$t0_code)
mydata$t0_recr_wave <- factor(mydata$t0_recr_wave, levels = 1:2, 
                              labels = c("first", "second"))
mydata$t0_tx <- factor(mydata$t0_tx, levels = 0:1, labels = c("PE", "PFA"))
mydata$t0_treat_rec <- factor(mydata$t0_treat_rec, levels = 0:2,
                              labels = c("PE", "PFA", 
                                         "Retract after randomization"))
mydata$group <- factor(mydata$group, levels = 0:3,
                       labels = c("PE_W1", "PFA_W1", "PE_W2", "PFA_W2"))
mydata$t0_hospital <- factor(mydata$t0_hospital, levels = 1:5,
                             labels = c("Sotero", "Barros Luco", 
                                        "Padre Hurtado", "ACHS", "UC"))
mydata$t0_recuit_date <- as.POSIXct(mydata$t0_recuit_date[1], 
                                    format = "%d/%m/%y")
mydata$t0_trauma_type <- factor(mydata$t0_trauma_type, levels = 1:10,
                                labels = c("Motor vehicle accident", 
                                           "Other accidents",
                                           "Severe medical condition",
                                           "Witness",
                                           "Fire",
                                           "Close relative",
                                           "Disaster",
                                           "Sexual offense",
                                           "Assault",
                                           "Other trauma"))
mydata$t0_sex <- factor(mydata$t0_sex, levels = 1:2, labels = c("Male", 
                                                                "Female"))
mydata$t0_perspective <- factor(mydata$t0_perspective, levels = 1:2,
                                labels = c("First person", "Witness"))
mydata$t0_who_acquaint <- factor(mydata$t0_who_acquaint, levels = 1:5,
                                 labels = c("Wife or husband", 
                                            "Parent or children",
                                            "Sibling",
                                            "Friend",
                                            "Other"))
mydata$t0_time_slept <- ordered(mydata$t0_time_slept, levels = 1:5,
                                labels = c("Less than 30 min",
                                           "30 min - 2 hours",
                                           "2-6 hours",
                                           "6-8 hours",
                                           "More than 8 hours"))
mydata$t0_civil_status <- factor(mydata$t0_civil_status, levels = 1:5,
                                 labels = c("Married or civil union",
                                            "Widowed",
                                            "Separated",
                                            "Divorced",
                                            "Single"))
mydata$t0_part_time <- ordered(mydata$t0_part_time, levels = c(2, 1), 
                               labels = c("Part-time", "Full-time"))
mydata$t1_fol_pcl_dum <- factor(mydata$t1_fol_pcl_dum, levels = 0:3,
                                labels = c("Perdido PFA",
                                           "Seguido PFA",
                                           "Perdido PE",
                                           "Seguido PE"))
mydata[c(68, 70, 74, 77, 90, 92, 95:97, 109, 111:114)] <- 
  lapply(mydata[c(68, 70, 74, 77, 90, 92, 95:97, 109, 111:114)], factor,
         levels = 1:7, labels = c("Face-to-face follow-up",
                                  "Phone follow-up",
                                  "No show",
                                  "Untraceable",
                                  "Quit",
                                  "Died",
                                  "Unplanned"))
mydata[c(10, 20, 22, 25, 26, 38, 41, 44, 58, 61:66, 75, 82:89, 101:108)] <- 
  lapply(mydata[c(10, 20, 22, 25, 26, 38, 41, 44, 58, 61:66, 75, 82:89, 
                  101:108)], factor, levels = 0:1, labels = c("No", "Yes"))
mydata[c(27, 28, 30, 35, 45, 46)] <- lapply(mydata[c(27, 28, 30, 35, 45, 46)], 
                                            factor, levels = c(1, 5), 
                                            labels = c("No", "Yes"))

#### 4 UNDERSTANDING DATA ####

str(mydata) # Summary of data
head(mydata,1) # See first row of data

# Describe all variables (n, mean, sd, median, trimmed (mean discarding high and low values))
describe(mydata)

# List factor levels for factor variables and frecuency of values
table(mydata$t0_sex)
table(mydata$t0_trauma_type)
table(mydata$t0_tx)
table(mydata$t1_dx_tept)

# Describe each numeric Variable (mean, sd, median, trimmed, mad, min, max, 
#   range, skew, kurtosis (posible outliers if greater than 3), se)
describe(mydata$t0_age)#age
describe(mydata$t0_bdi)#Depression t0
describe(mydata$t1_bdi_tot)#Depression t1
describe(mydata$t0_pcl_tot)#PTSD score t0
describe(mydata$t1_pcl_tot)# PTSD score t1
describe(mydata$t0_years_educ)#years of education
describe(mydata$t0_mspss)#perceived social support?
describe(mydata$t0_pdeq)#dissociation after trauma score
describe(mydata$t0_pdi)#traumatic stress after trauma
describe(mydata$t0_tq)#trauma history (number of traumatic events?)

# other descriptives
describe(mydata, na.rm = TRUE, interp = FALSE, skew = TRUE, ranges = TRUE,
         trim = .1, type = 3, check = TRUE, fast = NULL, quant = NULL,
         IQR = FALSE, omit = T)
describeData(mydata, head = 4, tail = 4)
describeBy(mydata, mydata$sex, skew = FALSE, ranges = FALSE)

# Plot 1 variable
plot(mydata$t0_age)#age
plot(mydata$t0_bdi)#Depression t0  
plot(mydata$t0_pcl_tot)#PTSD score t0  
plot(mydata$t1_bdi_tot)#Depression t1 
plot(mydata$t1_pcl_tot)# PTSD score t1  
plot(mydata$t0_years_educ)#years of education
plot(mydata$t0_mspss)#percived social support?
plot(mydata$t0_pdeq)#dissociation after trauma score  
plot(mydata$t0_pdi)#traumatic stress after trauma  
plot(mydata$t0_tq)#trauma history (number of traumatic events?)  

# Histogram
multi.hist(mydata[c("t0_age", "t0_bdi", "t0_pcl_tot", "t1_bdi_tot", 
                    "t1_pcl_tot", "t0_years_educ", "t0_mspss", "t0_pdeq", 
                    "t0_pdi", "t0_tq")], main = c("Age", "Depression t0", "PTSD score t0",
                                         "Depression t1", "PTSD score t1", 
                                         "Years of education", "Perceived social support",
                                         "Dissociation aftertrauma score",
                                         "Traumatic stress after trauma", 
                                         "Trauma history"))

# Plot 2 variables (each independent variable with the dependent variable 
#   (t0_tq  trauma history (number of traumatic events?)))
# RD: Verify that t0_t1 is the dependent variable. In the manuscript it seems 
#   that the dependent variables are "Peritraumatic dissociation" and "PTSD 
#   symptoms".

pairs.panels(mydata[c("t0_tq", "t0_age", "t0_bdi", "t0_pcl_tot", "t1_bdi_tot", 
                      "t1_pcl_tot", "t0_years_educ", "t0_mspss", "t0_pdeq", 
                      "t0_pdi")])

#### 5 TABLES ####
# Table W1+2T0: Baseline characteristics of the sample.
tables <- list()
tables$'W1+W2T0' <- descTable(mydata)
tables$'W1+W2T0'

# Table W1+2pcl_attrition1m: First and second trials merged. Selective attrition
#   analysis: baseline characteristics of those who answered the PCL-S and BDI-II on T1.
tables$'W1+2pcl_attrition1m' <- descTable(subset(mydata, t1_pcl_tot != is.na(1) & 
                                                   t1_bdi_tot != is.na(1)))
tables$'W1+2pcl_attrition1m'

# Table W1+2_subsample: First and second trials merged. Selective attition analysis:
#   baseline characteristics of those who answered the TQ and the PDEQ on T0 , and the PCL on T1
tables$'W1+2_attrition' <- descTable(subset(mydata, t0_tq != is.na(1) &  t0_pdeq != is.na(1) & 
                                              t1_pcl_tot != is.na(1)))
tables$'W1+2_attrition'

#### 6 SUBSET DATA ####

# Select code book variables & data from patients who have t0 data
#mydata <- subset(mydata, BDI_t0 != is.na(1) | PCL_t0 != is.na(1), 
                 #select=c("Nreclutado","Sexo", "Edad", "CIDI_A8_basal",
#                          "tipo_trauma", "PDEQ_t0", "PDI_t0", "TQ_t0",
#                          "MSPSS_t0", "BDI_t0", "PCL_t0",  "BDI_t1", "PCL_t1", 
#                          "CIDI_K_t1", "rama"))

# Select code book variables & data from patients who have t0 and t1 data
# Data for ptsd paper w Rodrigo 170124
mydataSub <- subset(mydata, t0_tq != is.na(1) &  t0_pdeq != is.na(1) & 
                   t1_pcl_tot != is.na(1), 
                 select=c("t0_code","t0_sex", "t0_age", "t0_years_educ",
                        "t0_trauma_type", "t0_pdeq", "t0_pdi", "t0_tq",
                        "t0_mspss", "t0_bdi", "t0_pcl_tot", "t1_bdi_tot", 
                        "t1_pcl_tot", "t1_dx_tept", "t0_tx"))




#### 7 PREPARING DATA ####
#### 7.1 Univariate Outliers ####

# RD: Check "Leys C et al. (2013). Detecting outliers. Journal of Experimental 
#   Social Psychology. 2013 Jul;49(4):764–6." for a more robust criteria.

# Deciding cut-off point of z=3.29 for outliers
# Outliers, with z-scores larger than 3.29, will be transformed by replacing 
#   their raw score value with the next highest or lowest value as recommended 
#   by Tabachnick and Fidell (2001). 

# Check for univariate outliers in numeric variables
sapply(mydataSub[sapply(mydataSub, is.numeric)], 
       function(x){unlist(detectOutlier(x)[2])})

# Replace outliers where detected
mydataSub$t0_years_educ <- detectOutlier(mydataSub$t0_years_educ)[[3]]
mydataSub$t0_tq <- detectOutlier(mydataSub$t0_tq)[[3]]

# Check for outliers again
detectOutlier(mydataSub$t0_years_educ)[[2]]
detectOutlier(mydataSub$t0_tq)[[2]]
mydataSub$t0_tq[72] <- 9
detectOutlier(mydataSub$t0_tq)[[2]]
sapply(mydataSub[c("t0_years_educ", "t0_tq")], 
       function(x){unlist(detectOutlier(x)[2])})

#### 8 DATA ANALYSES ####
#### 8.1 T-test ####

# Run independent t-test to assess significant differences in numeric variables
#   when comparing participants by gender and by group allocation

# independent 2-group t-test
t.test(mydataSub$t0_pdeq ~ mydataSub$t0_sex)
t.test(mydataSub$t0_pdeq ~ mydataSub$t0_tx)
t.test(mydataSub$t1_pcl_tot ~ mydataSub$t0_sex)
t.test(mydataSub$t1_pcl_tot ~ mydataSub$t0_tx)

hetcor(mydataSub[2:15])

#### 8.2 Multiple regressions ####

# Traumatic load as predictor of mental health after a new traumatic event 
#   and after 1 month (time 1).
# Controlling for age, sex, years of education, and perceived social support.
# In the case of dependent variables measured at time 1 will control for 
#   intervention (t0_tx)

# Depression t0
r21 <- lm(formula = t0_bdi ~ t0_tq + t0_age + t0_sex + t0_years_educ + 
            t0_mspss, data = mydataSub)
r21
summary(r21)

# Depression t1
r22 <- lm(formula = t1_bdi_tot ~ t0_tq + t0_age + t0_sex + t0_years_educ + 
            t0_mspss + t0_tx, data = mydataSub)
r22
summary(r22)

# PTSD score t0 
r23 <- lm(formula = t0_pcl_tot ~ t0_tq + t0_age + t0_sex + t0_years_educ + 
            t0_mspss, data = mydataSub)
r23
summary(r23)

# PTSD score t1   
r24 <- lm(formula = t1_pcl_tot ~ t0_tq + t0_age + t0_sex + t0_years_educ + 
            t0_mspss, data = mydataSub)
r24
summary(r24)

#dissociation after trauma t0
r25 <- lm(formula = t0_pdeq ~ t0_tq + t0_age + t0_sex + t0_years_educ + 
            t0_mspss, data = mydataSub)
r25
summary(r25)

# traumatic stress after trauma t0
r26 <- lm(formula = t0_pdi ~ t0_tq + t0_age + t0_sex + t0_years_educ + 
            t0_mspss, data = mydataSub)
r26
summary(r26)   

#### 8.3 Other Multiple Regressions ####
# Adds to multiple regressions III dissociation and traumatic stress as 
#   predictor variables for PTSD and Depression T1
# RD: Consider adding values of PSTD and Depression at time T0 as predictors or 
#   using change scores instead, to control for auto-correlation.

# Depression t1
r32 <- lm(formula = t1_bdi_tot ~ t0_tq + t0_age + t0_sex + t0_years_educ + 
            t0_mspss + t0_tx + t0_pdeq + t0_pdi, data = mydataSub)
r32
summary(r32)


# PTSD score t1   
r34 <- lm(formula = t1_pcl_tot ~ t0_tq + t0_age + t0_sex + t0_years_educ + 
            t0_mspss + t0_tx + t0_pdeq + t0_pdi, data = mydataSub)
r34
summary(r34)

#### 8.4 Multiple regressions for paper ####

# Table 1 of paper: Predicting peritraumatic dissociation
r41 <- lm(formula = t0_pdeq ~ t0_tq + t0_sex + t0_age + t0_years_educ + 
            t0_mspss, data = mydata)
r41
summary(r41)

# Table 1 of paper: Predicting PTSD symptomatology after one month
r42 <- lm(formula = t1_pcl_tot ~ t0_sex + t0_age + t0_years_educ + t0_tx +
            t0_tq + t0_mspss + t0_pdi + t0_pdeq, data = mydataSub)
r42
summary(r42)


#### 8.4 Mediational Models ####
# RESULTS: none of the mediations were significant
## Mediational models:
# I: TQ_t0 (traumatic load) -> PDEQ_t0 (dissociation) -> 
#       PCL_t1 (PTSD 1 month after)  
# II: TQ_t0 (traumatic load) -> PDEQ_t0 (dissociation) -> 
#       BDI_t1 (depression 1 month after)
# III: TQ_t0 (traumatic load) -> PDI_t0 (peritraumatic distress) -> 
#       PCL_t1 (PTSD 1 month after)  
# IV: TQ_t0 (traumatic load) -> PDI_t0 (peritraumatic distress) -> 
#       BDI_t1 (depression 1 month after)

##Regresions with variables in mediational models ####
# Dissociation predicts PTSD
a1 <- lm(formula = t1_pcl_tot ~ t0_pdeq, data = mydataSub)
a1
summary(a1)

# Dissociation predicts depression
a2 <- lm(formula = t1_bdi_tot ~ t0_pdeq, data = mydataSub)
a2
summary(a2)

# Peritraumatic distress predicts PTSD
a3 <- lm(formula = t1_pcl_tot ~ t0_pdi, data = mydataSub)
a3
summary(a3)

# Peritraumatic distress predicts depression
a4 <- lm(formula = t1_bdi_tot ~ t0_pdi, data = mydataSub)
a4
summary(a4)

# Traumatic load predicts dissociation 
a5 <- lm(formula = t0_pdeq ~ t0_tq, data = mydataSub)
a5
summary(a5)

# Traumatic load predicts peritraumatic distress 
a6 <- lm(formula = t0_pdi ~ t0_tq, data = mydataSub)
a6
summary(a6)

## Regressions adding control variables: ####
# Controlling for: Age, Sex, Perceived social support (MSPSS_t0), Intervention
#   (t0_tx)

# Dissociation predicts PTSD
a11 <- lm(formula = t1_pcl_tot ~ t0_pdeq + t0_age + t0_sex + t0_years_educ 
          + t0_tx + t0_mspss, data = mydataSub)
a11
summary(a11)

# Dissociation predicts depression
a12 <- lm(formula = t1_bdi_tot ~  t0_pdeq + t0_age + t0_sex + t0_years_educ 
          + t0_tx + t0_mspss, data = mydataSub)
a12
summary(a12)

# Peritraumatic distress predicts PTSD
a13 <- lm(formula = t1_pcl_tot ~ t0_pdi + t0_age + t0_sex + t0_years_educ 
          + t0_tx + t0_mspss, data = mydataSub)
a13
summary(a13)

# Peritraumatic distress predicts depression
a14 <- lm(formula = t1_bdi_tot ~ t0_pdi + t0_age + t0_sex + t0_years_educ 
          + t0_tx + t0_mspss, data = mydataSub)
a14
summary(a14)

#Traumatic load predicts dissociation 
a15 <- lm(formula = t0_pdeq ~ t0_tq + t0_age + t0_sex + t0_years_educ +
            t0_mspss, data = mydataSub)
a15
summary(a15)

#Traumatic load predicts peritraumatic distress 
a16 <- lm(formula = t0_pdi ~ t0_tq + t0_age + t0_sex + t0_years_educ +
            t0_mspss, data = mydataSub)
a16
summary(a16)

#### Mediational Model I ####
  #I: t0_tq (traumatic load) -> t0_pdeq (dissociation) -> t1_pcl_tot (PTSD 1 month after) 

# Select data using subset function (to drop cases with missing values)
data_m1 <- subset(mydataSub, t0_tq != is.na(1) & t0_pdeq != is.na(1) & 
                    t1_pcl_tot != is.na(1), select=c("t0_code","t0_tq", 
                                                     "t0_pdeq", "t1_pcl_tot"))


med.fit <- lm(data_m1$t0_pdeq ~ data_m1$t0_tq) # Independent variable (X) predicts mediator (M)
summary(med.fit)
out.fit <- glm(data_m1$t1_pcl_tot ~ data_m1$t0_tq+data_m1$t0_pdeq) # Independent variable (X) and mediator (M), predict dependent variable (Y)
summary(out.fit)
med.out <- mediate(med.fit, out.fit, treat="data_m1$t0_tq", sims = 10000, 
                 mediator="data_m1$t0_pdeq", robustSE = TRUE,  dropobs = TRUE) # Estimate mediation coefficients using 10 000 bootstrap samples
summary(med.out) 

#### Mediational Model II ####
  # II: t0_tq (traumatic load) -> t0_pdeq (dissociation) -> t1_bdi_tot (depression 1 month after)

# Select data using subset function (to drop cases with missing values)
data_m2 <- subset(mydataSub, t0_tq != is.na(1) & t0_pdeq != is.na(1) & 
                    t1_bdi_tot != is.na(1), 
                  select=c("t0_code","t0_tq", "t0_pdeq", "t1_bdi_tot"))

med.fit <- lm(data_m2$t0_pdeq ~ data_m2$t0_tq) # Independent variable (X) predicts mediator (M)
summary(med.fit)
out.fit <- glm(data_m2$t1_bdi_tot ~ data_m2$t0_tq + data_m2$t0_pdeq)  # Independent variable (X) and mediator (M), predict dependent variable (Y)
summary(out.fit)
med.out <- mediate(med.fit, out.fit, treat = "data_m2$t0_tq", sims = 10000, 
                   mediator = "data_m2$t0_pdeq", robustSE = TRUE, dropobs = TRUE) # Estimate mediation coefficients using 10 000 bootstrap samples
summary(med.out) 

#### Mediational Model III ####
  # III: t0_tq (traumatic load) -> t0_pdi (peritraumatic distress) -> t1_pcl_tot (PTSD 1 month after)  

data_m3 <- subset(mydataSub, t0_tq != is.na(1) & t0_pdi != is.na(1) & 
                    t1_pcl_tot != is.na(1), 
                  select=c("t0_code","t0_tq", "t0_pdi", "t1_pcl_tot"))

med.fit<-lm(data_m3$t0_pdi ~ data_m3$t0_tq) # Independent variable (X) predicts mediator (M)
summary(med.fit)
out.fit<-glm(data_m3$t1_pcl_tot ~ data_m3$t0_tq + data_m3$t0_pdi) # Independent variable (X) and mediator (M), predict dependent variable (Y)
summary(out.fit)
med.out<-mediate(med.fit, out.fit, treat = "data_m3$t0_tq", sims = 10000, 
                 mediator = "data_m3$t0_pdi", robustSE = TRUE, dropobs = TRUE) # Estimate mediation coefficients using 10 000 bootstrap samples
summary(med.out) 

#### Mediational Model IV ####

  # IV: t0_tq (traumatic load) -> t0_pdi (peritraumatic distress) -> t1_bdi_tot (depression 1 month after)

data_m4 <- subset(mydataSub, t0_tq != is.na(1) & t0_pdi != is.na(1) & t1_bdi_tot != is.na(1), 
                  select=c("t0_code","t0_tq", "t0_pdi", "t1_bdi_tot"))

med.fit<-lm(data_m4$t0_pdi ~ data_m4$t0_tq) # Independent variable (X) predicts mediator (M)
summary(med.fit)
out.fit<-glm(data_m4$t1_bdi_tot ~ data_m4$t0_tq + data_m4$t0_pdi) # Independent variable (X) and mediator (M), predict dependent variable (Y)
summary(out.fit)
med.out<-mediate(med.fit, out.fit, treat = "data_m4$t0_tq", sims = 10000, 
                 mediator = "data_m4$t0_pdi", robustSE = TRUE, dropobs = TRUE) # Estimate mediation coefficients using 10 000 bootstrap samples
summary(med.out) 

#### Other Mediational Models ####
#V: t0_years_educ (education) -> t0_pdeq (dissociation) -> t1_pcl_tot (PTSD 1 month after) 

# Select data using subset function (to drop cases with missing values)
data_m5 <- subset(mydataSub, t0_years_educ != is.na(1) & t0_pdeq != is.na(1) & 
                    t1_pcl_tot != is.na(1), 
                  select=c("t0_code","t0_years_educ", "t0_pdeq", "t1_pcl_tot"))

med.fit<-lm(data_m5$t0_pdeq ~ data_m5$t0_years_educ) # Independent variable (X) predicts mediator (M)
summary(med.fit)
out.fit<-glm(data_m5$t1_pcl_tot ~ data_m5$t0_pdeq + data_m5$t0_years_educ) # Independent variable (X) and mediator (M), predict dependent variable (Y)
summary(out.fit)
med.out<-mediate(med.fit, out.fit, treat = "data_m5$t0_years_educ", sims = 10000, 
                 mediator = "data_m5$t0_pdeq", robustSE = TRUE, dropobs = TRUE) # Estimate mediation coefficients using 10 000 bootstrap samples
summary(med.out) 

#####End of Code####


