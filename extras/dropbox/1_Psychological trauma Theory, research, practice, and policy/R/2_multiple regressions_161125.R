####2: Multiple regressions ##################################################################################

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


### Multiple Regressions 3 and FINAL (with education - as regressions 1- and adding MSPSS_t0 
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


#####End of Code#############################################################################################

