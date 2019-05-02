if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(MASS)) {install.packages("MASS"); require(MASS)}
if (!require(pscl)) {install.packages("pscl"); require(pscl)}

source("Functions.R")

# load in data
# Dataset:
#    Patients discharged from all NY state hospitals in 1993
#    who were admitted due to heart attack, and did not get
#    surgery
# Patient: patient ID from 1, ..., 12844
# DIAGNOSIS: 5 digit code (ICD-9 code) representing part of 
#            heart that was affected
# SEX: F or M
# DRG: Diagnosis related group 
#    121: had heart attack, cardio complications, survived
#    122: had heart attack, no cardio complications, survived
#    123: had heart attack, died
# DIED: 1 if died in hospital, 0 o/w
# CHARGES: total hospital charges
# LOS: length of stay in days since admittance
# AGE: age of patient in years

ami = load.data("")

#######################################################################################
##############################    VISUALIZATIONS    ###################################
##############################                      ###################################
#######################################################################################

##### SINGLE VARIABLE #######
#############################

# los
ggplot(ami, aes(x = LOS)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "Hosital Length of Stay", x = "Hospital Length of Stay", y = " ") 

# charges (note missing values are dropped)
ggplot(ami, aes(x = CHARGES)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "Total Hospital Charges ($)", x = "Total Hospital Charges ($)", y = " ")

# age
ggplot(ami, aes(x = AGE)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "Age", x = "Age", y = " ") 

# diagnosis
ggplot(ami, aes(x = DIAGNOSIS)) + geom_bar() + theme_gray() + 
  labs(title = "Diagnosis", x = "Diagnosis", y = " ") 

# sex
ggplot(ami, aes(x = SEX)) + geom_bar() + theme_gray() + 
  labs(title = "Sex", x = "Sex", y = " ") 

# drg
ggplot(ami, aes(x = DRG)) + geom_bar() + theme_gray() + 
  labs(title = "Diagnosis Related Group (DRG)", x = "Diagnosis Related Group (DRG)", y = " ") 

# died
ggplot(ami, aes(x = DIED)) + geom_bar() + theme_gray() + 
  labs(title = "Died", x = "Died", y = " ") 

# FEATURE VS PREDICTOR VIS ##
#############################

# charges (note missing values are dropped)
ggplot(ami, aes(x = CHARGES, y = LOS)) + geom_point(alpha = 0.2) + theme_gray() + 
  labs(title = "Total Hospital Charges and  Length of Stay", x = "Total Hospital Charges ($)", y = "Hospital Length of Stay")

# age
ggplot(ami, aes(x = AGE, y = LOS)) + geom_point(alpha = 0.2) + theme_gray() + 
  labs(title = "Age and  Length of Stay", x = "Age", y = "Hospital Length of Stay")

# diagnoses
ggplot(ami, aes(x = DIAGNOSIS, y = LOS)) + geom_boxplot(alpha = 0.2) + theme_gray() + 
  labs(title = "Length of Stay by Diagnosis", x = "Diagnoses", y = "Hospital Length of Stay")

# sex
ggplot(ami, aes(x = SEX, y = LOS)) + geom_boxplot(alpha = 0.2) + theme_gray() + 
  labs(title = "Length of Stay by Sex", x = "Sex", y = "Hospital Length of Stay")

# drg
ggplot(ami, aes(x = DRG, y = LOS)) + geom_boxplot(alpha = 0.2) + theme_gray() + 
  labs(title = "Length of Stay by Diagnosis Related Group (DRG)", x = "Diagnosis Related Group (DRG)", y = "Hospital Length of Stay")

# died
ggplot(ami, aes(x = DIED, y = LOS)) + geom_boxplot(alpha = 0.2) + theme_gray() + 
  labs(title = "Length of Stay by Death", x = "Death", y = "Hospital Length of Stay")


########################################################################
#                            MODEL SELECTION
########################################################################

# VIF tests, other multicollinearity tests
# Deal with missing values for each model type
# Test interaction terms for each

# Linear model is bad
# HOW TO USE ALL PREDICTOR VARS BESIDES ID COL?
lr = lm(LOS ~ . -Patient, data = ami)
summary(lr)
plot(lr)

# Log doesn't work, neither does sqrt (based on residual and QQ plots)
loglr = lm(log(LOS + 1) ~ . -Patient, data = ami)
sqrtlr = lm(sqrt(LOS) ~ . -Patient, data = ami)
plot(loglr)
plot(sqrtlr)

# Model mean length of stay as Poisson first
# - Why might this be a reasonable choice
# - Talk about drawbacks of this once it doesn't work

# There are 39 different lengths of stay across the dataset
# Might be reasonable to model each individidual's response as 1 hot vector
# Need to convert each LOS number into a one-hot vector for the cbind() in model fit

# Ordinal model for different LOS buckets, or for all 39 LOS's


# AARON
# Geometric regression
# Each day you have same prob of being discharged, once you are discharged, your
# LOS won't increase again (unlike Poisson, which allows for more successes after
# a failure occurs)

# Negative binomial regression (geometric is special case of this where
# a1 is 1 i.e. where r = 1 desired success).  Do hurdle and ZIP within this

# The num in "...Negative Binomial(num) family..." is alpha1

# We see that mean of LOS is lower than variance of LOS, which 
# suggests the response might be overdispersed.  NBin might be
# a good model.  (IS THAT GOOD REASONING?? This website says
# we should compare conditional means and vars instead of 
# overall means and vars):
# https://stats.idre.ucla.edu/stata/dae/negative-binomial-regression/
#
# We are using all vars for inference (except Patient)
# a1 = number of successes needed, each having success prob a2/(a2 + 1).
# Theta is what they call a1
# a1 is constant across individuals
#
# Don't include DIED because it is linear combination of DRG cols
#
# nb = glm.nb(
#   LOS ~ DIAGNOSIS + AGE, 
#   data = ami
# )

nb0 = glm.nb(LOS ~ 1, data = ami)
nb1 = glm.nb(LOS ~ DIAGNOSIS, data = ami) 
anova(nb0, nb1) # Better than nb0
nb2 = glm.nb(LOS ~ DIAGNOSIS + SEX, data = ami)
anova(nb1, nb2) # Better than nb1
nb3 = glm.nb(LOS ~ DIAGNOSIS + SEX + DRG, data = ami)
anova(nb2, nb3) # Better than nb2
nb4 = glm.nb(LOS ~ DIAGNOSIS + SEX + DRG + LOGCHARGES + LOGCHARGES.na, data = ami)
anova(nb3, nb4) # Better than nb3

nb5 = glm.nb(LOS ~ DIAGNOSIS + SEX + DRG + LOGCHARGES + LOGCHARGES.na + AGE, data = ami)
anova(nb4, nb5) # Better than nb4

# Replace DRG with DIED, compare log likelihoods, see which is bigger
# nb5 is best fit
# nb6 = glm.nb(LOS ~ DIAGNOSIS + SEX + DIED + CHARGES + CHARGES.na + AGE, data = ami)
# if (nb6$twologlik > nb5$twologlik) {
#   print("NB6 IS BETTER")
# } else {
#   print("NB5 IS BETTER")
# }

# Try Age * DRG because chances of death/complications after heart attack will depend on age
nb7 = glm.nb(LOS ~ DRG * AGE + SEX + DIAGNOSIS + LOGCHARGES + LOGCHARGES.na, data = ami)
anova(nb5, nb7) # nb7 is better

nb8 = glm.nb(LOS ~ DRG * AGE + SEX * DRG + DIAGNOSIS + LOGCHARGES + LOGCHARGES.na, data = ami)
anova(nb7, nb8) # nb8 is NOT better

# Nevermind, don't include age and charges interactions
# nb9 = glm.nb(LOS ~ DRG * AGE + LOGCHARGES * AGE + SEX + DIAGNOSIS + LOGCHARGES.na, data = ami)
# anova(nb7, nb9) # nb9 is better than nb7
# 
# nb10 = glm.nb(LOS ~ DRG * AGE + LOGCHARGES * AGE + LOGCHARGES.na * AGE + SEX + DIAGNOSIS, data = ami)
# anova(nb9, nb10) #nb10 better than nb9

nb11 = glm.nb(LOS ~ DRG * AGE + SEX + DIAGNOSIS * DRG + LOGCHARGES + LOGCHARGES.na, data = ami)
anova(nb7, nb11) # nb11 is NOT better

nb12 = glm.nb(LOS ~ DRG * AGE + SEX + DIAGNOSIS + LOGCHARGES * DRG + LOGCHARGES.na, data = ami)
anova(nb7, nb12) # nb12 better than nb7

nb13 = glm.nb(LOS ~ DRG * AGE + SEX + DIAGNOSIS + LOGCHARGES * DRG + LOGCHARGES.na * DRG, data = ami)
anova(nb12, nb13) # nb13 better than nb12

cooks_plot(nb13, "cow")
resid_plot(nb13, "cow")

# zeroinfl.control(maxit = 10000)
# Preds to right of "|" are for predicting logit(pi)
# Same for hurdle
zinb = zeroinfl(
  LOS - 1 ~ DRG * AGE + SEX + DIAGNOSIS + LOGCHARGES * DRG + LOGCHARGES.na * DRG | DIAGNOSIS + AGE + SEX, 
  dist = "negbin", 
  data = ami
)

plot(nb$fitted.values, nb$residuals)

nbhurdle = hurdle(
  LOS - 1 ~ DRG * AGE + SEX + DIAGNOSIS + LOGCHARGES * DRG + LOGCHARGES.na * DRG | DIAGNOSIS + AGE + SEX, 
  dist = "negbin", 
  data = ami
)




