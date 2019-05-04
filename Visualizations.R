
source('/Users/jeanettejin/stat149project/Functions.R')

import('aod')
import('sjPlot')
import('sjmisc')
import('ggplot2')
import('gridExtra')
import('gridExtra')
import('effects')
import('dplyr')


# load in data
path <- '/Users/jeanettejin/stat149project/'
ami <- load.data(path)

ami$CHARGES <- NULL
ami$Patient <- NULL
ami$CHARGES.na <- NULL


#######################################################################################
##############################    VISUALIZATIONS    ###################################
##############################                      ###################################
#######################################################################################

##### SINGLE VARIABLE #######
#############################

# los
# very right skewed
ggplot(ami, aes(x = LOS)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "Hosital Length of Stay", x = "Hospital Length of Stay", y = " ") 

# charges (note missing values are dropped)
# also very right skewed
ggplot(ami, aes(x = CHARGES)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "Total Hospital Charges ($)", x = "Total Hospital Charges ($)", y = " ")

# age
# left skewed
# mass is at older ages
ggplot(ami, aes(x = AGE)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "Age", x = "Age", y = " ") 

# diagnosis
# there are more males
ggplot(ami, aes(x = DIAGNOSIS)) + geom_bar() + theme_gray() + 
  labs(title = "Diagnosis", x = "Diagnosis", y = " ") 

# sex
ggplot(ami, aes(x = SEX)) + geom_bar() + theme_gray() + 
  labs(title = "Sex", x = "Sex", y = " ") 

# drg
ggplot(ami, aes(x = DRG)) + geom_bar() + theme_gray() + 
  labs(title = "Diagnosis Related Group (DRG)", x = "Diagnosis Related Group (DRG)", y = " ") 



# FEATURE VS PREDICTOR VIS ##
#############################

# los
# very right skewed
los <- ggplot(ami, aes(x = LOS)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "LOS", x = "LOS", y = " ") 
los

# charges 
charges.los <- ggplot(ami, aes(x = LOGCHARGES, y = LOS)) + geom_point(alpha = 0.2) + theme_gray() + 
  labs(title = "LOS by LogCharges", x = "LogCharges", y = "LOS")
charges.los

# age
age.los <- ggplot(ami, aes(x = AGE, y = LOS)) + geom_point(alpha = 0.2) + theme_gray() + 
  labs(title = "LOS by Age", x = "Age", y = "LOS")
age.los

# diagnoses
diag.los <- ggplot(ami, aes(x = DIAGNOSIS, y = LOS)) + geom_boxplot(alpha = 0.2) + theme_gray() + 
  labs(title = "LOS by Diagnosis", x = "Diagnoses", y = "LOS")
diag.los

# sex
sex.los <- ggplot(ami, aes(x = SEX, y = LOS)) + geom_boxplot(alpha = 0.2) + theme_gray() + 
  labs(title = "LOS by Sex", x = "Sex", y = "LOS")
sex.los

# drg
drg.los <- ggplot(ami, aes(x = DRG, y = LOS)) + geom_boxplot(alpha = 0.2) + theme_gray() + 
  labs(title = "LOS by Drg", x = "DRG", y = "LOS")
drg.los


######### INTERACTIONS VIS ##
#############################

# interactions with charges
inter.c.diag <- lm(LOS ~ LOGCHARGES * DIAGNOSIS, data = ami)
inter.c.diag.plot <- plot_model(inter.c.diag, type = "int", terms = c('LOGCHARGES', 'DIAGNOSIS'),  title = c('LOGCHARGES DIAGNOSIS'))

inter.c.sex <- lm(LOS ~ LOGCHARGES * SEX, data = ami)
inter.c.sex.plot <- plot_model(inter.c.sex, type = "int", terms = c('LOGCHARGES', 'SEX'),  title = c('LOGCHARGES SEX'))

inter.c.drg <- lm(LOS ~ LOGCHARGES * DRG, data = ami)
inter.c.drg.plot <- plot_model(inter.c.drg, type = "int", terms = c('LOGCHARGES', 'DRG'),  title = c('LOGCHARGES DRG'))

inter.c.na <- lm(LOS ~ LOGCHARGES *LOGCHARGES.na, data = ami)
inter.c.na.plot <- plot_model(inter.c.na, type = "int", terms = c('LOGCHARGES', 'LOGCHARGES.na'), title = c('LOGCHARGES LOGCHARGES.na'))

# interactions with ages
inter.a.diag <- lm(LOS ~ AGE * DIAGNOSIS, data = ami)
inter.a.diag.plot <- plot_model(inter.a.diag, type = "int", terms = c('AGE', 'DIAGNOSIS'), title = c('AGE DIAGNOSIS'))

inter.a.sex <- lm(LOS ~ AGE * SEX, data = ami)
inter.a.sex.plot <- plot_model(inter.a.sex, type = "int", terms = c('AGE', 'SEX'), title = c('AGE SEX'))

inter.a.drg <- lm(LOS ~ AGE * DRG, data = ami)
inter.a.drg.plot <- plot_model(inter.a.drg, type = "int", terms = c('AGE', 'DRG'), title = c('AGE DRG'))

inter.a.na <- lm(LOS ~ AGE *LOGCHARGES.na, data = ami)
inter.a.na.plot <- plot_model(inter.a.na, type = "int", terms = c('AGE', "LOGCHARGES.na"), title = c('AGE LOGCHARGES.na'))

grid.arrange(inter.c.diag.plot, inter.c.sex.plot, inter.c.drg.plot, inter.c.na.plot, 
             inter.a.diag.plot, inter.a.sex.plot, inter.a.drg.plot, inter.a.na.plot , ncol=4)


# interactions with categorical
inter.diag.drg <- interact.plot('DIAGNOSIS', 'DRG')
inter.diag.sex <- interact.plot('DIAGNOSIS', 'SEX')
inter.diag.na <- interact.plot('DIAGNOSIS', 'LOGCHARGES.na')
inter.drg.sex <- interact.plot('DRG', 'SEX')
inter.drg.na <- interact.plot('DRG', 'LOGCHARGES.na')
inter.sex.na <- interact.plot('SEX', 'LOGCHARGES.na')

grid.arrange(inter.diag.drg, inter.diag.sex, inter.diag.na, inter.drg.sex, inter.drg.na, inter.sex.na, ncol=3)


###########
###########
##########

amin <-read.csv('/Users/jeanettejin/stat149project/amidata.csv')

##### VIZ 1


missing.data <- amin[!complete.cases(amin),]
notmissing.data <- amin[complete.cases(amin), ]




table(missing.data$) /699
table(notmissing.data$DRG) / 12145


not.missing <- ggplot(amin[complete.cases(amin), ], aes(x = LOS)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "LOS Not Including Missing Value Rows", x = "LOS", y = " ") 
missing <- ggplot(amin[!complete.cases(amin), ], aes(x = LOS)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "LOS of Missing Value Rows", x = "LOS", y = " ") 

grid.arrange(not.missing, missing, ncol=2)

not.missing <- ggplot(amin[complete.cases(amin), ], aes(x = AGE)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "LOS Not Including Missing Value Rows", x = "LOS", y = " ") 
missing <- ggplot(amin[!complete.cases(amin), ], aes(x = AGE)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "LOS of Missing Value Rows", x = "LOS", y = " ") 


grid.arrange(not.missing, missing, ncol=2)


##### VIZ 2)


dev.off()
charges <- ggplot(amin, aes(x = CHARGES)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "Charges", x = "Charges", y = " ") 

log.charges <- ggplot(amin, aes(x = log(CHARGES))) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "LOG(Charges)", x = "LOG(Charges)", y = " ") 

grid.arrange(charges, log.charges, ncol=2)


##### VIS 1)
grid.arrange(los, charges.los, age.los, diag.los, sex.los, drg.los, ncol=3)
