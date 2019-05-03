if (!require(aod)) {install.packages("aod"); require(aod)}
if (!require(sjPlot)) {install.packages("sjPlot"); require(sjPlot)}
if (!require(sjmisc)) {install.packages("sjmisc"); require(sjmisc)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(gridExtra)) {install.packages("gridExtra"); require(gridExtra)}

source('/Users/jeanettejin/stat149project/Functions.R')


# load in data
path <- '/Users/jeanettejin/stat149project/'
ami <- load.data(path)



ami.drop <- ami[complete.cases(ami), ]
ami.convert <- na.convert.mean(ami)
summary(ami)
ami.convert$CHARGES.na <- as.factor(ami.convert$CHARGES.na)


hist(amin[complete.cases(amin), ]$LOS)
hist(ami[!complete.cases(ami), ]$LOS)


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

# died
# most don't die
ggplot(ami, aes(x = DIED)) + geom_bar() + theme_gray() + 
  labs(title = "Died", x = "Died", y = " ") 

# FEATURE VS PREDICTOR VIS ##
#############################

# los
# very right skewed
los <- ggplot(ami, aes(x = LOS)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "LOS", x = "LOS", y = " ") 

# charges (note missing values are dropped)
charges.los <- ggplot(ami, aes(x = log(CHARGES), y = LOS)) + geom_point(alpha = 0.2) + theme_gray() + 
  labs(title = "LOS by Charges", x = "Charges ($)", y = "LOS")
charges.los

# age
age.los <- ggplot(ami, aes(x = AGE, y = LOS)) + geom_point(alpha = 0.2) + theme_gray() + 
  labs(title = "LOS by Age", x = "Age", y = "LOS")
age

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




# died
ggplot(ami, aes(x = DIED, y = LOS)) + geom_boxplot(alpha = 0.2) + theme_gray() + 
  labs(title = "Length of Stay by Death", x = "Death", y = "Hospital Length of Stay")


######### INTERACTIONS VIS ##
#############################

# interactions with charges
inter.c.diag <- lm(LOS ~ CHARGES * DIAGNOSIS, data = ami)
plot_model(inter.c.diag, type = "int", terms = c('CHARGES', 'DIAGNOSIS'))

inter.c.sex <- lm(LOS ~ CHARGES * SEX, data = ami)
plot_model(inter.c.sex, type = "int", terms = c('CHARGES', 'SEX'))

inter.c.drg <- lm(LOS ~ CHARGES * DRG, data = ami)
plot_model(inter.c.drg, type = "int", terms = c('CHARGES', 'DRG'))

inter.c.died <- lm(LOS ~ CHARGES * DIED, data = ami)
plot_model(inter.c.died, type = "int", terms = c('CHARGES', 'DIED'))

# interactions with ages
# NOTE
inter.a.diag <- lm(LOS ~ AGE * DIAGNOSIS, data = ami)
plot_model(inter.a.diag, type = "int", terms = c('AGE', 'DIAGNOSIS'))

inter.a.sex <- lm(LOS ~ AGE * SEX, data = ami)
plot_model(inter.a.sex, type = "int", terms = c('AGE', 'SEX'))

inter.a.drg <- lm(LOS ~ AGE * DRG, data = ami)
plot_model(inter.a.drg, type = "int", terms = c('AGE', 'DRG'))

# NOTE
inter.a.died <- lm(LOS ~ AGE * DIED, data = ami)
plot_model(inter.a.died, type = "int", terms = c('AGE', 'DIED'))



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