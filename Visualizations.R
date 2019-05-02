if (!require(aod)) {install.packages("aod"); require(aod)}
if (!require(sjPlot)) {install.packages("sjPlot"); require(sjPlot)}
if (!require(sjmisc)) {install.packages("sjmisc"); require(sjmisc)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}

# load in data
ami <- read.csv('amidata.csv')
str(ami)

# turn specific columns to factors
factor.columns <- c("Patient", "DIAGNOSIS", 'SEX', 'DRG', 'DIED')
ami[, factor.columns] <- data.frame(apply(ami[factor.columns], 2, as.factor))

summary(ami)

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