if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}

# load in data
ami <- read.csv('/Users/martinurbisaia/Documents/STAT149/Project/amidata.csv')
str(ami)

# turn specific columns to factors
factor.columns <- c("Patient", "DIAGNOSIS", 'SEX', 'DRG', 'DIED')
ami[, factor.columns] <- data.frame(apply(ami[factor.columns], 2, as.factor))


#######################################################################################
##############################    VISUALIZATIONS    ###################################
##############################                      ###################################
#######################################################################################

##### SINGLE VARIABLE #######
#############################

# los
ggplot(ami, aes(x = LOS)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "Hospital Length of Stay", x = "Hospital Length of Stay", y = " ") 

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


