if (!require(aod)) {install.packages("aod"); require(aod)}

# load in data
ami <- read.csv('amidata.csv')
str(ami)

# turn specific columns to factors
factor.columns <- c("Patient", "DIAGNOSIS", 'SEX', 'DRG', 'DIED')
ami[, factor.columns] <- data.frame(apply(ami[factor.columns], 2, as.factor))

summary(ami)




#######################################################################################
##############################    GAMMA         #################################
##############################                      ###################################
#######################################################################################

ami.gamma <- ami
ami.gamma[ami.gamma$LOS == 0, ]$LOS <- 1

rat.null = glm(time ~ 1, family = Gamma(log), data = rats)



