if (!require(aod)) {install.packages("aod"); require(aod)}

source('Functions.R')

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

ami.gamma <- ami.gamma[complete.cases(ami.gamma), ]

str(ami.gamma)

model <- lm(LOS ~  DIAGNOSIS + SEX + DRG + DIED + scale(CHARGES) + AGE, data = ami.gamma)
table(ami.gamma$DRG,ami.gamma$DIED)
summary(model)

ami.gamma[ami.gamma$LOS == 0, ]$LOS <- 1


null <- glm(LOS ~ 1, family = Gamma(log), data = ami.gamma)
sex <- glm(LOS ~ SEX, family = Gamma(log), data = ami.gamma)
a <- anova(null, sex, test = 'F')
a


model_str1 <- 'glm('
model_str2 <- ',family = Gamma(log), data = ami.gamma)'
predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'DIED', 'scale(CHARGES)' , 'AGE')
dependent.name <- "LOS"

# find the best model
best.gamma <- best_model(model_str1, model_str2, dependent.name, predictors, data = ami.gamma, test = 'F')
best.gamma

best.gamma <- glm(LOS ~ 1 + scale(CHARGES) + DRG + AGE + DIAGNOSIS + SEX + DIED, family = Gamma(log), data = ami.gamma)
summary(best.gamma)


##### null 

