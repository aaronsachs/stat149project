if (!require(aod)) {install.packages("aod"); require(aod)}
library(MASS)
library(stats)

source('Functions.R')

# load in data
path <- '/Users/jeanettejin/stat149project/'
ami <- load.data(path)
str(ami)
ami.drop.1000 <-  ami[ami$CHARGES > 1000,]
ami.drop.500 <- ami[ami$CHARGES > 500,]
ami.drop.300 <- ami[ami$CHARGES > 300,]




gamma <- glm(LOS ~  AGE  + DRG + SEX + DIAGNOSIS + log(CHARGES) + CHARGES.na,  family = Gamma(log), data = ami)


negbin <-  glm.nb(LOS ~  AGE  + DRG + SEX + DIAGNOSIS + log(CHARGES), data = ami)

poisson <- glm(LOS ~  AGE  + DRG + SEX + DIAGNOSIS + LOGCHARGES + CHARGES.na, family='poisson', data=ami)
poisson.drop.300 <- glm(LOS ~  AGE  + DRG + SEX + DIAGNOSIS + LOGCHARGES + CHARGES.na, family='poisson', data=ami.drop.300)
poisson.drop.500 <- glm(LOS ~  AGE  + DRG + SEX + DIAGNOSIS + LOGCHARGES + CHARGES.na, family='poisson', data=ami.drop.500)
poisson.drop.1000 <- glm(LOS ~  AGE  + DRG + SEX + DIAGNOSIS + LOGCHARGES + CHARGES.na, family='poisson', data=ami.drop.1000)

par(mfrow=c(2,2))
resid_plot(poisson, 'Poisson')
resid_plot(poisson.drop.300, 'Poisson 300')
resid_plot(poisson.drop.500, 'Poisson 500')
resid_plot(poisson.drop.1000, 'Poisson 1000')

resid_plot(gamma, 'gamma')

resid_plot(negbin, "negbin")


#######################################################################################
##############################          GAMMA         #################################
##############################                        #################################
#######################################################################################

# DRG:AGE, DRG:LOGCHARGES, DIAGNOSIS:LOGCHARGES, DIAGNOSES:SEX, 


#### with dropped data ########
model_str1 <- 'glm('
model_str2 <- ',family = Gamma(log), data = ami)'
predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'log(CHARGES + 2000)' , 'AGE', 'LOGCHARGES.na', 'DRG:AGE', 'DRG:LOGCHARGES', 'DIAGNOSIS:LOGCHARGES', 'DIAGNOSIS:SEX')
dependent.name <- "LOS"

# find the best model
gamma.bm.str <- best_model(model_str1, model_str2, dependent.name, predictors, data = ami, test = 'F')
gamma.bm <- glm(LOS ~ log(CHARGES + 2000) + DRG:AGE +  DIAGNOSIS:LOGCHARGES + DRG + DRG:LOGCHARGES + SEX + DIAGNOSIS + LOGCHARGES.na + DIAGNOSIS:SEX , family = Gamma(log), data = ami)

cooks <- cooks.distance(gamma.bm)



load.data <- function(path){
  print(paste(path, 'amidata.csv', sep = ''))
  ami <- read.csv(paste(path, 'amidata.csv', sep =''))
  
  # drop this value
  ami <- ami[ami$LOS != 0, ]
  
  ami$LOGCHARGES <- log(ami$CHARGES)
  
  # drop died
  ami$DIED <- NULL
  
  # convert
  ami <- na.convert.mean(ami)
  
  # turn specific columns to factors
  factor.columns <- c("Patient", "DIAGNOSIS", 'SEX', 'DRG', 'LOGCHARGES.na')
  ami[, factor.columns] <- data.frame(apply(ami[factor.columns], 2, as.factor))
  
  return(ami)
  
}

# INSERT PATH HERE
#path <- '/Users/jeanettejin/stat149project/'
ami <- load.data(path)

# this does not converge 
gamma.not.converge <- glm(LOS ~ log(CHARGES) + DRG + SEX + DIAGNOSIS + LOGCHARGES.na  , family = Gamma(log), data = ami)

gamma.converge <- glm(LOS ~ log(CHARGES + 2000) + DRG + SEX + DIAGNOSIS + LOGCHARGES.na  , family = Gamma(log), data = ami)





