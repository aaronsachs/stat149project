if (!require(aod)) {install.packages("aod"); require(aod)}
library(MASS)
library(stats)

source('~/Documents/STAT149/proyecto/lucas/Functions.R')

# load in data
path <- '~/Documents/STAT149/proyecto/lucas/'
ami <- load.data(path)
str(ami)

ami <- ami[ami$CHARGES > 300,]

#######################################################################################
##############################          POISSON       #################################
##############################                        #################################
#######################################################################################

# DRG:AGE, DRG:LOGCHARGES, DIAGNOSIS:LOGCHARGES, DIAGNOSES:SEX, 

#### with dropped data ########
model_str1 <- 'glm('
model_str2 <- ',family = poisson, data = ami)'
predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'log(CHARGES + 2000)' , 'AGE', 'LOGCHARGES.na', 'DRG:AGE', 'DRG:LOGCHARGES', 'DIAGNOSIS:LOGCHARGES', 'DIAGNOSIS:SEX')
dependent.name <- "LOS"

# find the best model
poisson.bm.str <- best_model(model_str1, model_str2, dependent.name, predictors, data = ami, test = 'F')
poisson.bm <- glm(LOS ~ log(CHARGES + 2000) + DRG:AGE +  DIAGNOSIS:LOGCHARGES + DRG + DRG:LOGCHARGES + SEX + DIAGNOSIS + LOGCHARGES.na + DIAGNOSIS:SEX , family = 'poisson', data = ami)

cooks <- cooks.distance(poisson.bm)



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



