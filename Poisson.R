if (!require(aod)) {install.packages("aod"); require(aod)}
library(MASS)
library(stats)
library(car)
library(pscl)

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
model_str2 <- ',family = \'poisson\', data = ami)'
predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'log(CHARGES + 2000)' , 'AGE', 'LOGCHARGES.na')
                #, 'DRG:AGE', 'DRG:LOGCHARGES', 'DIAGNOSIS:LOGCHARGES', 'DIAGNOSIS:SEX', 'DIAGNOSIS:DRG')
dependent.name <- "LOS"

# find the best model
poisson.bm.str <- best_model(model_str1, model_str2, dependent.name, predictors, data = ami, test = 'Chisq')
poisson.bm <- glm(LOS ~ 1 + log(CHARGES + 2000) + DRG + AGE + DIAGNOSIS + SEX + 
                  LOGCHARGES.na, family = 'poisson', data = ami)

cooks <- cooks.distance(poisson.bm)
summary(ami
        )

vif(poisson.bm)

#######################################################################################
##############################          POISSON       #################################
##############################           HURDLE        ################################
#######################################################################################

model_str1 <- 'hurdle('
model_str2 <- ',dist = \'poisson\', data = ami)'
predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'log(CHARGES + 2000)' , 'AGE', 'LOGCHARGES.na', 'DRG:AGE', 'DRG:LOGCHARGES', 'DIAGNOSIS:LOGCHARGES', 'DIAGNOSIS:SEX', 'DIAGNOSIS:DRG')
dependent.name <- "LOS-1"

# find the best model
poisson.bm.str <- best_model(model_str1, model_str2, dependent.name, predictors, data = ami, test = 'Chisq')
poisson.bm <- glm(LOS-1 ~ 1 + log(CHARGES + 2000) + DRG + AGE + DIAGNOSIS + SEX + 
                    LOGCHARGES.na, family = 'poisson', data = ami)

mod1 = hurdle(LOS-1~DIAGNOSIS,dist='poisson',data=ami)
mod2 = hurdle(LOS-1~DIAGNOSIS+AGE,dist='poisson',data=ami)
logLik(mod1)
logLik(mod2)

a5 <- apply(cbind(paste("(",names(ami[6]),"-1)~"),data.frame(t(combn(predictors, 10)))), 1, paste, collapse="+")
models5 <- lapply(a5,FUN = function(X) hurdle(X, dist="poisson",data=ami))

results <- data.frame(model = c(), deviance = c(), coefficients = c(), least_deviance = c())
buffer <- data.frame(model = c(), deviance = c(), coefficients = c(), least_deviance = c())
for(i in 1:length(models5)){
  newdf <- data.frame(model = models5[[i]][['formula']],deviance = models5[[i]][['deviance']], coefficients = length(models5[[i]][["coefficients"]]), least_deviance = 0)
  buffer <- rbind(buffer,newdf)
}
buffer$least_deviance[which.min(buffer$deviance)] <- 1
results <- rbind(results,buffer)
buffer <- data.frame(model = c(), deviance = c(), coefficients = c(), least_deviance = c())
