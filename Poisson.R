
source('~/Documents/STAT149/proyecto/lucas/Functions.R')

if (!require(aod)) {install.packages("aod"); require(aod)}

library(MASS)
library(stats)
library(car)
library(pscl)
library(pander)
import('gridExtra')
import('GLMMadaptive')



# load in data
path <- '~/Documents/STAT149/proyecto/lucas/'
ami <- load.data(path)
str(ami)


#######################################################################################
##############################          POISSON       #################################
##############################                        #################################
#######################################################################################

# DRG:AGE, DRG:LOGCHARGES, DIAGNOSIS:LOGCHARGES, DIAGNOSES:SEX, 


#### with dropped data ########
model_str1 <- 'glm('
model_str2 <- ',family = \'poisson\', data = ami)'
predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'LOGCHARGES' , 'AGE', 'LOGCHARGES.na')
dependent.name <- "LOS"

# find the best model
poisson.bm.str <- best_model(model_str1, model_str2, dependent.name, predictors, data = ami, test = 'Chisq')
poisson.bm <- glm(poisson.bm.str, family = 'poisson', data = ami)


summary(poisson.bm)
# diagnostic plots
diagnostic_plots(poisson.bm, "Poisson Full Effect Final Model")

current.formula <- poisson.bm.str
predictors.list <-  c('DIAGNOSIS:DRG','DIAGNOSIS:LOGCHARGES.na',
                      'DRG:SEX', 'DRG:LOGCHARGES.na', 'SEX:LOGCHARGES.na', 
                      'LOGCHARGES:DIAGNOSIS', 'LOGCHARGES:SEX', 'LOGCHARGES:DRG', 
                      'AGE:DIAGNOSIS')

poisson.bm.inter.str <- consider_predictors(model_str1, model_str2, current.formula, predictors.list, "Chisq")
poisson.bm.inter <- glm(poisson.bm.inter.str , family = 'poisson', data = ami)

summary(poisson.bm.inter)
diagnostic_plots(poisson.bm.inter, "Poisson Full Effect + Interactions Final Model")


poisson.chi <- goodness.fit.model(poisson.bm)
poisson.inter.chi <- goodness.fit.model(poisson.bm.inter)

poisson.tchi <- test.chi.sq('glm(formula = poisson.bm.str, family = \'poisson\',')
poisson.inter.tchi <- test.chi.sq('glm(formula = poisson.bm.inter.str, family = \'poisson\',')


#######################################################################################
##############################          POISSON       #################################
##############################           HURDLE        ################################
#######################################################################################

path <- '~/Documents/STAT149/proyecto/lucas/scaled_data/'
ami <- load.data(path)

response_str <- 'LOS'
predictor_str <- c('DIAGNOSIS' , 'SEX', 'DRG', 'LOGCHARGES' , 'AGE', 'LOGCHARGES.na')

best.hurdle.model <- hurdle_best(response_str,predictor_str)

poisson.hurdle.chi <- goodness.fit.model(best.hurdle.model)

poisson.hurdle.tchi <- test.chi.sq('hurdle(best.hurdle.model, dist = \'poisson\',')



response_str <- 'LOS'
predictor_str_inter <- c(predictor_str,'DIAGNOSIS:DRG','DIAGNOSIS:LOGCHARGES.na',
                         'DRG:SEX', 'DRG:LOGCHARGES.na', 'SEX:LOGCHARGES.na', 
                         'LOGCHARGES:DIAGNOSIS', 'LOGCHARGES:SEX', 'LOGCHARGES:DRG', 
                         'AGE:DIAGNOSIS')

best.hurdle.model.inter <- hurdle(LOS~DIAGNOSIS+SEX+DRG+LOGCHARGES+AGE+LOGCHARGES.na+DIAGNOSIS:DRG+DIAGNOSIS:LOGCHARGES.na+DRG:SEX+DRG:LOGCHARGES.na+SEX:LOGCHARGES.na+LOGCHARGES:DIAGNOSIS+LOGCHARGES:SEX+LOGCHARGES:DRG+AGE:DIAGNOSIS, dist='poisson',data=ami)

poisson.hurdle.inter.chi <- goodness.fit.model(best.hurdle.model)

poisson.hurdle.inter.tchi <- test.chi.sq('hurdle(best.hurdle.model.inter, dist = \'poisson\',')

model <- c('Poisson','Poisson + Inter','Poisson Hurdle', 'Poisson Hurdle + Inter')
chi.stat <- c(poisson.chi,poisson.inter.chi,poisson.hurdle.chi,poisson.hurdle.inter.chi)
test.chi.stat <- c(poisson.tchi,poisson.inter.tchi,poisson.hurdle.tchi,poisson.hurdle.inter.tchi)
dev.off()
pander(data.frame(model, chi.stat, test.chi.stat))

grid.table(data.frame(model, chi.stat, test.chi.stat), row = c('','','',''))

par(mfrow=c(1,2))
# diagnostics
fitted <- fitted(best.hurdle.model)
stdresid <- residuals(best.hurdle.model, type="pearson")

# Residual plot (pearson residuals)
plot(fitted, stdresid,
     xlab="Fitted values",
     ylab="Pearson residuals",
     pch=19, col="red", cex=1.5)
abline(h=0,lty=2,col="green")
title(paste("Full Hurdle Model", ''), line = 1)

# diagnostics
fitted <- fitted(best.hurdle.model.inter)
stdresid <- residuals(best.hurdle.model.inter, type="pearson")

# Residual plot (pearson residuals)
plot(fitted, stdresid,
     xlab="Fitted values",
     ylab="Pearson residuals",
     pch=19, col="red", cex=1.5)
abline(h=0,lty=2,col="green")
title(paste("Hurdle+Interactions", ''), line = 1)

