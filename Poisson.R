
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

ami <- ami[ami$CHARGES > 300,]


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

model <- c('Poisson','Poisson + Inter')
chi.stat <- c(poisson.chi,poisson.inter.chi)
test.chi.stat <- c(poisson.tchi,poisson.inter.tchi)
dev.off()
pander(data.frame(model, chi.stat, test.chi.stat))

grid.table(data.frame(model, chi.stat, test.chi.stat), row = c('',''))

#######################################################################################
##############################          POISSON       #################################
##############################           HURDLE        ################################
#######################################################################################

ami$LOS <- ami$LOS-1 

response_str <- 'LOS'
predictor_str <- c('DIAGNOSIS' , 'SEX', 'DRG', 'LOGCHARGES' , 'AGE', 'LOGCHARGES.na')
hurdle_best <- function(response_str,predictor_str){
  
  # fit full model
  predictors.str <- paste(predictor_str,collapse=" + ")
  full.str <- paste('hurdle(',response_str,'~',predictors.str,',dist=\'poisson\',data = ami)',sep="")
  hurdle.best <- eval(parse(text=full.str))
  
  p <- 0.05
  
  # fit all possible H_0
  nulls <- apply(cbind(paste(names(ami)[6],"~"),data.frame(t(combn(predictor_str, (length(predictor_str)-1))))), 1, paste, collapse="+")
  model.nulls.str <- rep(NA,length(nulls))
  for (i in 1:length(nulls)){
    model.nulls.str[i] <- paste('hurdle(',nulls[i],',dist=\'poisson\',data = ami)',sep="")
    model.null <- eval(parse(text=model.nulls.str[i]))
    lrt <- 2*(hurdle.best$loglik-model.null$loglik)
    degrees <- model.null$df.residual-hurdle.best$df.residual
    val[i] <- 1-pchisq(lrt,degrees)
    if (val[i] > p){
      hurdle.best <- model.null
      }
  }
  return(hurdle.best)
}

best.hurdle.model <- hurdle_best(response_str,predictor_str)

diagnostic_plots(best.hurdle.model, "Poisson Hurdle Final Model")
poisson.chi <- goodness.fit.model(best.hurdle.model)

poisson.tchi <- test.chi.sq('hurdle(formula = best.hurdle.model, family = \'poisson\',')



response_str <- 'LOS'
predictor_str_inter <- c(predictor_str,'DIAGNOSIS:DRG','DIAGNOSIS:LOGCHARGES.na',
                         'DRG:SEX', 'DRG:LOGCHARGES.na', 'SEX:LOGCHARGES.na', 
                         'LOGCHARGES:DIAGNOSIS', 'LOGCHARGES:SEX', 'LOGCHARGES:DRG', 
                         'AGE:DIAGNOSIS')

best.hurdle.model.inter <- hurdle_best(response_str,predictor_str_inter)


