if (!require(aod)) {install.packages("aod"); require(aod)}
library(MASS)
library(stats)

source('Functions.R')

# load in data
path <- '/Users/jeanettejin/stat149project/'
ami <- load.data(path)
str(ami)

#######################################################################################
##############################          GAMMA         #################################
##############################                        #################################
#######################################################################################


# find the best model full effects
model_str1 <- 'glm('
model_str2 <- ',family = Gamma(log), data = ami)'
predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'LOGCHARGES' , 'AGE', 'LOGCHARGES.na', 'DRG:AGE', 'DRG:LOGCHARGES', 'DIAGNOSIS:LOGCHARGES', 'DIAGNOSIS:SEX')
dependent.name <- "LOS"


gamma.bm.str <- best_model(model_str1, model_str2, dependent.name, predictors, data = ami, test = 'F')
gamma.bm <- glm(gamma.bm.str , family = Gamma(log), data = ami)

# find the best model after full effect fit including interactions
model.str1 <- 'glm('
model.str2 <- ',  Gamma(log), data = ami)'
current.formula <- 'LOS ~ DIAGNOSIS + SEX + DRG + LOGCHARGES + AGE'
predictors.list <-  c('DIAGNOSIS:DRG','DIAGNOSIS:SEX','DIAGNOSIS:LOGCHARGES.na',
                      'DRG:SEX', 'DRG:LOGCHARGES.na', 'SEX:LOGCHARGES.na', 
                      'LOGCHARGES:DIAGNOSIS', 'LOGCHARGES:SEX', 'LOGCHARGES:DRG', 
                      'AGE:DIAGNOSIS')


gamma.bm.inter.str <- consider_predictors(model.str1, model.str2, current.formula, predictors.list, "F")
gamma.bm.inter <- glm(gamma.bm.inter.str , family = Gamma(log), data = ami)

resid_plot(gamma.bm.inter, 'Gamma w/ Interact')



















# model.str1 <- 'glm('
# model.str2 <- ',  poisson(link = "log"), data = ami)'
# current.formula <- 'LOS ~ DIAGNOSIS + SEX + DRG + LOGCHARGES + AGE'
# predictors.list <-  c('DIAGNOSIS:DRG','DIAGNOSIS:SEX','DIAGNOSIS:LOGCHARGES.na',
#                       'DRG:SEX', 'DRG:LOGCHARGES.na', 'SEX:LOGCHARGES.na', 
#                       'LOGCHARGES:DIAGNOSIS', 'LOGCHARGES:SEX', 'LOGCHARGES:DRG', 
#                       'AGE:DIAGNOSIS')
# 
# 
# poisson.inter.model.str <- consider_predictors(model.str1, model.str2, current.formula, predictors.list)
# poisson.inter.bm <- glm(poisson.inter.model.str, poisson(link = "log"), data = ami)
# 
# summary(poisson.inter.bm)











