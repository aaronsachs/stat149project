if (!require(aod)) {install.packages("aod"); require(aod)}
library(MASS)
library(stats)
library(mgcv)

source('/Users/jeanettejin/stat149project/Functions.R')

# load in data
path <- '/Users/jeanettejin/stat149project/'
ami <- load.data(path)


#######################################################################################
##############################          GAMMA         #################################
##############################                        #################################
#######################################################################################


# find the best model full effects
model_str1 <- 'glm('
model_str2 <- ',family = Gamma(log), data = ami)'
predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'LOGCHARGES' , 'AGE', 'LOGCHARGES.na')
dependent.name <- "LOS"


gamma.bm.str <- best_model(model_str1, model_str2, dependent.name, predictors, data = ami, test = 'F')
gamma.bm <- glm(gamma.bm.str , family = Gamma(log), data = ami)

summary(gamma.bm)
# diagnostic plots
diagnostic_plots(gamma.bm, "Gamma Full Effect Final Model")



# find the best model after full effect fit including interactions
model.str1 <- 'glm('
model.str2 <- ',  Gamma(log), data = ami)'
current.formula <- 'LOS ~ DIAGNOSIS + SEX + DRG + LOGCHARGES +  LOGCHARGES.na + AGE'
predictors.list <-  c('DIAGNOSIS:DRG','DIAGNOSIS:LOGCHARGES.na',
                      'DRG:SEX', 'DRG:LOGCHARGES.na', 'SEX:LOGCHARGES.na', 
                      'LOGCHARGES:DIAGNOSIS', 'LOGCHARGES:SEX', 'LOGCHARGES:DRG', 
                      'AGE:DIAGNOSIS')


gamma.bm.inter.str <- consider_predictors(model.str1, model.str2, current.formula, predictors.list, "F")
gamma.bm.inter <- glm(gamma.bm.inter.str , family = Gamma(log), data = ami)

summary(gamma.bm.inter)
diagnostic_plots(gamma.bm.inter, "Gamma Full Effect + Interactions Final Model")


#######################################################################################
##############################    INVERSE GAUSSIAN ###################################
##############################                     ###################################
#######################################################################################

# find the best model full effects
model_str1 <- 'glm('
model_str2 <- ',family=inverse.gaussian(log), data = ami)'
predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'LOGCHARGES' , 'AGE', 'LOGCHARGES.na')
dependent.name <- "LOS"

invg.bm.str <- best_model(model_str1, model_str2, dependent.name, predictors, data = ami, test = 'F')
invg.bm <- glm(invg.bm.str, family=inverse.gaussian(log), data = ami)
summary(invg.bm)

diagnostic_plots(invg.bm, "Inv. Gaussian Full Effect + Interactions Final Model")


# find the best model after full effect fit including interactions
model.str1 <- 'glm('
model.str2 <- ',family=inverse.gaussian(log), data = ami)'
current.formula <- "LOS ~  LOGCHARGES + DRG + AGE + DIAGNOSIS + LOGCHARGES.na + SEX"
predictors.list <-  c('DIAGNOSIS:DRG','DIAGNOSIS:LOGCHARGES.na',
                      'DRG:SEX', 'DRG:LOGCHARGES.na', 'SEX:LOGCHARGES.na', 
                      'LOGCHARGES:DIAGNOSIS', 'LOGCHARGES:SEX', 'LOGCHARGES:DRG', 
                      'AGE:DIAGNOSIS')


invg.bm.inter.str <- consider_predictors(model.str1, model.str2, current.formula, predictors.list, "F")
invg.bm.inter <- glm(invg.bm.inter.str , family=inverse.gaussian(log), data = ami)

summary(invg.bm.inter)
diagnostic_plots(invg.bm.inter, "Inv. Gaussian Full Effect + Interactions Final Model")

#######################################################################################
##############################    TWEEEDIE         ###################################
##############################                     ###################################
#######################################################################################
    

# find the best model full effects
model_str1 <- 'gam('
model_str2 <- ',family=tw(link="log"), data = ami)'
predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'LOGCHARGES' , 'AGE', 'LOGCHARGES.na')
dependent.name <- "LOS"

twe.bm.str <- best_model(model_str1, model_str2, dependent.name, predictors, data = ami, test = 'Chisq')
twe.bm <- gam(twe.bm.str, family=tw(link="log"), data = ami)
summary(twe.bm)

par(mfrow=c(1,2))
resid_plot(twe.bm, '')
cooks_plot(twe.bm, '')
mtext('Tweedie Full Effect', side = 3, line = -3, outer = TRUE)
dev.off()


# find the best model after full effect fit including interactions
model.str1 <- 'gam('
model.str2 <- ',family=tw(link="log"), data = ami)'
current.formula <- "LOS ~  LOGCHARGES + DRG + AGE + DIAGNOSIS + LOGCHARGES.na + SEX"
predictors.list <-  c('DIAGNOSIS:DRG','DIAGNOSIS:LOGCHARGES.na',
                      'DRG:SEX', 'DRG:LOGCHARGES.na', 'SEX:LOGCHARGES.na', 
                      'LOGCHARGES:DIAGNOSIS', 'LOGCHARGES:SEX', 'LOGCHARGES:DRG', 
                      'AGE:DIAGNOSIS')


twe.bm.inter.str <- consider_predictors(model.str1, model.str2, current.formula, predictors.list, "F")
twe.bm.inter <- gam(LOS ~ DIAGNOSIS + SEX + DRG + LOGCHARGES +  LOGCHARGES.na + AGE + LOGCHARGES:DRG + DRG:LOGCHARGES.na + SEX:LOGCHARGES.na + LOGCHARGES:DIAGNOSIS, family=tw(link="log"), data = ami)
summary(twe.bm.inter)


par(mfrow=c(1,2))
resid_plot(twe.bm.inter, '')
cooks_plot(twe.bm.inter, '')
mtext('Tweedie Full Effect + Interactions Final Model', side = 3, line = -3, outer = TRUE)
dev.off()



gamma.chi <- goodness.fit.model(gamma.bm)
gamma.inter.chi <- goodness.fit.model(gamma.bm.inter)
invg.chi <- goodness.fit.model(invg.bm)
invg.inter.chi <- goodness.fit.model(invg.bm.inter)
twe.chi <- goodness.fit.model(twe.bm)
twe.inter.chi <- goodness.fit.model(twe.bm.inter)


# gamma bm str
gamma.tchi <- test.chi.sq('glm(formula = gamma.bm.str, family = Gamma(log),')
gamma.inter.tchi <- test.chi.sq('glm(formula = gamma.bm.inter.str, family = Gamma(log),')
invg.tchi <- test.chi.sq('glm(formula = invg.bm.str, family=inverse.gaussian(log),')
invg.inter.tchi <- test.chi.sq('glm(formula = invg.bm.inter.str, family=inverse.gaussian(log),')
twe.tchi <- test.chi.sq('gam(formula = twe.bm.str, family=tw(link="log"),')
twe.inter.tchi <- test.chi.sq('gam(formula = LOS ~ DIAGNOSIS + SEX + DRG + LOGCHARGES +  LOGCHARGES.na + AGE + LOGCHARGES:DRG + DRG:LOGCHARGES.na + SEX:LOGCHARGES.na + LOGCHARGES:DIAGNOSIS, family=tw(link="log"),')

model <- c('Gamma', "Gamma + Inter", "InvG", 'InvG + Inter', 'Tweedle', 'Tweedle + Inter')
chi.stat <- c(gamma.chi, gamma.inter.chi, invg.chi, invg.inter.chi, twe.chi, twe.inter.chi)
test.chi.stat <- c(gamma.tchi, gamma.inter.tchi, invg.tchi, invg.inter.tchi, twe.tchi, twe.inter.tchi)

pander(data.frame(names, chi.stat, test.chi.stat))






