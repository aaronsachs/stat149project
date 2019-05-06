
source('/Users/jeanettejin/stat149project/Functions.R')

import('aod')
import('MASS')
import('stats')
import('mgcv')
import('gridExtra')
import('pander')
import('gsub')


# load in data
path <- '/Users/jeanettejin/stat149project/'
ami <- load.data(path)


#######################################################################################
##############################          GAMMA         #################################
##############################                        #################################
#######################################################################################


# find the best model independent variables
model_str1 <- 'glm('
model_str2 <- ',family = Gamma(log), data = ami)'
predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'LOGCHARGES' , 'AGE', 'LOGCHARGES.na')
dependent.name <- "LOS"


gamma.bm.str <- best_model(model_str1, model_str2, dependent.name, predictors, data = ami, test = 'F')
gamma.bm <- glm(gamma.bm.str , family = Gamma(log), data = ami)
summary(gamma.bm)
# diagnostic plots
diagnostic_plots(gamma.bm, "Gamma Indep. Var Final Model")



# find the best model after full effect fit including interactions
model.str1 <- 'glm('
model.str2 <- ',  Gamma(log), data = ami)'
current.formula <- 'LOS ~ DIAGNOSIS + SEX + DRG + LOGCHARGES +  LOGCHARGES.na + AGE'
predictors.list <-   c('DIAGNOSIS:DRG','DIAGNOSIS:LOGCHARGES.na', 'DRG:SEX', 'SEX:LOGCHARGES.na', 
                       'LOGCHARGES:DIAGNOSIS', 'LOGCHARGES:SEX', 'LOGCHARGES:DRG', 'AGE:DIAGNOSIS', 'AGE:SEX',
                       'AGE:DRG', 'AGE:LOGCHARGES.na')




gamma.bm.inter.str <- consider_predictors(model.str1, model.str2, current.formula, predictors.list, "F")
gamma.bm.inter <- glm(gamma.bm.inter.str , family = Gamma(log), data = ami)


summary(gamma.bm.inter)
diagnostic_plots(gamma.bm.inter, "Gamma Indep. Var + Interactions Final Model")

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

diagnostic_plots(invg.bm, "Inv. Gaussian Indep. Var + Interactions Final Model")


# find the best model after indep variables fit including interactions
model.str1 <- 'glm('
model.str2 <- ',family=inverse.gaussian(log), data = ami)'
current.formula <- "LOS ~  LOGCHARGES + DRG + AGE + DIAGNOSIS + LOGCHARGES.na + SEX"
predictors.list <-   c('DIAGNOSIS:DRG','DIAGNOSIS:LOGCHARGES.na', 'DRG:SEX', 'SEX:LOGCHARGES.na', 
                       'LOGCHARGES:DIAGNOSIS', 'LOGCHARGES:SEX', 'LOGCHARGES:DRG', 'AGE:DIAGNOSIS', 'AGE:SEX',
                       'AGE:DRG', 'AGE:LOGCHARGES.na')


invg.bm.inter.str <- consider_predictors(model.str1, model.str2, current.formula, predictors.list, "F")
invg.bm.inter <- glm(invg.bm.inter.str , family=inverse.gaussian(log), data = ami)

gsub("(.{5})", "\\1 ", invg.bm.inter.str)

summary(invg.bm.inter)
diagnostic_plots(invg.bm.inter, "Inv. Gaussian Indep. Var + Interactions Final Model")

#######################################################################################
##############################    TWEEEDIE         ###################################
##############################                     ###################################
#######################################################################################
    

# find the best model full effects
model_str1 <- 'gam('
model_str2 <- ',family=tw(link="log"), data = ami)'
predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'LOGCHARGES' , 'AGE', 'LOGCHARGES.na')
dependent.name <- "LOS"

twe.bm.str <- best_model(model_str1, model_str2, dependent.name, predictors, data = ami, test = 'F')
twe.bm <- gam(twe.bm.str, family=tw(link="log"), data = ami)
summary(twe.bm)

par(mfrow=c(1,2))
resid_plot(twe.bm, '')
cooks_plot(twe.bm, '')
mtext('Tweedie Indep Var. ', side = 3, line = -3, outer = TRUE)

dev.off()


# find the best model after full effect fit including interactions
model.str1 <- 'gam('
model.str2 <- ',family=tw(link="log"), data = ami)'
current.formula <- "LOS ~  LOGCHARGES + DRG + AGE + DIAGNOSIS + LOGCHARGES.na + SEX"
predictors.list <-   c('DIAGNOSIS:DRG','DIAGNOSIS:LOGCHARGES.na', 'DRG:SEX', 'SEX:LOGCHARGES.na', 
                       'LOGCHARGES:DIAGNOSIS', 'LOGCHARGES:SEX', 'LOGCHARGES:DRG', 'AGE:DIAGNOSIS', 'AGE:SEX',
                       'AGE:DRG', 'AGE:LOGCHARGES.na')


twe.bm.inter.str <- consider_predictors(model.str1, model.str2, current.formula, predictors.list, "F")
twe.bm.inter.str <- "LOS ~  LOGCHARGES + DRG + AGE + DIAGNOSIS + LOGCHARGES.na + SEX + LOGCHARGES:DIAGNOSIS + 
LOGCHARGES:SEX + AGE:DIAGNOSIS + AGE:DRG + AGE:SEX"
twe.bm.inter <- gam(eval(parse(text = twe.bm.inter.str)), family=tw(link="log"), data = ami)
summary(twe.bm.inter)


par(mfrow=c(1,2))
resid_plot(twe.bm.inter, '')
cooks_plot(twe.bm.inter, '')
mtext('Tweedie Interactions Final Model', side = 3, line = -3, outer = TRUE)
dev.off()



#######################################################################################
##############################   COMPARING MODELS  ###################################
##############################                     ###################################
#######################################################################################


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
twe.inter.tchi <- test.chi.sq('gam(formula = LOS ~ DIAGNOSIS + SEX + DRG + LOGCHARGES +  LOGCHARGES.na + AGE + LOGCHARGES:DRG  + SEX:LOGCHARGES.na + LOGCHARGES:DIAGNOSIS, family=tw(link="log"),')

model <- c('Gamma', "Gamma + Inter", "InvG", 'InvG + Inter', 'Tweedle', 'Tweedle + Inter')
twe.bm.str <- 'LOS ~ 1 + LOGCHARGES + DRG + DIAGNOSIS + LOGCHARGES.na'
# twe.bm.inter.str <- "LOS ~  LOGCHARGES + DRG + AGE + DIAGNOSIS + \n
# LOGCHARGES.na + SEX + LOGCHARGES:DIAGNOSIS + \nLOGCHARGES:SEX + AGE:DIAGNOSIS + AGE:DRG + AGE:SEX"
formula <- c(spaces_formula(gamma.bm.str), spaces_formula(gamma.bm.inter.str), 
             spaces_formula(invg.bm.str), spaces_formula(invg.bm.inter.str),
             spaces_formula(as.character(twe.bm.str)), spaces_formula(twe.bm.inter.str))
chi.stat <- c(gamma.chi, gamma.inter.chi, invg.chi, invg.inter.chi, twe.chi, twe.inter.chi)
test.chi.stat <- c(gamma.tchi, gamma.inter.tchi, invg.tchi, invg.inter.tchi, twe.tchi, twe.inter.tchi)

skew.table.comparison <- data.frame(model, formula,chi.stat, test.chi.stat)
skew.table.comparison 
rownames(skew.table.comparison) <- NULL
colnames(skew.table.comparison) <- c('Model','Formula', 'Chi Stat', 'Chi Stat Test Set')
dev.off()

pander(skew.table.comparison)
grid.table(skew.table.comparison, row = c('','',' ','','',''))
# final model is Gamma




