source('/Users/jeanettejin/stat149project/Functions.R')

import("MASS")

# load in data
path <- '/Users/jeanettejin/stat149project/'
ami <- load.data(path)


#######################################################################################
##############################    NBINOMIAL       ###################################
##############################                     ###################################
#######################################################################################

# find the best model independent variables
model_str1 <- 'glm.nb('
model_str2 <- ' ,data = ami)'
predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'LOGCHARGES' , 'AGE', 'LOGCHARGES.na')
dependent.name <- "LOS"

nbin.bm.str <- best_model(model_str1, model_str2, dependent.name, predictors, data = ami)
nbin.bm <- glm.nb(nbin.bm.str, data = ami)
summary(nbin.bm)
diagnostic_plots(nbin.bm.inter, "Negative Binomial Indep. Variables Final Model")



# find the best model after indepedent varaibles fit including interactions
model.str1 <- 'glm.nb('
model.str2 <- ', data = ami)'
current.formula <- 'LOS ~ DIAGNOSIS + SEX + DRG + LOGCHARGES +  LOGCHARGES.na + AGE'
predictors.list <-   c('DIAGNOSIS:DRG','DIAGNOSIS:LOGCHARGES.na', 'DRG:SEX', 'SEX:LOGCHARGES.na', 
                       'LOGCHARGES:DIAGNOSIS', 'LOGCHARGES:SEX', 'LOGCHARGES:DRG', 'AGE:DIAGNOSIS', 'AGE:SEX',
                       'AGE:DRG', 'AGE:LOGCHARGES.na')

nbin.bm.inter.str <- consider_predictors(model.str1, model.str2, current.formula, predictors.list)
nbin.bm.inter <- glm.nb(nbin.bm.inter.str , data = ami)
summary(nbin.bm.inter)
diagnostic_plots(nbin.bm.inter, "Negative Binomial Interactions Final Model")


########### MODEL COMPARISON
############################
############################
# find statistic for both models
nbin.chi <- goodness.fit.model(nbin.bm)
nbin.inter.chi <- goodness.fit.model(nbin.bm.inter)

# find statistic for both models
nbin.tchi <- test.chi.sq('glm.nb(formula = nbin.bm,')
nbin.inter.tchi <- test.chi.sq('glm.nb(formula = nbin.bm.inter,')
=
# put together lists 
model <- c("NBin", "NBin + Inter")
formula <- c(spaces_formula(nbin.bm.str), spaces_formula(nbin.bm.inter.str))
chi.stat <- c(nbin.chi, nbin.inter.chi)
test.chi.stat <- c(nbin.tchi, nbin.inter.tchi)

# make into table 
skew.table.comparison <- data.frame(model, formula, chi.stat, test.chi.stat)
skew.table.comparison 
rownames(skew.table.comparison) <- NULL
colnames(skew.table.comparison) <- c('Model','Formula', 'Chi Stat', 'Chi Stat Test Set')
dev.off()

# pretty print
pander(skew.table.comparison)
grid.table(skew.table.comparison, row = c('',''))
# final model is Gamma