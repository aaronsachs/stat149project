if (!require(aod)) {install.packages("aod"); require(aod)}

source('/Users/martinurbisaia/Documents/STAT149/project/Functions.R')

# load in data
path <- '/Users/martinurbisaia/Documents/STAT149/project/'
ami <- load.data(path)
str(ami)


#######################################################################################
##############################          POISSON       #################################
##############################                        #################################
#######################################################################################

# DRG:AGE, DRG:LOGCHARGES, DIAGNOSIS:LOGCHARGES, DIAGNOSIS:AGE, DIAGNOSIS:SEX

#### with dropped data ########
model_str1 <- 'glm('
model_str2 <- ',family = poisson, data = ami)'
predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'LOGCHARGES' , 'AGE', 'LOGCHARGES.na', 'DRG:AGE', 'DRG:LOGCHARGES', 'DIAGNOSIS:SEX')
dependent.name <- "LOS"

# find the best model
poisson.str <- best_model(model_str1, model_str2, dependent.name, predictors, data = ami, test = 'Chisq')
poisson.bm <- glm(poisson.str, family = poisson, data = ami)
summary(poisson.bm)


par(mfrow=c(1,2))
resid_plot(poisson.bm, 'Poisson')
cooks_plot(poisson.bm, 'Poisson')
dev.off()

