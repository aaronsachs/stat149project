if (!require(aod)) {install.packages("aod"); require(aod)}

source('Functions.R')

# load in data
path <- '/Users/jeanettejin/stat149project/'
ami <- load.data(path)
str(ami)


#######################################################################################
##############################          GAMMA         #################################
##############################                        #################################
#######################################################################################


#### with dropped data ########
model_str1 <- 'glm('
model_str2 <- ',family = Gamma(log), data = ami)'
predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'LOGCHARGES' , 'AGE', 'LOGCHARGES.na')
dependent.name <- "LOS"

# find the best model
gamma.bm.str <- best_model(model_str1, model_str2, dependent.name, predictors, data = ami, test = 'F')
gamma.bm <- glm(gamma.bm.str, family = Gamma(log), data = ami)
summary(gamma.bm)


par(mfrow=c(1,2))
resid_plot(gamma.bm, 'Gamma')
cooks_plot(gamma.bm, 'Gamma')
dev.off()

