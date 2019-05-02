if (!require(aod)) {install.packages("aod"); require(aod)}

source('Functions.R')

# load in data
path <- '/Users/jeanettejin/stat149project/'
ami <- load.data(path)

ami <- ami[ami$LOS != 0, ]


# dropped dataframe and converted dataframe
ami.drop <- ami[complete.cases(ami), ]
ami.convert <- na.convert.mean(ami)

hist(ami$LOS)


#######################################################################################
##############################          GAMMA         #################################
##############################                        #################################
#######################################################################################


#### with dropped data ########
model_str1 <- 'glm('
model_str2 <- ',family = Gamma(log), data = ami.drop)'
predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'scale(CHARGES)' , 'AGE')
dependent.name <- "LOS"

# find the best model
gamma.drop.bm.str <- best_model(model_str1, model_str2, dependent.name, predictors, data = ami.drop, test = 'F')
gamma.drop.bm.str <- glm(gamma.drop.bm.str, family = Gamma(log), data = ami.drop)
summary(gamma.drop.bm.str)


par(mfrow=c(1,2))
resid_plot(gamma.drop.bm.str, 'Gamma')
cooks_plot(gamma.drop.bm.str, 'Gamma')
dev.off()


##### convert data ######
#########################

model_str1 <- 'glm('
model_str2 <- ',family = Gamma(log), data = ami.convert)'
predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'scale(CHARGES)' , 'AGE', 'CHARGES.na')
dependent.name <- "LOS"

gamma.conv.bm.str <- best_model(model_str1, model_str2, dependent.name, predictors, data = ami.convert, test = 'F')
gamma.conv.bm <- glm(eval(gamma.conv.bm.str), family = Gamma(log), data = ami.convert)

summary(gamma.conv.bm)

par(mfrow=c(1,2))
resid_plot(gamma.conv.bm, 'Gamma')
cooks_plot(gamma.conv.bm, 'Gamma')
dev.off()
