if (!require(aod)) {install.packages("aod"); require(aod)}
if (!require(MASS)) {install.packages("MASS"); require(MASS)}

source('/Users/jeanettejin/stat149project/Functions.R')

# load in data
path <- '/Users/jeanettejin/stat149project/'
ami <- load.data(path)

# dropped dataframe and converted dataframe
ami.drop <- ami[complete.cases(ami), ]
ami.convert <- na.convert.mean(ami)

#######################################################################################
##############################    ORDINAL MODELING    #################################
##############################                      ###################################
#######################################################################################

###### DROPPED DATA
######
######

# make into categories
ami.drop$LOS.ordinal <- cut(ami.drop$LOS ,breaks=c(0,10,20,25,40),
                          labels=c("low","low-med","med","high"),ordered=T)

model_str1 <- 'polr('
model_str2 <- ', Hess = T, data = ami.drop)'
predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'DIED', 'scale(CHARGES)' , 'AGE')
dependent.name <- "LOS.ordinal"
ord.drop.bm.str <- best_model(model_str1, model_str2, dependent.name, predictors, ami.drop, test = "Chisq")

# final model
ord.drop.bm <- polr(eval(ord.drop.bm.str), Hess = T, data = ami.drop)
summary(ord.drop.bm)


##### USING NA CONVERT MEAN ########
################################

ami.convert[ami.convert$LOS == 0, ]$LOS <- 1

# make into categories
ami.convert$LOS.ordinal <- cut(ami.convert$LOS ,breaks=c(0,10,20,25,100),
                             labels=c("low","low-med","med","high"),ordered=T)


model_str1 <- 'polr('
model_str2 <- ', Hess = T, data = ami.convert)'
predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'DIED', 'scale(CHARGES)' , 'AGE', 'CHARGES.na')
dependent.name <- "LOS.ordinal"
ord.conv.bm.str <- best_model(model_str1, model_str2, dependent.name, predictors, ami.convert, test = "Chisq")

# best model
ord.conv.bm <- polr(eval(ord.conv.bm.str), Hess = T, data = ami.convert)
summary(ord.conv.bm)


