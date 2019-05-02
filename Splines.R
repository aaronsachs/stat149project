if (!require(mgcv)) {install.packages("mgcv"); require(mgcv)}
if (!require(gam)) {install.packages("gam"); require(gam)}
if (!require(Metrics)) {install.packages("Metrics"); require(Metrics)}
if (!require(berryFunctions)) {install.packages("berryFunctions"); require(berryFunctions)}
if (!require(rpart)) {install.packages("rpart"); require(rpart)}


source('/Users/jeanettejin/stat149project/Functions.R')

#----------------------------------------------------#
#----- INFERENCE WITH GAMES--------------------------#
#----------------------------------------------------#


# load in data
path <- '/Users/jeanettejin/stat149project/'
ami <- load.data(path)

# split a spline with all of the data
ami.spline <- gam(formula = LOS ~ s(LOGCHARGES) + s(AGE) + DIAGNOSIS + SEX + DRG + LOGCHARGES.na, family=poisson, data = ami)
plot(ami.spline)

# the gams with the smoothing terms give a significantly better fit!
ami.linear <- gam(formula = LOS ~ LOGCHARGES + AGE + DIAGNOSIS + SEX + DRG + LOGCHARGES.na, family=poisson, data = ami)
ami.s.lcharges <-  gam(formula = LOS ~ s(LOGCHARGES) + AGE + DIAGNOSIS + SEX + DRG + LOGCHARGES.na, family=poisson, data = ami)
ami.s.age <-  gam(formula = LOS ~ LOGCHARGES + s(AGE) + DIAGNOSIS + SEX + DRG + LOGCHARGES.na, family=poisson, data = ami)
anova(ami.linear, ami.s.lcharges, test = "Chisq")
anova(ami.linear, ami.s.age, test = "Chisq")
anova(ami.linear, ami.spline, test = "Chisq")


#----------------------------------------------------#
#----- PREDICTION WITH GAMS -------------------------#
#----------------------------------------------------#

# load data
path <- '/Users/jeanettejin/stat149project/'
prediction.data <- prediction.load.data(path)
ami.train <- prediction.data$train
ami.test <- prediction.data$test

ami.pgam <- gam(formula = LOS ~ s(AGE) + DIAGNOSIS + SEX, family=poisson, data = ami.train)
plot(ami.pgam)
summary(ami.pgam)
mae(ami.test$LOS, predict(ami.pgam, ami.test))
rsquare(ami.test$LOS, as.vector(predict(ami.pgam, ami.test)))

#----------------------------------------------------#
#----- PREDICTION WITH TREES ------------------------#
#----------------------------------------------------#

tree.pinit <- rpart(LOS ~ AGE + DIAGNOSIS + SEX,,
                    method = "anova", cp = .001, parm = list(split = "information"), data = ami.train)
printcp(tree.pinit)
plotcp(tree.pinit)

tree.final <- rpart(LOS ~ AGE + DIAGNOSIS + SEX,,
                    method = "anova", cp = .015, parm = list(split = "information"), data = ami.train)

mae(ami.test$LOS, predict(tree.final, ami.test))
rsquare(ami.test$LOS, as.vector(predict(tree.final, ami.test)))


#----------------------------------------------------#
#----- TREES FOR INTERACTIONS------------------------#
#----------------------------------------------------#

tree.ami <- rpart(LOS ~  DIAGNOSIS + SEX + DRG + LOGCHARGES.na,
                    method = "anova", parm = list(split = "information"), data = ami)
prp(tree.ami, extra = 1, main = "Tree representation of fitted model")
