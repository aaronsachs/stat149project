
source('/Users/jeanettejin/stat149project/Functions.R')

import('mgcv')
import('gam')





#----------------------------------------------------#
#----- INFERENCE WITH GAMS--------------------------#
#----------------------------------------------------#


# load in data
path <- '/Users/jeanettejin/stat149project/'
ami <- load.data(path)

# split a spline with all of the data
ami.spline <- gam(formula = LOS ~ s(LOGCHARGES) + s(AGE) + DIAGNOSIS + SEX + DRG + LOGCHARGES.na, family=poisson, data = ami)
plot(ami.spline)

summary(ami.spline)

spline.chi <- goodness.fit.model(ami.spline)
spline.chi
spline.tchi <- test.chi.sq('gam(formula = LOS ~ s(LOGCHARGES) + s(AGE) + DIAGNOSIS + SEX + DRG + LOGCHARGES.na, family=poisson,')
spline.tchi



# the gams with the smoothing terms give a significantly better fit!
ami.linear <- gam(formula = LOS ~ LOGCHARGES + AGE + DIAGNOSIS + SEX + DRG + LOGCHARGES.na, family=poisson, data = ami)
ami.s.lcharges <-  gam(formula = LOS ~ s(LOGCHARGES) + AGE + DIAGNOSIS + SEX + DRG + LOGCHARGES.na, family=poisson, data = ami)
ami.s.age <-  gam(formula = LOS ~ LOGCHARGES + s(AGE) + DIAGNOSIS + SEX + DRG + LOGCHARGES.na, family=poisson, data = ami)
anova(ami.linear, ami.s.lcharges, test = "Chisq")
anova(ami.linear, ami.s.age, test = "Chisq")
anova(ami.linear, ami.spline, test = "Chisq")






