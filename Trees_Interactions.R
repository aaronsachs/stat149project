source('/Users/jeanettejin/stat149project/Functions.R')
import('mgcv')
import('rpart')




# load in data
path <- '/Users/jeanettejin/stat149project/'
ami <- load.data(path)


#----------------------------------------------------#
#----- TREES FOR INTERACTIONS------------------------#
#----------------------------------------------------#

# looks like logcharges and age are most predictive
tree.ami.all <- rpart(LOS ~ LOGCHARGES + AGE + DIAGNOSIS + SEX + DRG + LOGCHARGES.na,
                      method = "anova", parm = list(split = "information"), data = ami)

prp(tree.ami.all, extra = 1, main = "Tree representation of fitted model")

# other features not that significant
tree.ami.cat <- rpart(LOS ~ DIAGNOSIS + SEX + DRG + LOGCHARGES.na,
                      method = "anova", parm = list(split = "information"), data = ami)
prp(tree.ami.cat, extra = 1, main = "Tree representation of fitted model")


