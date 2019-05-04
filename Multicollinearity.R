
source('/Users/jeanettejin/stat149project/Functions.R')
import('car')
import('gridExtra')



path <- '/Users/jeanettejin/stat149project/'
ami <- load.data(path)


vif.lm <- lm(LOS ~ DIAGNOSIS + SEX + DRG + LOGCHARGES + AGE, data = ami)

vif.df <- data.frame(vif(vif.lm))
vif.df$GVIF.Control.DF  <- vif.df$GVIF..1..2.Df.. 
vif.df$GVIF..1..2.Df.. <- NULL

grid.table(vif.df)
dev.off()







