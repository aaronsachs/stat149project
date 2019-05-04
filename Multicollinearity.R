if (!require(car)) {install.packages("car"); require(car)}
if (!require(gridExtra)) {install.packages("gridExtra"); require(gridExtra)}

source('/Users/jeanettejin/stat149project/Functions.R')

path <- '/Users/jeanettejin/stat149project/'
ami <- load.data(path)


vif.lm <- lm(LOS ~ DIAGNOSIS + SEX + DRG + LOGCHARGES + AGE, data = ami)

vif.df <- data.frame(vif(vif.lm))
vif.df$GVIF.Control.DF  <- vif.df$GVIF..1..2.Df.. 
vif.df$GVIF..1..2.Df.. <- NULL

grid.table(vif.df)
dev.off()






vif.lm <- lm(LOS ~ DIAGNOSIS + SEX + DRG + LOGCHARGES + AGE +
     DIAGNOSIS:DRG + DIAGNOSIS:SEX + DIAGNOSIS:LOGCHARGES.na + 
     DRG:SEX + DRG:LOGCHARGES.na + SEX:LOGCHARGES.na + 
     LOGCHARGES:DIAGNOSIS + LOGCHARGES:SEX + LOGCHARGES:DRG + AGE:DIAGNOSIS
   + AGE:SEX + AGE:DRG, data = ami)
vif(vif.lm)

vif.lm.sex <- lm(LOS ~ DIAGNOSIS + DRG + LOGCHARGES + AGE +
               DIAGNOSIS:DRG + DIAGNOSIS:SEX + DIAGNOSIS:LOGCHARGES.na + 
               DRG:SEX + DRG:LOGCHARGES.na + SEX:LOGCHARGES.na + 
               LOGCHARGES:DIAGNOSIS + LOGCHARGES:SEX + LOGCHARGES:DRG + AGE:DIAGNOSIS
             + AGE:SEX + AGE:DRG, data = ami)



+ AGE:SEX + AGE:DRG

