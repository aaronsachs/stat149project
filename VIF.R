if (!require(car)) {install.packages("car"); require(car)}
if (!require(gridExtra)) {install.packages("gridExtra"); require(gridExtra)}

source('/Users/jeanettejin/stat149project/Functions.R')

path <- '/Users/jeanettejin/stat149project/'
ami <- load.data(path)

# vif for full effects
vif.lm <- lm(LOS ~ DIAGNOSIS + SEX + DRG + LOGCHARGES + AGE, data = ami)

# store in table and rename
vif.df <- data.frame(vif(vif.lm))
vif.df$GVIF.Control.DF  <- vif.df$GVIF..1..2.Df.. 
vif.df$GVIF..1..2.Df.. <- NULL

# pretty plot for report
dev.off()
grid.table(vif.df)
dev.off()


