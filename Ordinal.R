if (!require(aod)) {install.packages("aod"); require(aod)}

# load in data
ami <- read.csv('amidata.csv')
str(ami)

# turn specific columns to factors
factor.columns <- c("Patient", "DIAGNOSIS", 'SEX', 'DRG', 'DIED')
ami[, factor.columns] <- data.frame(apply(ami[factor.columns], 2, as.factor))

summary(ami)


#######################################################################################
##############################    ORDINAL MODELING    #################################
##############################                      ###################################
#######################################################################################

# new df for model
ami.ordinal <- ami


##### just droping the data ####
################################
# df for dropped data
ami.od <- ami.ordinal[complete.cases(ami.ordinal), ]

# make into categories
ami.od$LOS.ordinal <- cut(ami.od$LOS ,breaks=c(0,10,20,25,40),
                          labels=c("low","low-med","med","high"),ordered=T)

# analysis of deviance
ord.null <- polr(LOS.ordinal ~ 1, Hess = T, data = ami.od)
ord.c <- polr(LOS.ordinal ~ scale(CHARGES), Hess = T, data = ami.od)
ord.dia  <- polr(LOS.ordinal ~ DIAGNOSIS, Hess = T, data = ami.od)
ord.s  <- polr(LOS.ordinal ~ SEX, Hess = T, data = ami.od)
ord.dr  <- polr(LOS.ordinal ~ DRG, Hess = T, data = ami.od)
ord.die <- polr(LOS.ordinal ~ DIED, Hess = T, data = ami.od)
ord.a <- polr(LOS.ordinal ~ AGE, Hess = T, data = ami.od)

# most significant
anova(ord.null, ord.c, test = "Chisq")
anova(ord.null, ord.dia, test = "Chisq")
anova(ord.null, ord.s,  test = "Chisq")
anova(ord.null, ord.dr, test = "Chisq")
anova(ord.null, ord.die, test = "Chisq")
anova(ord.null, ord.a, test = "Chisq")

ord.c.dia <- polr(LOS.ordinal ~ scale(CHARGES) + DIAGNOSIS, Hess = T, data = ami.od)
ord.c.s <- polr(LOS.ordinal ~ scale(CHARGES)+ SEX, Hess = T, data = ami.od)
ord.c.dr <- polr(LOS.ordinal ~ scale(CHARGES)+ DRG, Hess = T, data = ami.od)
ord.c.die <- polr(LOS.ordinal ~ scale(CHARGES)+ DIED, Hess = T, data = ami.od)
ord.c.a <- polr(LOS.ordinal ~ scale(CHARGES) + AGE, Hess = T, data = ami.od)


anova(ord.c, ord.c.dia, test = "Chisq")
anova(ord.c, ord.c.s, test = "Chisq")
anova(ord.c, ord.c.dr, test = "Chisq")
anova(ord.c, ord.c.die, test = "Chisq")
# most signifiant
anova(ord.c, ord.c.a, test = "Chisq")


ord.c.a.dia <- polr(LOS.ordinal ~ scale(CHARGES) + AGE+ DIAGNOSIS, Hess = T, data = ami.od)
ord.c.a.s <- polr(LOS.ordinal ~ scale(CHARGES) + AGE + SEX, Hess = T, data = ami.od)
ord.c.a.die <- polr(LOS.ordinal ~ scale(CHARGES) + AGE+ DIED, Hess = T, data = ami.od)
ord.c.a.dr <- polr(LOS.ordinal ~ scale(CHARGES) + AGE + DRG , Hess = T, data = ami.od)


anova(ord.c.a, ord.c.a.dia, test = "Chisq")
anova(ord.c.a, ord.c.a.s, test = "Chisq")
# most significant
anova(ord.c.a, ord.c.a.die, test = "Chisq")
anova(ord.c.a, ord.c.a.dr, test = "Chisq")


ord.c.a.die.dia <- polr(LOS.ordinal ~ scale(CHARGES) + AGE+ DIED + DIAGNOSIS, Hess = T, data = ami.od)
ord.c.a.die.s <- polr(LOS.ordinal ~ scale(CHARGES) + AGE+ DIED + SEX, Hess = T, data = ami.od)
ord.c.a.die.dr <- polr(LOS.ordinal ~ scale(CHARGES) + AGE+ DIED + DRG, Hess = T, data = ami.od)

# take diagnosis more significant
anova(ord.c.a.die, ord.c.a.die.dia, test = "Chisq")
anova(ord.c.a.die, ord.c.a.die.s , test = "Chisq")
anova(ord.c.a.die, ord.c.a.die.dr , test = "Chisq")

ord.c.a.die.dia.s <- polr(LOS.ordinal ~ scale(CHARGES) + AGE+ DIED + DIAGNOSIS + SEX, Hess = T, data = ami.od)
ord.c.a.die.dia.dr <- polr(LOS.ordinal ~ scale(CHARGES) + AGE+ DIED + DIAGNOSIS + DRG, Hess = T, data = ami.od)

anova(ord.c.a.die.dia, ord.c.a.die.dia.s,test = "Chisq")
# more significant
anova(ord.c.a.die.dia, ord.c.a.die.dia.dr,test = "Chisq")

ord.c.a.die.dia.dr.s <- polr(LOS.ordinal ~ scale(CHARGES) + AGE+ DIED + DIAGNOSIS + DRG + SEX, Hess = T, data = ami.od)
anova(ord.c.a.die.dia.dr, ord.c.a.die.dia.dr.s,test = "Chisq")

# final model
summary(ord.c.a.die.dia.dr.s)

resid_plot(ord.c.a.die.dia.dr.s)


##### dealing with nans ########
################################
ami.od <- ami
ami.od[ami.od$LOS == 0, ]$LOS <- 1
ami.od.na <- na.convert.mean(ami)

# make into categories
ami.od.na$LOS.ordinal <- cut(ami.od.na$LOS ,breaks=c(0,10,20,25,100),
                             labels=c("low","low-med","med","high"),ordered=T)



ord.na.c.a.die.dia.s <- polr(LOS.ordinal ~ scale(CHARGES) + AGE+ DIED + DIAGNOSIS + SEX + CHARGES.na , Hess = T, data = ami.od.na)
summary(ord.na.c.a.die.dia.s)
