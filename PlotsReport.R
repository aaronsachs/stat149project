if (!require(aod)) {install.packages("aod"); require(aod)}
if (!require(sjPlot)) {install.packages("sjPlot"); require(sjPlot)}
if (!require(sjmisc)) {install.packages("sjmisc"); require(sjmisc)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(gridExtra)) {install.packages("gridExtra"); require(gridExtra)}

source('/Users/jeanettejin/stat149project/Functions.R')


#----------------------------------------------------#
#------- JUSTIFICATION FOR MISSING VAL---------------#
#----------------------------------------------------#

print(paste(path, 'amidata.csv', sep = ''))
amii <- read.csv(paste(path, 'amidata.csv', sep =''))

amii.complete <- amii[complete.cases(amii), ]
amii.drop <- amii[!complete.cases(amii), ]

whole <- ggplot(amii.complete, aes(x = LOS)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "LOS Non-Missing Data", x = "LOS", y = " ") 

drop <- ggplot(amii.drop, aes(x = LOS)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "LOS Missing Data", x = "LOS", y = " ") 


grid.arrange(whole, drop, ncol = 2)

#----------------------------------------------------#
#------- JUSTIFICATION FOR LOG CHARGES---------------#
#----------------------------------------------------#
# histogram comparisons
ggplot(ami, aes(x = CHARGES)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "Total Hospital LOGCHARGES ($)", x = "Total Hospital LOGCHARGES ($)", y = " ")
ggplot(ami, aes(x = LOGCHARGES)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "Total Hospital LOGCHARGES ($)", x = "Total Hospital LOGCHARGES ($)", y = " ")
dev.off()


dev.off()
par(mfrow=c(1,2))
fit.wo.log <- glm(LOS ~  AGE  + DRG + SEX + DIAGNOSIS + CHARGES + CHARGES.na, family='poisson', data=ami)
fit.w.log <- glm(LOS ~  AGE  + DRG + SEX + DIAGNOSIS + LOGCHARGES + CHARGES.na, family='poisson', data=ami)

resid_plot(fit.wo.log, 'Charges')
resid_plot(fit.w.log, 'Log Charges')



#----------------------------------------------------#
#------- JUSTIFICATION FOR DROPPING VALUES-----------#
#----------------------------------------------------#

amid <- load.data.show(path)

# summary of ami shows min charges is 3, which is really small
summary(amid)

length(amid[amid$CHARGES < 300,]$CHARGES)

mean(amid[amid$LOS == 6,]$CHARGES)

amid.less.1000 <-  amid[amid$CHARGES < 1000,]

all <- ggplot(amid, aes(x = CHARGES)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "CHARGES", x = "Total Hospital CHARGES", y = " ")
less1000 <- ggplot(amid.less.1000, aes(x = CHARGES)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "CHARGES -- Closer Look", x = "Total Hospital CHARGES", y = " ")

# plot
grid.arrange(all, less1000,  ncol = 2)

# drop 
amid.drop.300 <-  amid[(!amid$CHARGES < 300) | (is.na(amid$CHARGES)),]

poisson <- glm(LOS ~  AGE  + DRG + SEX + DIAGNOSIS + LOGCHARGES + CHARGES.na, family='poisson', data=amid)
poisson.drop.300 <- glm(LOS ~  AGE  + DRG + SEX + DIAGNOSIS + LOGCHARGES + CHARGES.na, family='poisson', data=amid.drop.300)

par(mfrow=c(1,2))
resid_plot(poisson, 'Poisson')
resid_plot(poisson.drop.300, 'Poisson 300')


#----------------------------------------------------#
#------- JUSTIFICATION FOR RANDOM NA-----------#
#----------------------------------------------------#

ami.drop <- ami[complete.cases(ami), ]
ami.convert <- na.convert.mean(ami)
summary(ami)
ami.convert$CHARGES.na <- as.factor(ami.convert$CHARGES.na)


hist(amin[complete.cases(amin), ]$LOS)
hist(ami[!complete.cases(ami), ]$LOS)