if (!require(aod)) {install.packages("aod"); require(aod)}
if (!require(sjPlot)) {install.packages("sjPlot"); require(sjPlot)}
if (!require(sjmisc)) {install.packages("sjmisc"); require(sjmisc)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(gridExtra)) {install.packages("gridExtra"); require(gridExtra)}


install.packages("ggplot2", dependencies = TRUE)
# load in data
path <- '/Users/jeanettejin/stat149project/'
ami <- load.data(path)
#----------------------------------------------------#
#------- JUSTIFICATION FOR MISSING VAL---------------#
#----------------------------------------------------#

print(paste(path, 'amidata.csv', sep = ''))
amii <- read.csv(paste(path, 'amidata.csv', sep =''))

amii.drop <- ami[complete.cases(amii), ]
amii.convert <- na.convert.mean(amii)
summary(amii)
amii.convert$CHARGES.na <- as.factor(amii.convert$CHARGES.na)


hist(amin[complete.cases(amin), ]$LOS)
hist(ami[!complete.cases(ami), ]$LOS)


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

# summary of ami shows min charges is 3, which is really small
summary(ami)



ami.drop.1000 <-  ami[ami$CHARGES > 1000,]
ami.drop.500 <- ami[ami$CHARGES > 500,]
ami.drop.300 <- ami[ami$CHARGES > 300,]


ggplot(ami.drop.300, aes(x = CHARGES)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "CHARGES", x = "Total Hospital CHARGES", y = " ")
ggplot(ami.drop.500, aes(x = CHARGES)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "CHARGES", x = "Total Hospital CHARGES", y = " ")
ggplot(ami.drop.1000, aes(x = CHARGES)) + geom_histogram(alpha = 0.8) + theme_gray() + 
  labs(title = "CHARGES", x = "Total Hospital CHARGES", y = " ")

poisson <- glm(LOS ~  AGE  + DRG + SEX + DIAGNOSIS + LOGCHARGES + CHARGES.na, family='poisson', data=ami)
poisson.drop.300 <- glm(LOS ~  AGE  + DRG + SEX + DIAGNOSIS + LOGCHARGES + CHARGES.na, family='poisson', data=ami.drop.300)
poisson.drop.500 <- glm(LOS ~  AGE  + DRG + SEX + DIAGNOSIS + LOGCHARGES + CHARGES.na, family='poisson', data=ami.drop.500)
poisson.drop.1000 <- glm(LOS ~  AGE  + DRG + SEX + DIAGNOSIS + LOGCHARGES + CHARGES.na, family='poisson', data=ami.drop.1000)

par(mfrow=c(2,2))
resid_plot(poisson, 'Poisson')
resid_plot(poisson.drop.300, 'Poisson 300')
resid_plot(poisson.drop.500, 'Poisson 500')
resid_plot(poisson.drop.1000, 'Poisson 1000')