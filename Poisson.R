################################################################################
###################### POISSON MODEL ###########################################
################################################################################

#------------------------------------------------------------------------------
# This chunk may be removed if script is compiled with the main file
#------------------------------------------------------------------------------

if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}

# load in data
ami <- read.csv('/Users/martinurbisaia/Documents/STAT149/Project/amidata.csv')
str(ami)

# turn specific columns to factors
factor.columns <- c("Patient", "DIAGNOSIS", 'SEX', 'DRG', 'DIED')
ami[, factor.columns] <- data.frame(apply(ami[factor.columns], 2, as.factor))

################################################################################
######################### MINDLESS MODEL-BASHING (can I build a loop for this?)#
################################################################################

# combinatronics: find all possible variable combinations
vars <- c("DIAGNOSIS", "SEX", "DRG", "DIED", "CHARGES", "AGE")
a1 <- apply(cbind(paste(names(ami)[7],"~"),data.frame(t(combn(vars, 1)))), 1, paste, collapse="+")
a2 <- apply(cbind(paste(names(ami)[7],"~"),data.frame(t(combn(vars, 2)))), 1, paste, collapse="+")
a3 <- apply(cbind(paste(names(ami)[7],"~"),data.frame(t(combn(vars, 3)))), 1, paste, collapse="+")
a4 <- apply(cbind(paste(names(ami)[7],"~"),data.frame(t(combn(vars, 4)))), 1, paste, collapse="+")
a5 <- apply(cbind(paste(names(ami)[7],"~"),data.frame(t(combn(vars, 5)))), 1, paste, collapse="+")
a6 <- apply(cbind(paste(names(ami)[7],"~"),data.frame(t(combn(vars, 6)))), 1, paste, collapse="+")

# fit models
models1 <- lapply(a1,FUN = function(X) glm(X, data=ami, family="poisson"))
models2 <- lapply(a2,FUN = function(X) glm(X, data=ami, family="poisson"))
models3 <- lapply(a3,FUN = function(X) glm(X, data=ami, family="poisson"))
models4 <- lapply(a4,FUN = function(X) glm(X, data=ami, family="poisson"))
models5 <- lapply(a5,FUN = function(X) glm(X, data=ami, family="poisson"))
models6 <- lapply(a6,FUN = function(X) glm(X, data=ami, family="poisson"))

# list each model's deviance and coefficients
results <- data.frame(model = c(), deviance = c(), coefficients = c(), least_deviance = c())
buffer <- data.frame(model = c(), deviance = c(), coefficients = c(), least_deviance = c())
  for(i in 1:length(models1)){
    newdf <- data.frame(model = models1[[i]][['formula']],deviance = models1[[i]][['deviance']], coefficients = length(models1[[i]][["coefficients"]]), least_deviance = 0)
    buffer <- rbind(buffer,newdf)
  }
  buffer$least_deviance[which.min(buffer$deviance)] <- 1
  results <- rbind(results,buffer)
  buffer <- data.frame(model = c(), deviance = c(), coefficients = c(), least_deviance = c())
  
  for(i in 1:length(models2)){
    newdf <- data.frame(model = models2[[i]][['formula']],deviance = models2[[i]][['deviance']], coefficients = length(models2[[i]][["coefficients"]]), least_deviance = 0)
    buffer <- rbind(buffer,newdf)
  }
  buffer$least_deviance[which.min(buffer$deviance)] <- 1
  results <- rbind(results,buffer)
  buffer <- data.frame(model = c(), deviance = c(), coefficients = c(), least_deviance = c())
  
  for(i in 1:length(models3)){
    newdf <- data.frame(model = models3[[i]][['formula']],deviance = models3[[i]][['deviance']], coefficients = length(models3[[i]][["coefficients"]]), least_deviance = 0)
    buffer <- rbind(buffer,newdf)
  }
  buffer$least_deviance[which.min(buffer$deviance)] <- 1
  results <- rbind(results,buffer)
  buffer <- data.frame(model = c(), deviance = c(), coefficients = c(), least_deviance = c())
  
  for(i in 1:length(models4)){
    newdf <- data.frame(model = models4[[i]][['formula']],deviance = models4[[i]][['deviance']], coefficients = length(models4[[i]][["coefficients"]]), least_deviance = 0)
    buffer <- rbind(buffer,newdf)
  }
  buffer$least_deviance[which.min(buffer$deviance)] <- 1
  results <- rbind(results,buffer)
  buffer <- data.frame(model = c(), deviance = c(), coefficients = c(), least_deviance = c())
  
  for(i in 1:length(models5)){
    newdf <- data.frame(model = models5[[i]][['formula']],deviance = models5[[i]][['deviance']], coefficients = length(models5[[i]][["coefficients"]]), least_deviance = 0)
    buffer <- rbind(buffer,newdf)
  }
  buffer$least_deviance[which.min(buffer$deviance)] <- 1
  results <- rbind(results,buffer)
  buffer <- data.frame(model = c(), deviance = c(), coefficients = c(), least_deviance = c())
  
  for(i in 1:length(models6)){
    newdf <- data.frame(model = models6[[i]][['formula']],deviance = models6[[i]][['deviance']], coefficients = length(models6[[i]][["coefficients"]]), least_deviance = 0)
    buffer <- rbind(buffer,newdf)
  }
  buffer$least_deviance[which.min(buffer$deviance)] <- 1
  results <- rbind(results,buffer)
  buffer <- data.frame(model = c(), deviance = c(), coefficients = c(), least_deviance = c())

  #------------------------------------------------------------------------------
  # Ideal model seems to be the full model
  #------------------------------------------------------------------------------
  ideal_model <- models6[[1]]
  plot(ideal_model,1)
  
  ideal_model.cooks = cooks.distance(ideal_model)
  
  # Cook's distances
  plot(ideal_model.cooks, type="h", lwd=2,
       xlab="Observation index",
       ylab="Cook's distances",
       main="Cook's distances for 'LOS' ")
  abline(h=1,lty=2,col="red")
  
  #------------------------------------------------------------------------------
  # No influential observations. Expected from size of data array. 
  # Concern: lack of fit.
  #------------------------------------------------------------------------------
