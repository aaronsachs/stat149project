
if (!require(aod)) {install.packages("aod"); require(aod)}

############# FUNCTIONS #################
############# ############# ############# 

# missing value 
na.convert.mean = function(frame) {
  vars <- names(frame)
  if (!is.null(resp <- attr(attr(frame, "terms"), "response"))) {
    vars <- vars[-resp]
    x <- frame[[resp]]
    pos <- is.na(x)
    if (any(pos)) {
      frame <- frame[!pos, , drop = FALSE]
      warning(paste(sum(pos), "observations omitted due to missing values in the response"))
    }
  }
  for (j in vars) {  #j is variable names
    x <- frame[[j]]
    pos <- is.na(x)
    if (any(pos)) {
      if (length(levels(x))) {   # factors
        xx <- as.character(x)
        xx[pos] <- "NA"
        x <- factor(xx, exclude = NULL)
      }
      else if (is.matrix(x)) {   # matrices
        ats <- attributes(x)
        x.na <- 1*pos
        #               x[pos] <- 0
        w <- !pos
        n <- nrow(x)
        TT <- array(1, c(1, n))
        xbar <- (TT %*% x)/(TT %*% w)
        xbar <- t(TT) %*% xbar
        x[pos] <- xbar[pos]
        attributes(x) <- ats
        attributes(x.na) <- ats
        dimnames(x.na)[[2]]=paste(dimnames(x)[[2]],".na",sep='')
        frame[[paste(j,".na",sep='')]] <- x.na 
      } else {   # ordinary numerical vector
        ats <- attributes(x)
        x[pos] <- mean(x[!pos])
        #               x[pos] <- 0
        x.na <- 1*pos
        frame[[paste(j,".na",sep='')]] <- x.na 
        attributes(x) <- ats
      }
      frame[[j]] <- x
    }
  }
  frame
}

# data loading
load.data <- function(path){
  print(paste(path, 'amidata.csv', sep = ''))
  ami <- read.csv(paste(path, 'amidata.csv', sep =''))
  
  ami <- ami[!ami$LOS == 0, ]
  
  # drop low values of charges
  ami <- ami[(!ami$CHARGES < 300) | (is.na(ami$CHARGES)),]
  
  ami$LOGCHARGES <- log(ami$CHARGES)
  
  # drop died, and charges
  ami$DIED <- NULL

  ami <- na.convert.median(ami)
  print(str(ami))
  # turn specific columns to factors
  factor.columns <- c("Patient", "DIAGNOSIS", 'SEX', 'DRG', 'LOGCHARGES.na')
  ami[, factor.columns] <- data.frame(apply(ami[factor.columns], 2, as.factor))
  
  return(ami)
}

# path <- '/Users/jeanettejin/stat149project/'
# str(load.data(path))

# data loading
load.data.show <- function(path){
  print(paste(path, 'amidata.csv', sep = ''))
  ami <- read.csv(paste(path, 'amidata.csv', sep =''))
  
  ami <- ami[!ami$LOS == 0, ]
  
  
  ami$LOGCHARGES <- log(ami$CHARGES)
  
  # drop died, and charges
  ami$DIED <- NULL
  
  ami <- na.convert.mean(ami)
  print(str(ami))
  # turn specific columns to factors
  factor.columns <- c("Patient", "DIAGNOSIS", 'SEX', 'DRG', 'LOGCHARGES.na')
  ami[, factor.columns] <- data.frame(apply(ami[factor.columns], 2, as.factor))
  
  return(ami)
}

# data loading
prediction.load.data <- function(path){
  set.seed(101) 
  # load in clean version
  ami <- load.data(path)
  
  ami$DRG <- NULL
  ami$LOGCHARGES <- NULL
  ami$LOGCHARGES.na <- NULL
  
  # subset train and test
  test.ind <- sample(seq_len(nrow(ami)), size = 3000)
  ami.test <- ami[test.ind, ]
  ami.train <- ami[-test.ind, ]
  
  # return train and test sets
  returning = list("train" = ami.train, 'test' = ami.test)
  return(returning)
  
}

# select next best predictor
next_best <- function(model_str1, model_str2, 
                      current.formula, predictors,
                      data, test = 'Chisq', tie_max_better = TRUE){
  # init
  anova <- c()
  p.value <- c()
  formulas <- c()
  tie.breaker <- c()
  
  # fit H_0 model
  current.model.str <- paste(model_str1, current.formula, model_str2, sep ="")
  current.model <- eval(parse(text = current.model.str))
  
  # fit all the possible H_A model
  for (i in 1:length(predictors)){
    formulas[i] <- paste(current.formula, predictors[i], sep = ' + ')
    next.model.str <- paste(model_str1, formulas[i], model_str2, sep ="")
    
    next.model <- eval(parse(text = next.model.str))
    anova.results <- anova(current.model, next.model, test = test)
    print(anova.results)
    
    p.value[i] <-  anova.results[length(anova.results)][2, ]
    tie.breaker[i] <- anova.results[length(anova.results) - 1][2, ]
  }
  
  
  # if only na left
  if (length(p.value[!is.na(p.value)]) == 0){
    print('nothing is left')
    return(next.model)
  }else{
    index <- which(p.value == min(p.value, na.rm = TRUE))
    
    # if tie
    if (length(index) > 1){
      print('Multiple models comparisons yield the same p value')
      # if better for tie breaker to be max
      if (tie_max_better == 1){
        index <- which(tie.breaker == max(tie.breaker, na.rm = TRUE))
      } else{ ### else take the min
        index <- which(tie.breaker == min(tie.breaker, na.rm = TRUE))
      }
    }
    
    # index 
    formula <- formulas[index]
    predictors <- predictors[predictors != predictors[index]] 
    p.value.report <- p.value[index]
    
    returning = list("formula" = formula, "p.value" = p.value.report , "predictors" = predictors)
    return(returning)
  }
}


# analysis of deviance automated
best_model <- function(model_str1, model_str2, dependent.name, predictors, data, test = "Chisq"){
  tilde = '~'
  formula.start <- paste(dependent.name, tilde,  sep = " ")
  formula.null <- paste(dependent.name, tilde, "1",  sep = " ")
  message(paste('The null formula is ', formula.null))
  
  null <- 0
  p.value <- .01
  
  while (p.value < .05){
    if (null == 0){
      message('Comparing the null model against one independent variable')
      null.results <- next_best(model_str1, model_str2, formula.null, predictors, data, test)
      current.model <- null.results$formula
      current.predictors <- null.results$predictors
      p.value <- null.results$p.value
      null <- 1
    }else{
      message(paste('The current model is', current.model))
      
      current.results <- next_best(model_str1, model_str2, current.model, current.predictors, data)
      
      current.model <- current.results$formula
      current.predictors <- current.results$predictors
      p.value <- current.results$p.value
      
      if (length(current.predictors) == 0){
        
        if (length(p.value) < 1){
          return(current.model)
        } else{
          print(p.value)
          if(p.value < .05){
            
            return(current.model)
          }else{
            return(past.model)
          }
        }
      }
      past.model <- current.model
      next
    }
  }
  return(current.model)
}

  
####### usage #########  

# # load in data
# ami <- read.csv('amidata.csv')
# 
# # turn specific columns to factors
# factor.columns <- c("Patient", "DIAGNOSIS", 'SEX', 'DRG', 'DIED')
# ami[, factor.columns] <- data.frame(apply(ami[factor.columns], 2, as.factor))
# 
# # make na dataframe
# ami.od <- ami
# ami.od[ami.od$LOS == 0, ]$LOS <- 1
# ami.od.na <- na.convert.mean(ami)
# 
# # make into categories
# ami.od.na$LOS.ordinal <- cut(ami.od.na$LOS ,breaks=c(0,10,20,25,100),
#                              labels=c("low","low-med","med","high"),ordered=T)
# 
# model_str1 <- 'polr('
# model_str2 <- ', Hess = T, data = ami.od.na)'
# predictors <- c('DIAGNOSIS' , 'SEX', 'DRG', 'DIED', 'scale(CHARGES)' , 'AGE', 'CHARGES.na' )
# dependent.name <- "LOS.ordinal"
# 
# # find the best model
# best <- best_model(model_str1, model_str2, dependent.name, predictors, data = ami.od.na)



consider_predictors <- function(model.str1, model.str2, current.formula, predictors.list, test = "Chisq"){
  base.model.str <- paste(model.str1, current.formula, model.str2)
  base.model <- eval(parse(text = base.model.str))
  continue <- 1
  
  while (continue  == 1){
    formulas <- c()
    p.value <- c()
    
    for (i in 1:length(predictors.list)){
      formulas[i] <- paste(current.formula, predictors.list[i], sep = ' + ')
      next.model.str <- paste(model.str1, formulas[i], model.str2, sep ="")
      
      next.model <- eval(parse(text = next.model.str))
      anova.results <- anova(base.model, next.model, test = test)
      print(anova.results)
      p.value[i] <-  anova.results[length(anova.results)][2, ]
    }
    index <- which(p.value == min(p.value, na.rm = TRUE))
    
    if (p.value[index] > .05){
      continue <- 0
      break 
    }else{
      base.model.str <- paste(model.str1, formulas[index], model.str2, sep ="")
      base.model <- eval(parse(text = base.model.str))
      current.formula <- formulas[index]
      predictors.list <- predictors.list[predictors.list != predictors.list[index]] 
      
      if (length(predictors.list) == 0){
        continue <- 0
        break
      }
    }
  }
  return(current.formula)
}



# ### USAGE
# model.str1 <- 'glm('
# model.str2 <- ',  poisson(link = "log"), data = ami)'
# current.formula <- 'LOS ~ DIAGNOSIS + SEX + DRG + LOGCHARGES + AGE'
# predictors.list <-  c('DIAGNOSIS:DRG','DIAGNOSIS:SEX','DIAGNOSIS:LOGCHARGES.na',
#                       'DRG:SEX', 'DRG:LOGCHARGES.na', 'SEX:LOGCHARGES.na',
#                       'LOGCHARGES:DIAGNOSIS', 'LOGCHARGES:SEX', 'LOGCHARGES:DRG',
#                       'AGE:DIAGNOSIS')
# 
# 
# poisson.bm.inter.str <- consider_predictors(model.str1, model.str2, current.formula, predictors.list)
# poisson.bm.inter <- glm(poisson.bm.inter.str, poisson(link = "log"), data = ami)
# 



############

# residual plot 
resid_plot <- function(model, model_name){
  # diagnostics
  fitted <- fitted(model)
  devresid <- residuals(model, type="deviance")
  
  # Residual plot (deviance residuals)
  plot(fitted, devresid,
       xlab="Fitted values",
       ylab="Deviance residuals",
       pch=19, col="red", cex=1.5)
  abline(h=0,lty=2,col="green")
  title(paste("Fitted vs. Dev Residual", model_name), line = -2)
}

# cooks plot
cooks_plot <- function(model, model_name){
  
  cooks <- cooks.distance(model)
  
  # Cooks distances
  plot(cooks, type="h", lwd=2,
       xlab="Observation index",
       ylab="Cook's distances")
  abline(h=1,lty=2,col="red")
  title(paste("Cook's distances", model_name), line = -2)
  
}

jacks_plot <- function(model, model_name){
  fitted <- fitted(model)
  jresid <- rstudent(model)
  
  plot(fitted, jresid,
       xlab="Fitted probabilities",
       ylab="Jackknifed residuals",
       pch=19, col="red", cex=1.5)
  abline(h=0,lty=2,col="green")
  title(paste("Fitted vs jackknifed", model_name), line = -2)
}


interact.plot <- function(cat1, cat2){
  
  cat1 <- cat1
  cat2 <- cat2
  
  ami %>% 
    group_by( eval(parse(text = cat1)), eval(parse(text = cat2))) %>% 
    summarise(mean_los = mean(LOS)) -> inter.val
  
  colnames(inter.val)[1] <- cat1
  colnames(inter.val)[2] <- cat2
  
  inter.val %>% 
    ggplot() + 
    aes(x = eval(parse(text = cat1)), y = mean_los, color = eval(parse(text = cat2))) +
    geom_line(aes(group = eval(parse(text = cat2)))) +
    geom_point() +  xlab(cat1) + labs(color=cat2) + labs(title = paste(cat1, cat2))
  
}

diagnostic_plots <- function(model, model_name){
  par(mfrow=c(1,3))
  resid_plot(model, '')
  cooks_plot(model, '')
  jacks_plot(model, '')
  mtext(model_name, side = 3, line = -3, outer = TRUE)
}
