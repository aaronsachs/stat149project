
############# FUNCTIONS #################
############# ############# ############# 
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
    
    p.value[i] <-  anova.results[length(results)][2, ]
    tie.breaker[i] <- anova.results[length(results) -1][2, ]
    
  }
  
  print(p.value)
  # if only na left
  if (length(p.value[!is.na(p.value)]) == 0){
    print('null is left')
    return(current.formula)
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
    returning = list("formula" = formula, "p.value" = min(p.value, na.rm =TRUE), "predictors" = predictors )
    
    return(returning)
  }
}
  
# best_model_me <- function(dependent.name, predictors, data, test = "Chisq"){
#   tilde = '~'
#   formula.start <- paste(dependent.name, tilde,  sep = " ")
#   formula.null <- paste(dependent.name, tilde, "1",  sep = " ")
#   print(formula.null)
#   
#   null <- 0
#   p.value <- .01
#   null.model <- lm(eval(formula.null), data = data)
#   
#   while (p.value < .05){
#     if (null == 0){
#       null.results <- next_best(formula.null, predictors, data)
#       current.model <- null.results$formula
#       current.predictors <- null.results$predictors
#       p.value <- null.results$p.value
#       null <- 1
#     }else{
#       current.results <- next_best(current.model, current.predictors, data)
#       
#       if (length(current.results) < 3){
#         break()
#       }else{
#         current.model <- current.results$formula
#         current.predictors <- current.results$predictors
#         p.value <- null.results$p.value
#         print(current.model)
#       }
#     }
#   }
#   return(current.results)
# }

  
#dependent.name = 'LOS.ordinal'
#predictors <- c('DIAGNOSIS', 'SEX', 'DRG', 'DIED', 'scale(CHARGES)', 'AGE')

#best_model_me(dependent.name, predictors, ami.od)
  
resid_plot <- function(model){
  # diagnostics
  fitted <- fitted(model)
  devresid <- residuals(model, type="deviance")
  
  # Residual plot (deviance residuals)
  plot(fitted, devresid,
       xlab="Fitted values",
       ylab="Deviance residuals",
       pch=19, col="red", cex=1.5,
       main="Fitted vs deviance residual plot")
  abline(h=0,lty=2,col="green")
}

cooks_plot <- function(model){
  
  cooks <- cooks.distance(model)
  
  # Cooks distances
  plot(cooks, type="h", lwd=2,
       xlab="Observation index",
       ylab="Cook's distances",
       main="Cook's distances for Divorce Interaction Model")
  abline(h=1,lty=2,col="red")
  
}
