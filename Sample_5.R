#Sometimes we may not want to run a linear model! Sometimes we may suspect that our variables do not progress linearly. 

#We are going to run a multiple linear probability model.  

#But first, we are going to run a multiple (binary) logistic model.  

d = read.csv(file.choose()) #Using GSS file. 

#The question we are analyzing is: What are some things that predict "How often do you come home from work exhausted?" 
#Answers range from always to never.

sub <- d[, c("xhaustn", "hrs1", "age", "prestg80", "babies", "wrkstat")] #We are sub-setting the data for just the variables we want to use. 

sub <- na.omit(sub) #We are getting rid of the N/As.

sub$exh = ifelse(sub$xhaustn==1, 1, 0) #We are going to re-code to look only at "always exhausted" (1) vs. everything else (0).

table(sub$exh) #Check re-code. 

lm1 = lm(exh ~ hrs1 + age + prestg80 + babies, sub, subset= wrkstat==1) #Linear regression w/ multiple predictive variables, sub-set to full-time workers. 
summary(lm1)

#For each one-unit increase in hours worked, exhaustion increases by an average of 0.004 points for full-time workers, net of other factors. 
#For each one-unit increase in prestige, exhaustion dncreases by an average of 0.002 points for full-time workers, net of other factors. 
#For each one-unit increase in babies, exhaustion increases by an average of 0.054 points for full-time workers, net of other factors. 


#Running a multiple (binary) logistic model:

logit1 = glm(exh ~ hrs1 + age + prestg80 + babies, sub, subset= wrkstat==1, family=binomial)
summary(logit1)

#When we interpret, we do so on the logit scale.
#For example, for each one-unit increase in babies, there is an average LOGIT increase of 0.402 points in exhaustion for full-time workers. 

#For substantive interpretation, we can get odds-ratios.  

exp(coef(logit1))

#To transform odds ratios into predicted probabilities:
#We subtract 1 and multiply by 100. E.g., (1-0.065)*100=93.5%.
#Odds-ratios >1 means the odds increase, <1 means that odds decrease. 

#We can also get predicted probabilities for some constellations of X values. 

predict(logit1, type = "response", newdata = data.frame(hrs1 = c(35,80), age = c(35, 35), prestg80 = c(50, 50), babies = c(0,0)))
#Predicted probabilities for "exh" for respondents who work between 35 and 80 hrs1, are 35 y/o, have a prestige score of 50, and have 0 babies. 

predict(logit1,  type = "response", newdata = data.frame( hrs1 = c(45,45), age = c(35, 35), prestg80 = c(50, 50), babies = c(0,2)))
#For people who work between 45 hrs, are 35 y/o, have a prestige score of 50, and have between 0 and 2 babies. 

predict(logit1,  type = "response", newdata = data.frame( hrs1 = c(35,60), age = c(20, 50), prestg80 = c(80, 40), babies = c(0,2)))
#For people who work between 35 and 60 hrs, are between the ages of 20 and 50, have prestige scores between 40 and 80, and have between 0 to 2 babies. 

#The below will return predicted probabilities for any combination of variables w/ everything else set to means. 

pred.dat <- with(sub, expand.grid( 
  hrs1 = sort(unique(hrs1)),
  age = mean(age),
  prestg80 = mean(prestg80),
  babies = sort(unique(babies))))


#This function is from QMSS package. Run: 

#' Predicted probabilities and confidence intervals from logit or probit model
#'
#' @param model A \code{\link[stats]{glm}} model fit with \code{binomial} family and 
#' either a \code{logit} or \code{probit} link function.
#' @param predData A data frame to pass to \code{\link[stats]{predict.glm}} in which to look 
#' for variables with which to predict. 
#' @param ci Logical value indicating whether to compute confidence intervals. 
#' @param level The confidence level to use if \code{ci} is \code{TRUE}. 
#' @return A data frame with \code{predData} and the associated predicted probabilities. 
#' Confidence intervals are included if argument \code{ci} is \code{TRUE}. 
#' @author Jonah Gabry <jsg2201@@columbia.edu>
#' @export
#' @examples
#' GSS_2010$Y <- with(GSS_2010, 
#'                    cut(realinc, 
#'                    breaks=c(-Inf, median(realinc, na.rm = T), Inf), 
#'                    labels=c("Low", "High")))
#' logitmodel <- glm(Y ~ age + educ, data = GSS_2010, family = binomial)
#' probitmodel <- glm(Y ~ age + educ, data = GSS_2010, family = binomial(link = "probit"))
#' predData <- data.frame(age = 20, educ = 15)
#' predProb(logitmodel, predData, ci = F)
#' predProb(probitmodel, predData, ci= F)
#' predData <- expand.grid(age = c(20, 50, 80), educ = c(5, 10, 15))
#' predProb(logitmodel, predData, ci = T)
#' predProb(probitmodel, predData, ci= T)

predProb <- function(model, predData, ci = TRUE, level = 0.95){
  
  link <- model$family$link
  bad_link <- !(link %in% c("logit", "probit"))
  
  if (bad_link) {
    stop("Link function should be 'logit' or 'probit'")
  }
  
  fun <- ifelse(link == "probit", "pnorm", "plogis")
  
  if (ci == FALSE){
    preds <- predict(model, type = "response", newdata = predData)
    preds <- cbind(predData, PredictedProb = preds)
    return(preds)
  }
  else {
    temp <- predict(model, type = "link", se = TRUE, newdata = predData)
    fit <- temp$fit
    se <- temp$se.fit
    p <- (1 - level)/2
    p <- c(p, 1-p)
    PredictedProb <- do.call(fun, args = list(q = fit))
    ci1 <- do.call(fun, args = list(q = fit + qnorm(p[1])*se))
    ci2 <- do.call(fun, args = list(q = fit + qnorm(p[2])*se))
    CI <- cbind(ci1, ci2)
    colnames(CI) <- paste0(paste(100*p), "%")
    preds <- cbind(predData, PredictedProb, CI)
    return(preds)
  }
}

predProb(logit1, predData = pred.dat, ci = F) #Another way to get predicted probabilities. 

#Now, visualize your probabilities:

sub$b = as.factor(sub$babies)
logit2 = glm(exh ~ hrs1 + age + prestg80 + b, sub, subset= wrkstat==1 & babies<3, family=binomial)
summary(logit2)

install.packages("visreg")
library(visreg)
visreg(logit2, "hrs1", by = "b", 
       partial = F, overlay = T, 
       xlab = "Hours", 
       ylab = "Predicted probability", 
       scale= "response",
       type="conditional",
       alpha=.05) ## 
