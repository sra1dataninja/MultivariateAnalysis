# Module 2 Regression Assignment (Team)

library(HH)
data(uscrime)
head(uscrime)
summary(uscrime)

uscrime$S <- as.factor(uscrime$S)
summary(uscrime$S)

# the State variable should not be included in the regression analysis
# since this is a label (not a predictor)
uscrime.var <- uscrime[, 1:14] ## removes column 15 the state variable
head(uscrime.var)

# for your regression model remember to have data=uscrime.var

# Basic Scatter Plot on full model
splom(~ uscrime.var, pch=16, 
      pscales=2, 
      varname.cex=.8, 
      axis.text.cex=.5) #Indication that Ex0 and Ex1 will have multicollinaerity because of linear relationship.

# View of full model while looking at all possible predictors
uscrime.lm <- lm( R ~ . , data=uscrime.var) 
summary(uscrime.lm) #S1, Ex0, Ex1, LF, M, N, NW, U1, W variables all have large P values
vif(uscrime.lm) #Ed, Ex0, Ex1, U1, W and X all have values over 5

# 3D plot of R ~ Ex0 + Ex1 
regr2.plot(uscrime.var[,"Ex0"],	xlab="Ex0",
           uscrime.var[,"Ex1"], ylab="Ex1", 
           uscrime.var[,"R"], zlab="Reported Crime Rate", 
           resid.plot="square",
           theta=-40, phi=25, r=sqrt(3), 
           box=is.R(),
           plot.back.planes=FALSE, 
           plot.base.plane=FALSE,
           main="Least squares with two highly 
           collinear X-variables")   #Further shows linear relationship and redundency between Ex0 and Ex1

# QUESTION 1)
# ----------------------------
# MANUAL SELECTION - BEGIN
# ----------------------------
# drop Ex1 from complete model in uscrime.lm 
uscrime2.lm <- lm( R ~
                     Age + S + Ed + Ex0 + LF + M + N + NW + U1
                   + U2 + W + X, data=uscrime.var)
summary(uscrime2.lm) #S1, LF, M, N, NW, U1, W variables all have large P values
vif(uscrime2.lm) #Ex0, U1, W and X all have values over 5

# drop Ex1 and W from complete model in uscrime.lm 
uscrime3.lm <- lm( R ~
                     Age + S + Ed + Ex0 + LF + M + N + NW + U1
                   + U2 + X, data=uscrime.var)
summary(uscrime3.lm) #S1, LF, M, N, NW, U1, W variables all have large P values
vif(uscrime3.lm) #U1 and X all have values over 5

# drop Ex1 and W from complete model in uscrime.lm 
uscrime3.lm <- lm( R ~
                     Age + S + Ed + Ex0 + LF + M + N + NW + U1
                   + U2 + X, data=uscrime.var)
summary(uscrime3.lm) #S1, LF, M, N, NW, U1, W variables all have large P values
vif(uscrime3.lm) #U1 and X all have values over 5

# drop Ex1, W and U1 from complete model in uscrime.lm 
uscrime4.lm <- lm( R ~
                     Age + S + Ed + Ex0 + LF + M + N + NW + U2 + X, data=uscrime.var)
summary(uscrime4.lm) #S1, LF, M, N, NW, W variables all have large P values
vif(uscrime4.lm) #U1 and X all have values over 5

# drop Ex1, W, U1, M from complete model in uscrime.lm 
uscrime5.lm <- lm( R ~
                     Age + S + Ed + Ex0 + LF + N + NW + U2 + X, data=uscrime.var)
summary(uscrime5.lm) #S1, LF, N, NW, W variables all have large P values
vif(uscrime5.lm) #X has value over 5

# drop Ex1, W, U1, M, N from complete model in uscrime.lm 
uscrime6.lm <- lm( R ~
                     Age + S + Ed + Ex0 + LF + NW + U2 + X, data=uscrime.var)
summary(uscrime6.lm) #S1, LF, NW, W variables all have large P values
vif(uscrime6.lm) #no variables have values over 5

# drop Ex1, W, U1, M, N, S1 from complete model in uscrime.lm 
uscrime7.lm <- lm( R ~
                     Age + Ed + Ex0 + LF + NW + U2 + X, data=uscrime.var)
summary(uscrime7.lm) #LF, NW variables all have large P values
vif(uscrime7.lm) #no variables have values over 5

# drop Ex1, W, U1, M, N, S1, NW from complete model in uscrime.lm 
uscrime8.lm <- lm( R ~
                     Age + Ed + Ex0 + LF + U2 + X, data=uscrime.var)
summary(uscrime8.lm) #LF variable has large P values
vif(uscrime8.lm) #no variables have values over 5

# drop Ex1, W, U1, M, N, S1, NW, LF from complete model in uscrime.lm 
uscrime9.lm <- lm( R ~
                     Age + Ed + Ex0 + U2 + X, data=uscrime.var)
summary(uscrime9.lm) #All remaining have small P values
vif(uscrime9.lm) #no variables have values over 5

# ----------------------------
# AUTOMATED  SELECTION - BEGIN
# ----------------------------

# stepwise regression analysis of uscrime.var data 
uscrime.var.subsets <-
  leaps::regsubsets(R ~ Age + S + Ed + 
                      Ex0 + Ex1 +
                      LF + M + N + 
                      NW + U1 + U2 +
                      W + X,
                    data=uscrime.var, nbest=2) 

uscrime.var.subsets.Summary <- 
  summaryHH(uscrime.var.subsets) 
uscrime.var.subsets.Summary

tmp <- (uscrime.var.subsets.Summary$cp <= 10) 
uscrime.var.subsets.Summary[tmp,]

plot(uscrime.var.subsets.Summary[tmp,], 
     statistic='cp', legend=FALSE) # Eliminated module 5, 8, 7 because those are over the line

##Checking model 11 - Wealth P-value is 0.09 so NOT Significant
uscrime11.var.lm <- lm( R ~
                          Age + Ed + Ex0 +
                          U2 + W + X, 
                        data=uscrime.var)
summary(uscrime11.var.lm) 
vif(uscrime11.var.lm)

##Checking model 9 - ALL P-values are Significant and no indication of collinearity
uscrime9.var.lm <- lm( R ~
                         Age + Ed + Ex0 +
                         U2 + X, 
                       data=uscrime.var)
summary(uscrime9.var.lm) 
vif(uscrime9.var.lm)

# QUESTION 2)
# Partial Residual Plots for model 
residual.plots.lattice(uscrime9.lm, par.strip.text=list(cex=1.1))

# Residual Plots for reduced model
lmplot(uscrime9.lm) 

## Option 1, Part 3 Consider the diagnostics plot and case statistics.  
## Observation 29, NY, should be outside the limits on some (but not all) case statistics on 
## the diagnostic plot.  Can your team explain why?  (Hint:  To completely answer this question 
## you will need to give details and reference variable amounts from NY in your discussion.)  
## Are there any other possible outliers?  If so, discuss them and explain why. 

# Residual Plots for reduced model
lmplot(uscrime9.lm) 
names(uscrime9.lm)


uscrime9.lm$residuals

## None of the residuals are above +50 (positive 50) or below -50 (negative 50)
## Observation 29 has the highest absolute value of 45.3438751 of all residuals.

uscrime9.lm$residuals[c(29)]
uscrime9.lm$fitted.values[c(29)]

##The observed crime rate for Observation 29 is 104.3
uscrime$R[c(29)]

## Observation 29 had observed crime rate of 104.3.  It had a predicted crime rate of 149.6439.  
## The observed crime rate is 45.3438751 below the predicted crime rate.  

##Diagnostic for best plots
## Cast Statistics
uscrime9.lm.case <- case(uscrime9.lm)
uscrime.lm.trellis <- plot(uscrime9.lm.case,uscrime9.lm, par.strip.text = list(cex=1.2), 
                           layout =c(4,3), main.cex = 1.6)
uscrime.lm.trellis



######################################## part 4 Start  #########################################


testdata <- data.frame(Age = 136, S = 0, Ed = 109, Ex0 = 83, Ex1 = 81,
                       
                       LF = 560, M = 990, N = 37, NW = 100, U1 = 97,
                       
                       U2 = 35, W = 538, X = 176)

testdata$S = as.factor(testdata$S)

# Prediction and Confidence Intervals

pi.fit <- predict(uscrime9.var.lm, newdata = testdata,
                  
                  se.fit = TRUE, interval = "prediction")

pi.fit

ci.fit <- predict(uscrime9.var.lm, newdata = testdata,
                  
                  se.fit = TRUE, interval = "confidence")

ci.fit


######################################## Part 4 End  ###########################################



