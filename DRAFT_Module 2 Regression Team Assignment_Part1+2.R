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

## Checking model 15 - largest rsq, adrsq2 and lowest stderr and rss, but not all P values (M,U1,W) are significant and tested positive for collinearity.
uscrime15.var.lm <- lm.regsubsets(uscrime.subsets, 15)
summary(uscrime.lm.15)
vif(uscrime.lm.15)

##Checking model 9 - ALL P-values are Significant and no indication of collinearity
uscrime9.var.lm <- lm( R ~
                         Age + Ed + Ex0 +
                         U2 + X, 
                       data=uscrime.var)
summary(uscrime9.var.lm) 
vif(uscrime9.var.lm)


# QUESTION 2)
# Partial Residual Plots for model 
residual.plots.lattice(
  uscrime9.lm, par.strip.text=list(cex=1.1))

# Residual Plots for reduced model
lmplot(uscrime9.lm) 


