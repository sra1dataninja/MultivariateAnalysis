##The uscrime data HW comments

library(HH)
data(uscrime)
head(uscrime)
summary(uscrime)

uscrime$S <- as.factor(uscrime$S)
summary(uscrime$S)

##splom with adjustments - good display
subset_uscrime <- uscrime[c(1,2,4:14)]
head(subset_uscrime)

splom(subset_uscrime, axis.text.cex=.5, varname.cex=.8, xlab=NULL,
      pch=16, group=uscrime$S, panel=panel.superpose,
      key=list(
        border=TRUE,
        text=list(c("Other", "Southern")),
        points=list(
          col=trellis.par.get("superpose.symbol")$col[1:2],
          pch=16),
        space="bottom", columns=2))

## the State variable should not be included in the regression analysis
## since this is a label (not a predictor)
uscrime.var <- uscrime[, 1:14]
head(uscrime.var)

## full regresson model
uscrime.var.lm <- lm(R ~ . , data=uscrime.var)
summary(uscrime.var.lm)

## stepwise regression
uscrime.subsets <-
  leaps::regsubsets(R ~ ., data=uscrime.var, nbest=2)
uscrime.subsets.Summary <- summaryHH(uscrime.subsets)
uscrime.subsets.Summary
tmp <- (uscrime.subsets.Summary$cp <= 10)
uscrime.subsets.Summary[tmp,]

uscrime.lm.9 <- lm.regsubsets(
  uscrime.subsets, 9)
summary(uscrime.lm.9)
vif(uscrime.lm.9)

uscrime.lm.15 <- lm.regsubsets(
  uscrime.subsets, 15)
summary(uscrime.lm.15)
vif(uscrime.lm.15)

anova(uscrime.lm.9, uscrime.lm.15)

# code for Partial Residual Plots
residual.plots.lattice(uscrime.var.lm, 
                       par.strip.text=list(
                         cex=1.1))
residual.plots.lattice(uscrime.lm.9, 
                       par.strip.text=list(
                         cex=1.1))
# Residual Plots for best model  
lmplot(uscrime.lm.9)

# Diagnostics for best model 
uscrime.lm.case <- case(uscrime.lm.9) 
uscrime.lm.trellis <-
  plot(uscrime.lm.case, uscrime.lm.9, 
       par.strip.text=list(cex=1.2), 
       layout=c(4,3), main.cex=1.6)
uscrime.lm.trellis

# In model 9, observation 29 is noted in the
# leveage but no Cooks becasue predictors
# age and x are low, while Ex0 is high 
# the value of R is not usual (by itself or 
# when considering the predictors)

uscrime.lm.9$residuals[29]
# large negative residual 
# crime rate is over estimated

uscrime.lm.9$fitted.values[29]
uscrime$R[29]
