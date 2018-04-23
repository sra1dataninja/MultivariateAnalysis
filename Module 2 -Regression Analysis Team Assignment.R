library(HH)
data(uscrime)
head(uscrime)
summary(uscrime)

uscrime.var

uscrime$S <- factor(uscrime$S,
                    levels = c(0,1),
                    labels = c("Non- Southern", "Southern state"))

summary(uscrime$S)

# the State variable should not be included in the regression analysis
# since this is a label (not a predictor)
uscrime.var <- uscrime[, 1:14] ## removes column 15 the state variable
head(uscrime.var)

# for your regression model remember to have data=uscrime.var
# Step 1: Create a scatter plot to understand the linear relationships between various predictors and our response variable
# i.e. reported crime rate
splom( ~ uscrime.var, pch=16, 
      pscales=2, 
      varname.cex=.8, 
      axis.text.cex=.5)

## After lookint at teh scatter plot it can be seen that Ex0 & Ex1 are both positive linear relationship
## Variable W & X  have negative relationshio. As wealth increases reported crime rates decreases.


# Step 2: Full regression model - This is to predict our response variable using all predcitors.
uscrime.var.lm <- lm( R ~ ., data=uscrime.var) 
summary(uscrime.var.lm)
vif(uscrime.var.lm)

## By reviewing the Output of coefficients of full regression model, it can be seen that Age, Ed, U2 & X varioables 
# has p values les than 0.05 and are significant.
## Adjusted R squared value is 67.83 which means we can explain the for all 13 predcitors.

# Step 3: Verify the 3D plot 

# 3D plot of Reported crimes R ~ Ex0 + Ex1 
# this graph depicts the least squares with two highly collinear variables Ex0 & Ex1
regr2.plot(uscrime.var[,"Ex0"],	xlab="X",
           uscrime.var[,"Ex1"], ylab="Y", 
           uscrime.var[,"R"], zlab="R", 
           resid.plot="square",
           theta=-40, phi=25, r=sqrt(3), 
           box=is.R(),
           plot.back.planes=FALSE, 
           plot.base.plane=FALSE,
           main="Least squares with two highly 
           collinear X-variables")

regr2.plot(uscrime.var[,"W"],	xlab="X",
           uscrime.var[,"X"], ylab="Y", 
           uscrime.var[,"R"], zlab="R", 
           resid.plot="square",
           theta=-40, phi=25, r=sqrt(3), 
           box=is.R(),
           plot.back.planes=FALSE, 
           plot.base.plane=FALSE,
           main="Least squares with two highly 
           collinear X-variables")

######################################## Option 1 Start ###########################################

## Starting of manual regression model
# Step 4: Partial regression model and this by exlcuding Ex1 varaible 

uscrime.var2.lm <- lm( R ~ Age + S + Ed + Ex0 + LF + M + N + NW + U1 + U2 + W + X, data=uscrime.var) 
summary(uscrime.var2.lm)
vif(uscrime.var2.lm)

# Step 5: Partial regression model and this by excluding Ex1 and S

uscrime.var3.lm <- lm( R ~  Age + Ed + Ex0 + LF + M + N + NW + U1 + U2 + W+  X, data=uscrime.var) 
summary(uscrime.var3.lm)
vif(uscrime.var3.lm)

# Step 7: Partial regression model and this by excluding Ex1,S  & Lf

uscrime.var4.lm <- lm( R ~  Age + Ed + Ex0 + Ed + M + N  + NW + U1 + U2 + W+  X, data=uscrime.var) 
summary(uscrime.var4.lm)
vif(uscrime.var4.lm)


# Step 8: Partial regression model and this by excluding Ex1,S,Lf & U1

uscrime.var5.lm <- lm( R ~  Age + Ed + Ex0  + M + N  + NW  + U2 + W + X, data=uscrime.var) 
summary(uscrime.var5.lm)
vif(uscrime.var5.lm)


# Step 9: Partial regression model and this by excluding Ex1,S,Lf, U1, N & W ( Removing two variables int this model)

uscrime.var6.lm <- lm( R ~  Age + Ed + Ex0  + M  + NW + U2  +  X, data=uscrime.var) 
summary(uscrime.var6.lm)
vif(uscrime.var6.lm)

# Step 10: Partial regression model and this by excluding Ex1,S,Lf, U1, N, W & NW

uscrime.var7.lm <- lm( R ~  Age + Ed + Ex0 + M + U2  + X, data=uscrime.var) 
summary(uscrime.var7.lm)
vif(uscrime.var7.lm)

# Step 11: Partial regression model and this by excluding Ex1,S,Lf, U1, N, W,  NW & M

uscrime.var8.lm <- lm( R ~  Age + Ed + Ex0 + U2  + X, data=uscrime.var) 
summary(uscrime.var8.lm)
vif(uscrime.var8.lm)


# Step 12: Partial regression model and this by excluding Ex1,S,Lf, U1, N, W, NW, M & U2

uscrime.var9.lm <- lm( R ~  Age + Ed + Ex0 +  X, data=uscrime.var) 
summary(uscrime.var9.lm)
vif(uscrime.var9.lm)

# Step 13: Partial regression model and this by excluding Ex1,S,Lf, W, N, NW, M, U1, U2, W & Age

uscrime.var10.lm <- lm( R ~    Ed + Ex0  +  X, data=uscrime.var) 
summary(uscrime.var10.lm)
vif(uscrime.var10.lm)

## End of manual regression model


## Automated model
uscrime.var.subsets <-
  leaps::regsubsets(R ~ .,
                    data=uscrime.var, nbest=2) 

uscrime.var.subsets.Summary <- 
  summaryHH(uscrime.var.subsets) 
uscrime.var.subsets.Summary


### Show the top 10 best models by Cp static
tmp <- (uscrime.var.subsets.Summary$cp <= 10) 
uscrime.var.subsets.Summary[tmp,]

plot(uscrime.var.subsets.Summary[tmp,], 
     statistic='cp', legend=FALSE)

##### After looking through the models it can be confirmed that the model 8 with predictors Age, Ed, E0, U2 & X 
###from the manual model seems best. Even though automated model suggested that the model#11 with predcitors
## Age, Ed, Ex0, U2, W & X with low Cp value but we still see that predictor W & X are highly correalted.
### Hence it is suggested to go with the model uscrime.var8.lm of manual model
### We could also see the same in automated model i.e. model #7 with Cp value as 3.66 and Standard Deviation of 21.3 


######################################## Option 1 End ###########################################



######################################## Option 2 Start #########################################

## Full regression model

uscrime.var.lm <- lm( R ~ ., data=uscrime.var) 
summary(uscrime.var.lm)
vif(uscrime.var.lm)

## Model that was selected as part of step wise manual regression and this will be considered for residual plots
uscrime.var8.lm <- lm( R ~  Age + Ed + Ex0 + U2  + X, data=uscrime.var) 
summary(uscrime.var8.lm)
vif(uscrime.var8.lm)


## Residual plots for full model

# Partial Residual Plots for full model 
residual.plots.lattice(
  uscrime.var.lm, par.strip.text=list(cex=1.1))

# Residual Plots for reduced model
lmplot(uscrime.var8.lm)


######################################## Option 2 End  ###########################################


######################################## Option 3 Start  #########################################

# Diagnostics for reduced model 
uscrime.var8.lm.case <- case(uscrime.var8.lm) 
uscrime.var8.lm.case

uscrime.var8.lm.trellis <-
  plot(uscrime.var8.lm.case, uscrime.var8.lm, 
       par.strip.text=list(cex=1.2), 
       layout=c(4,3), main.cex=1.6)
uscrime.var8.lm.trellis

####uscrime.var8[c(3,4,6,7)]

######################################## Option 3 End ############################################

######################################## Option 4 Start  #########################################


# Prediction and Confidence Intervals
pi.fit <- predict(uscrime.var.lm, 
                  newdata=data.frame(Age=136, 
                                     S = "Non- Southern", 
                                     Ed = 109, 
                                     Ex0 = 83,
                                     Ex1 = 81,
                                     LF = 560,
                                     M = 990,
                                     N= 37,
                                     NW = 100,
                                     U1= 97,
                                     U2=35,
                                     W = 538,
                                     X = 176),
                  se.fit=TRUE, 
                  interval = "prediction")
pi.fit

ci.fit <- predict(uscrime.var.lm, 
                  newdata=data.frame(Age=136, 
                                     S = "Non- Southern", 
                                     Ed = 109, 
                                     Ex0 = 83,
                                     Ex1 = 81,
                                     LF = 560,
                                     M = 990,
                                     N= 37,
                                     NW = 100,
                                     U1= 97,
                                     U2=35,
                                     W = 538,
                                     X = 176),
                  se.fit=TRUE, 
                  interval = "confidence")
ci.fit


######################################## Option 4 End  ###########################################

