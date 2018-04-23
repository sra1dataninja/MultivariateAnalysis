# Script file for Module 1 Week 1

# The following a basic R function that will 
# work without the HH package

x <- 1:20 
dummy <- data.frame(x=x, y= x + rnorm(20)) 
# dummy Make a data frame of two columns, 
# x and y

dummy
summary(dummy)
plot(y ~ x, data=dummy)

fm <- lm(y ~ x, data=dummy)
fm
summary(fm) 

# below is code that will require the HH package
install.packages("HH")
library(HH)

# The uscrime data file is explained in detail 
# in exercise 4.3

data(uscrime)
head(uscrime)
tail(uscrime)
summary(uscrime)

uscrime$S_factor <- as.factor(uscrime$S)
summary(uscrime)
summary(uscrime$S_factor)
summary(uscrime$S)

# boxplot of crime rate with 
# Southern/ Non-Southern states
# using wrong variable
bwplot(R ~ S, data=uscrime, 
       ylab=list("Crime Rate"),
       xlab=list("Southern States"))

#using correct variable
bwplot(R ~ S_factor, data=uscrime,  
       ylab=list("Crime Rate"),
       xlab=list("Southern State"))

uscrime.aov <- aov(R ~ S_factor, data=uscrime) 
# need to use S_factor variable
anova(uscrime.aov)
model.tables(uscrime.aov, "means")
uscrime.mmc <- mmc(uscrime.aov, 
                   linfct = mcp(S_factor = 
                                  "Tukey")) 
uscrime.mmc
mmcplot(uscrime.mmc, style = "both")

hovBF(R ~ S_factor, data=uscrime)
# Homogeneity of Variance test 

hovplotBF(R ~ S_factor, data=uscrime)
# Homogeneity of Variance plot 

#ANOVA example of differences
bwplot(Ed ~ S_factor, data=uscrime,
       panel=panel.bwplot.superpose, groups=S,
       ylab=list("Education"),
       xlab=list("Southern State"))
uscrime_Ed.aov <- aov(Ed ~ S_factor, data=uscrime)
anova(uscrime_Ed.aov)

model.tables(uscrime_Ed.aov, "means")
uscrime_Ed.mmc <- mmc(uscrime_Ed.aov, 
                      linfct = mcp(S_factor 
                                   = "Tukey")) 
uscrime_Ed.mmc
mmcplot(uscrime_Ed.mmc, style = "both")

hovBF(Ed ~ S_factor, data=uscrime)
hovplotBF(Ed ~ S_factor, data=uscrime)

