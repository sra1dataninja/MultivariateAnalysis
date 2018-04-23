##Module Assignment  prep work
Scatter plot of predicted crime rate vs Age
plot(uscrime.var[,"Age"], uscrime.var[,"R"], 
     ylab="", xlab="", 
     xlim=c(120,160),ylim=c(20,250))
mtext("Age", line=3, cex=1.2, side=1) 
mtext("R", line=3, cex=1.2, side=2)


# draw the regression line between Age and reported crime rate

regr1.plot(uscrime.var[,"Age"], uscrime.var[,"R"], 
           xlim=c(120,160),ylim=c(20,250),main="",
           ylab="", xlab=""
)
mtext("Age", line=3, cex=1.2, side=1) 
mtext("R", line=3, cex=1.2, side=2)



# draw the regression line

regr1.plot(houseprice[,"sqft"], 
           houseprice[,"price"], 
           xlim=c(1000,3500),ylim=c(500,2100), main="",  
           ylab="", xlab="")
mtext("sqft", line=3, cex=1.2, side=1) 
mtext("price", line=3, cex=1.2, side=2)



# Scatter plot of predicted crime rate vs EX0
plot(uscrime.var[,"EX0"], uscrime.var[,"R"], 
     ylab="", xlab="", 
     xlim=c(50,125),ylim=c(20,250))
mtext("EX0", line=3, cex=1.2, side=1) 
mtext("R", line=3, cex=1.2, side=2)




uscrime.var.lm <- lm(Age ~ M, data = uscrime.var)
summary(uscrime.var.lm)
vif(uscrime.var.lm)