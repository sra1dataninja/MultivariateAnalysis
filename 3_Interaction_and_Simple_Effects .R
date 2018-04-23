# Script file for Interaction and Simple Effects

library(HH) 
data(filmcoat) 
head(filmcoat)
summary(filmcoat)

# 2 way interaction plot 
interaction2wt(data=filmcoat, coat ~ 
                 temprt + pressure, 
               xlim=c(.5, 3.5), 
               between=list(x=.5, y=.5))

# ANOVA with interaction
film.aov1 <- aov(coat ~ temprt * pressure,
                 data=filmcoat) 
summary(film.aov1)

# table of means
model.tables(film.aov1, type = "means")

# 2 way interaction plot  
# displaying simple effects 
interaction2wt(data=filmcoat, coat ~ 
                 temprt + pressure,
               simple=TRUE, 
               simple.scale=list(temprt=.3, 
                                 pressure=.3), 
               xlim=c(.5, 3.5), 
               between=list(x=.5, y=.5))

# separate ANOVA for each pressure level
filmcoat.aov.3p <- sapply(levels(
  filmcoat$pressure),
  function(i) aov(coat ~ temprt, data = filmcoat, 
      subset=(pressure==i)),
      simplify=FALSE) 

print(lapply(filmcoat.aov.3p, anova))

# Adjustments needed for Multiple Comparisons
ResidMS <- function(x) 
  summary(x)[[1]]["Residuals","Mean Sq"] 

ResidMSAvg <- ResidMS(film.aov1)
ResidMSAvg

crit.val <- qtukey(p = 0.95, nmeans = 3, 
                   df = 18, nranges = 3)/sqrt(2)
crit.val

# Multiple Comparison Results by pressure 
filmcoat.mmc.3pc <- sapply(
  filmcoat.aov.3p, simplify = FALSE, 
  function(x) mmc(x, 
                  calpha = crit.val 
                  * sqrt(ResidMSAvg/ResidMS(x)))) 
filmcoat.mmc.3pc

# filmcoat.mmc.3pc graphic
mmc3pb <- sapply(filmcoat.mmc.3pc, 
                 mmcplot, style="both", 
                 simplify=FALSE, axis.right=2, 
                 xlim=c(-14, 14), ylim=c(33, 46), 
                 ylab.right=NULL, ylab=NULL)

# mmc3pb
mmc3pb[[1]]$condlevels[[1]][1] <- 
  names(mmc3pb)[1]
mmc3pb[[2]]$condlevels[[1]][1] <- 
  names(mmc3pb)[2]
mmc3pb[[3]]$condlevels[[1]][1] <- 
  names(mmc3pb)[3] 
old.digits <- options(digits=4)

# Prints each level of pressure individually 
print(mmc3pb[[1]])
print(mmc3pb[[2]])
print(mmc3pb[[3]])




# separate ANOVA for each temperature level
filmcoat.aov.3t <- sapply(levels(
  filmcoat$temprt),
  function(i) aov(coat ~ pressure, 
                  data = filmcoat, 
                  subset=(temprt==i)),
  simplify=FALSE) 

print(lapply(filmcoat.aov.3t, anova))

# Adjustments needed for Multiple Comparisons
# This is the same as above
ResidMS <- function(x) 
  summary(x)[[1]]["Residuals","Mean Sq"] 

ResidMSAvg <- ResidMS(film.aov1)
ResidMSAvg

crit.val <- qtukey(p = 0.95, nmeans = 3, 
                   df = 18, nranges = 3)/sqrt(2)
crit.val

# Multiple Comparison Results by temperature 
filmcoat.mmc.3tc <- sapply(
  filmcoat.aov.3t, simplify = FALSE, 
  function(x) mmc(x, 
                  calpha = crit.val 
                  * sqrt(ResidMSAvg/ResidMS(x)))) 
filmcoat.mmc.3tc

# filmcoat.mmc.3tc graphic
mmc3tb <- sapply(filmcoat.mmc.3tc, 
                 mmcplot, style="both", 
                 simplify=FALSE, axis.right=2, 
                 xlim=c(-14, 14), ylim=c(33, 46), 
                 ylab.right=NULL, ylab=NULL)

# mmc3tb
mmc3tb[[1]]$condlevels[[1]][1] <- 
  names(mmc3tb)[1]
mmc3tb[[2]]$condlevels[[1]][1] <- 
  names(mmc3tb)[2]
mmc3tb[[3]]$condlevels[[1]][1] <- 
  names(mmc3tb)[3] 
old.digits <- options(digits=4)

# Prints each level of pressure individually 
print(mmc3tb[[1]])
print(mmc3tb[[2]])
print(mmc3tb[[3]])

