library(HH)
data(retard)
head(retard)

summary(retard)

# create factors with labels
retard$Chemical <- as.factor(retard$Chemical)
retard$Panel <- as.factor(retard$Panel)
retard$Sample <- as.factor(retard$Sample)

retard$Chemical <- factor(retard$Chemical, labels=c("C1","C2","C3"))
retard$Panel <- factor(retard$Panel, labels=c("P1","P2", "P3", "P4"))
retard$Sample <- factor(retard$Sample, labels=c("S1","S2"))

summary(retard)

# Interaction plot
interaction2wt(Time ~ Chemical + Panel + Sample, 
               data=retard, 
               par.strip.text=list(cex=.8))