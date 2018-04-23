library(HH)
require(vcd)

lawsuit <- read.csv("lawsuit.csv")

head(lawsuit)
tail(lawsuit)
summary(lawsuit)

# below checks to make sure that we do not have any NA data in our source
any(is.na(lawsuit$Type))
any(is.na(lawsuit$Outcome))
any(is.na(lawsuit))

table(lawsuit$Type)
table(lawsuit$Outcome)
table(lawsuit)


mosaicplot(table(lawsuit$Type))
mosaicplot(table(lawsuit$Outcome))
mosaicplot(table(lawsuit))

mosaicplot(table(lawsuit),color = 4:5)

mosaicplot(t(table(lawsuit)))

chisq.test(table(lawsuit$Type, lawsuit$Outcome))
