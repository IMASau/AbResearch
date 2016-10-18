library(xlsx)
library(ggplot2)
library(dplyr)

temp <- read.xlsx(
 "D:/Fisheries Research/Abalone/Block30/Block30SL.xlsx",
 sheetName = "Sheet1",
 col.names = TRUE
)


# Density plots with semi-transparent fill
dat <- subset(temp, SLength <= 197)
ggplot(dat, aes(x=SLength, fill=as.factor(Trip))) + geom_density(alpha=.3)

# Interleaved histograms
ggplot(dat, aes(x=SLength, fill=as.factor(Trip))) +
 geom_histogram(binwidth=2, position="dodge")


table(dat$Trip)

