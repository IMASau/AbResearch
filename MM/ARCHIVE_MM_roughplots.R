library(ggplot2)

load("R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/MM_AnalysisOutput060716_V2.rdata")
load("c:/CloudStor/Fisheries Research/Abalone/MM/MM_AnalysisOutput060716_V2.rdata")


dat <- subset(compiled.df, subblocklist %in% c("6A","6B","6C") & shell.length <= 200)
dat <- subset(compiled.df, blocklist %in% c(30) & shell.length <= 200)

dat <- subset(compiled.df, subblocklist %in% c("13E") & shell.length <= 200)

ggplot(dat, aes(shell.length)) +
  geom_histogram(binwidth=2) +
  scale_x_continuous(
    limits = c(130, 200)) +
  facet_wrap(~ fishyear, 
             scales = "free_y")



## Block 30

dat$history <- "closed"
dat$history[which(dat$fishyear < 2006)] <- "open"


ggplot(dat, aes(shell.length, ..density.., colour = as.factor(history))) +
 geom_freqpoly(binwidth = 2, size=1) + 
 scale_x_continuous(
  limits = c(130, 200)) +
 facet_wrap(history ~ fishyear,
            scales = "free_y")

ggplot(dat, aes(shell.length, fill = as.factor(history))) +
 geom_histogram(binwidth=2, position="dodge")
 scale_x_continuous(
  limits = c(130, 200)) +
 facet_wrap(. ~ history,
            scales = "free_y")

standardCPUEplot + geom_hline(yintercept = ablineCPUEtarg,
                              colour = "blue",
                              linetype = "dashed")



