library(openxlsx)
library(ggplot2)
library(dplyr)

temp <- read.xlsx(
 "D:/owncloud/Fisheries Research/Abalone/Block30/Block30SL.xlsx",
 sheet = "Sheet1", detectDates = TRUE)

temp2 <- read.xlsx(
 "D:/owncloud/Fisheries Research/Abalone/Block30/Block30_2017.xlsx",
 sheet = "Pooled", detectDates = TRUE)

block30lf <- rbind(temp,temp2)

colnames(block30lf) <- tolower(colnames(block30lf))

table(block30lf$trip)

## Need to adjust the time and date so that we can look at the spatial position
## of lengths



# Density plots with semi-transparent fill
#dat <- subset(block30lf, slength >= 145 & slength <= 197 & Smp_date <= "2014-09-11") # Excludes Vic Rocks
dat <- subset(block30lf, slength >= 145 & slength <= 197)
ggplot(dat, aes(x=slength, fill=as.factor(trip))) + geom_density( kernel="gaussian", alpha=.5)

# Interleaved histograms
ggplot(dat, aes(x=slength, fill=as.factor(trip))) +
 geom_histogram(binwidth=2, position="dodge") + geom_density( kernel="gaussian", alpha=.5)

# Interleaved histograms
ggplot(dat, aes(x=slength, fill=as.factor(trip))) +
 stat_bin(binwidth=2, position="dodge") + geom_density( kernel="gaussian", alpha=.5)

ggplot(dat, aes(slength, ..density.., colour = as.factor(trip))) +
 geom_freqpoly(binwidth = 2, size=2) 


dat$bins <- cut(dat$slength, breaks = seq(145,195, 2), right = FALSE)

test <- dat %>%
 dplyr:::group_by(trip, bins) %>%
 dplyr:::summarise(n = length(slength)) %>%
 dplyr:::mutate(ngr = sum(n)) %>% 
 dplyr:::mutate(prop = n/ngr*100)


ggplot(test, aes(x = bins, y = prop)) +
 geom_bar(aes(fill = factor(trip)), position = "dodge", stat = "identity") +
 scale_y_continuous(
  breaks = seq(0, 20, 5),
  minor_breaks = seq(0 , 20, 1),
  limits = c(0, 20),
  expand = c(0, 0)
 )  +
 labs(x = "shell length class",
      y = "proportionBlock",
      title = "Block 30 Exp. Fishing") +
 theme_bw() +
 theme(
  axis.text.x = element_text(
   size = 12,
   color = "Black",
   angle = 90,
   vjust = 0.5
  ),
  axis.text.y = element_text(size = 14, color = "Black")
 )

