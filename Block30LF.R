library(tidyverse)
library(openxlsx)
library(lubridate)

temp <- read.xlsx(
 "C:/CloudStor/Fisheries/Research/Abalone/Block30/Block30SL.xlsx",
 sheet = "Sheet1", detectDates = TRUE)


temp2 <- read.xlsx(
 "C:/cloudstor/Fisheries/Research/Abalone/Block30/Block30_2017.xlsx",
 sheet = "Pooled", detectDates = TRUE)

# ## data from Tas Live Lobster collected in block 29 in April 2019 for quick comparison with experimental fishing
# temp3 <- read.xlsx('R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/TassieLobster_08042019.xlsx',
#                    sheet = "TassieLobster_08042019", detectDates = TRUE)
# temp4 <- temp3 %>% filter(blocklist == 29) %>%
#  select(c(SmpNum = "order", SLength = "shell.length", Smp_date = "unloading_date")) %>%
#  mutate(Trip = 2019, Diver = "JohnDiver", Smp_time = as.numeric(NA))


block30lf <- rbind(temp,temp2)
#block30lf <- bind_rows(temp, temp2, temp4)

colnames(block30lf) <- tolower(colnames(block30lf))

table(block30lf$trip, block30lf$diver)

## Need to adjust the time and date so that we can look at the spatial position
## of lengths



# Density plots with semi-transparent fill
#dat <- subset(block30lf, slength >= 145 & slength <= 197 & Smp_date <= "2014-09-11") # Excludes Vic Rocks
dat <- subset(block30lf, slength >= 138 & slength <= 197)
ggplot(dat, aes(x=slength, fill=as.factor(trip))) + geom_density( kernel="gaussian", alpha=.5)

# Interleaved histograms
ggplot(dat, aes(x=slength, fill=as.factor(trip))) +
 geom_histogram(binwidth=2, position="dodge") + geom_density( kernel="gaussian", alpha=.5)

# Interleaved histograms
ggplot(dat, aes(x=slength, fill=as.factor(trip))) +
 stat_bin(binwidth=5, position="dodge") + geom_density( kernel="gaussian", alpha=.5)

ggplot(dat, aes(slength, ..density.., colour = as.factor(trip))) +
 geom_freqpoly(binwidth = 2, size=2) 


dat$bins <- cut(dat$slength, breaks = seq(138,195, 2), right = FALSE)

test <- dat %>%
 dplyr:::group_by(trip, bins) %>%
 dplyr:::summarise(n = length(slength)) %>%
 dplyr:::mutate(ngr = sum(n)) %>% 
 dplyr:::mutate(prop = n/ngr*100)


ggplot(test, aes(x = bins, y = prop)) +
 geom_bar(aes(fill = factor(trip)), position = "dodge", stat = "identity") +
 scale_y_continuous(
  breaks = seq(0, 30, 5),
  minor_breaks = seq(0 , 30, 1),
  limits = c(0, 30),
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

summary(block30lf$slength)
