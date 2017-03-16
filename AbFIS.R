library(xlsx)
library(ggplot2)
library(dplyr)
library(tidyr)

temp <- read.xlsx(
 "D:/Fisheries Research/Abalone/AbResearchData/pop/2017/Ab_pop_bio_Lenght_density_2016.xlsx",
 sheetName = "Betsey Island",
 col.names = TRUE
)

temp$ Transect <- as.factor(temp$Transect)

## Filter for legal biomass
#dat <- filter(temp, Length > 137) %>%
 
dat <- temp %>% group_by(String, Date, Transect) %>%
 summarise(count = n()) %>% 
 complete(String, Date, Transect, fill = list(count = 0)) %>% data.frame()

dat$abs <- dat$count/15


datmns <- dat %>%
 group_by(Date, String) %>%
 summarise(N = n(), mnabs=mean(abs), sd=sd(abs)) %>% data.frame()


datmns$se <- datmns$sd / sqrt(datmns$N)  # Calculate standard error of the mean

# Confidence interval multiplier for standard error
# Calculate t-statistic for confidence interval: 
# e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
ciMult <- qt(.95/2 + .5, datmns$N-1)
datmns$ci <- datmns$se * ciMult


datmns$String <- as.factor(datmns$String)


pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(datmns, aes(x=Date, y=mnabs, colour=String)) + 
 geom_errorbar(aes(ymin=mnabs-ci, ymax=mnabs+ci), width=5, position=pd) +
 geom_line(position=pd) +
 geom_point(position=pd, size=2)


datmns$Date <- as.factor(datmns$Date)

# Error bars represent standard error of the mean
ggplot(datmns, aes(x=Date, y=mnabs, fill=String)) + 
 geom_bar(position=position_dodge(), stat="identity") +
 geom_errorbar(aes(ymin=mnabs-se, ymax=mnabs+se),
               width=.2,                    # Width of the error bars
               position=position_dodge(.9))


# Use 95% confidence intervals instead of SEM
ggplot(datmns, aes(x=Date, y=mnabs, fill=String)) + 
 geom_bar(position=position_dodge(), stat="identity") +
 geom_errorbar(aes(ymin=mnabs-ci, ymax=mnabs+ci),
               width=.2,                    # Width of the error bars
               position=position_dodge(.9))

131722 * 5 / 10000


bm <- 0.07*10000*.6
bm
bm*.15

149/9/0.15


# Actaeons
mins <- 135855
mins * 2/10000
mins * 5/10000

ha <- 831


# Betsey 

264/1.7/.15



