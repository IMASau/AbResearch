## Load packages
library(dplyr)
library(lubridate)
library(tidyr)
library(tidyverse)
library(splitstackshape)

## Make a quick back-up copy of compiled.df to refresh data frame
compiled.df.backup <-  compiled.df
#compiled.df <- compiled.df.backup

## Tidy-up and format some of the data for the compiled.df

# The compiled.df contains some records with no docket.numbers but do have measure date, processorname and 
# sub-block information.  These arise from MM.00.07 data and are represented by 'below' and '27/03/' as the
# docket number.  The shell length data can still be used given the above, however, the records can be removed 
# if needed:

compiled.df <- subset(compiled.df, !(docket.number %in% c('below', '27/03/')))
compiled.df$docket.number <- as.numeric(compiled.df$docket.number)

# Replace column names with lower case
colnames(compiled.df) <- tolower(colnames(compiled.df))

# Split compiled.df into sub-blocks
compiled.df <- cSplit(compiled.df, 'blocklist', ',', drop = F)

# Add column for fishing year
compiled.df$fishyear <- year(compiled.df$msr.date)

## Filter data for any combination containing block number (e.g. 9), and between 132-200 mm SL
plotdat <-
 compiled.df %>% filter_at(vars(blocklist_1, blocklist_2, blocklist_3, blocklist_4, blocklist_5), 
                           any_vars(. == 9)) %>% filter(between(shell.length, 132, 200)) 

# B&W plot by fishing year
boxplot(shell.length ~ fishyear, plotdat, xlab = 'Year', ylab = 'Shell length (mm)')

plotdat$fishyear <- as.factor(plotdat$fishyear)
bwplot(fishyear ~ shell.length, plotdat, ylab = 'Year', xlab = 'Shell length (mm)')



ggplot(plotdat) + geom_histogram(aes(x=shell.length, group = fishyear)) + facet_grid(~ fishyear)
ggplot(plotdat) + geom_density(aes(x=shell.length, group = factor(fishyear), colour = factor(fishyear), alpha= 2)) + scale_fill_brewer(palette = "Set1")


table(compiled.df$fishyear, compiled.df$fishyear2)
table(block13E$fishyear, block13E$fishyear2)


pick <- which(is.na(block13E$fishyear))
length(pick)

head(compiled.df[pick,])

compiled.df[423860,]

