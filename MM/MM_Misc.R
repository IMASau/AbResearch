

## Load packages
detach('package:plyr')
library(dplyr)
library(lubridate)
library(tidyr)
library(tidyverse)
library(splitstackshape)

## Extract historical MM data from 1960s to 2000 and join to 'compiled.df' ####

channel <- odbcConnect('Asd_data')
sql <- "SELECT * 
FROM MMCommercialSamplesQuery;"
ab.oldMM <- sqlQuery(channel, sql)
close(channel)

## save a copy of compiled.df and ab.oldMM .R data
saveRDS(compiled.df, 'C:/CloudStor/R_Stuff/MMLF/MM_compiled_df.RDS')
saveRDS(ab.oldMM, 'C:/CloudStor/R_Stuff/MMLF/MM_ab_oldMM.RDS')

# subset historic data to include only the required variables 
ab.oldMM.sub <- subset(ab.oldMM, select = c("CSA_Docket", "CSA_Processor", "CSA_SampleDate", "CSA_CatchDate",
                                           "CSA_Species", "CSA_Block", "CSA_SubBlock", "CSA_CatchLocation", "CSA_CatchWeight",
                                           "CSP_Length", "CommercialSamples_CSA_ID"))

# rename most variables to match more recent data
colnames(ab.oldMM.sub) <- c("docket.number", "processor_licence_id", "msr.date", "unloading_date", 
                           'species', "blocklist", "subblocklist", 'catchlocation', "total_landed_weight",
                           "shell.length", 'sampleid')

# remove records without shell length
ab.oldMM.sub<-ab.oldMM.sub[!is.na(ab.oldMM.sub$shell.length),]

# search for duplicate records
n.per.sample.id <- ab.oldMM.sub %>%
 group_by(sampleid, msr.date) %>%
 summarise(n = length(shell.length), 
           meanSL = round(mean(shell.length), 1),
           minSL = round(min(shell.length), 1))

db.dup.sample.ids<-unique(n.per.sample.id$sampleid)

if (exists("pick_db.sample.id")) 
 rm(pick_db.sample.id)

for(b in db.dup.sample.ids){
 choice<-subset(n.per.sample.id, sampleid ==b)
 maxim<-max(choice$n)  
 pick<-subset(choice, n == maxim)
 minin<-min(pick$msr.date)
 pick<-subset(pick, msr.date == minin)
 if (exists("pick_db.sample.id"))
  pick_db.sample.id <- rbind(pick_db.sample.id, pick)
 else
  pick_db.sample.id <- pick
}

n_occur <- data.frame(table(pick_db.sample.id$sampleid))
range(n_occur$Freq)

# add a column 'datasource' to distinguish historic from more rescent data
ab.oldMM.sub$datasource <- 'MM.old.pre00'

# add columns to match 'compiled.df' and populate with NA's
ab.oldMM.sub.var <- c('catch', 'received_location', 'processorname', 'joincode', 'zone_fishery_code',
                    'numblocks', 'numsubblocks', 'msr.date.diff')
ab.oldMM.sub[ab.oldMM.sub.var] <- NA

# populate additional columns where possible
ab.oldMM.sub$msr.date.diff <- ymd(ab.oldMM.sub$msr.date) - ymd(ab.oldMM.sub$unloading_date)
ab.oldMM.sub$numblocks <- ab.oldMM.sub$blocklist / ab.oldMM.sub$blocklist
ab.oldMM.sub$numsubblocks <- nchar(as.character(ab.oldMM.sub$subblocklist))

# summarise data for number of samples, mean and min shell length and add to dataframe
ab.oldMM.sub <- ab.oldMM.sub %>% 
 group_by(sampleid) %>%
 summarise(n = length(shell.length),
           meanSL = round(mean(shell.length, na.rm = T), 1),
           minSL = round(min(shell.length), 1)) %>%
 inner_join(ab.oldMM.sub, 'sampleid')

# re-order columns
ab.oldMM.sub <- ab.oldMM.sub[,c("sampleid", "species", "catchlocation", "docket.number", "msr.date", "unloading_date", "msr.date.diff", "shell.length",
                  "total_landed_weight", "catch", "received_location", "processorname", "joincode",
                  "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist",
                  "numsubblocks", "datasource", "n", "meanSL", "minSL")]

## code historical data zones and regions
source("C:/GitCode/AbHarvestStrategy/Region_Recode2018.r")

# rename 'blocklist' to 'blockno' to enable function to run
ab.oldMM.sub <- rename(ab.oldMM.sub, blockno = blocklist)

# code zones for black and greenlip
ab.oldMM.sub <- codeBlZoneHistoric(ab.oldMM.sub) 
ab.oldMM.sub <- ab.oldMM.sub %>%
 mutate(newzone = replace(newzone, species == 2, 'G'))

# subset ab.oldMM.sub into blacklip and greenlip to define regions
ab.oldMM.sub.bl <- subset(ab.oldMM.sub, species == 1)
ab.oldMM.sub.gl <- subset(ab.oldMM.sub, species == 2)

# Code regions for blacklip and greenlip
ab.oldMM.sub.bl <- codeBlRegionHistoric(ab.oldMM.sub.bl)
ab.oldMM.sub.gl <- codeGlRegionHistoric(ab.oldMM.sub.gl)

# Re-join blacklip and greenlip df
ab.oldMM.df <- bind_rows(ab.oldMM.sub.bl, ab.oldMM.sub.gl)

## Tidy-up and format some of the data for ab.newMM ####

## Make a copy of compiled.df
#compiled.df <- MM_compiled_df
ab.newMM <- compiled.df

# The compiled.df contains some records with no docket.numbers but do have measure date, processorname and 
# sub-block information.  These arise from MM.00.07 data and are represented by 'below' and '27/03/' as the
# docket number.  The shell length data can still be used given the above, however, the records can be removed 
# if needed:

ab.newMM <- subset(ab.newMM, !(docket.number %in% c('below', '27/03/')))
ab.newMM$docket.number <- as.numeric(ab.newMM$docket.number)

# distinguish between black and greenlip species based on 'zone_fishery_code'
# (i.e. 1 = black, 2 = green) and create a 'species'
# assume NA's are blacklip?
ab.newMM$species <- ifelse(is.na(ab.newMM$zone_fishery_code == T), 1,
                              ifelse(ab.newMM$zone_fishery_code == 'AQG', 2, 1))

# extract zone from 'zone_fishery_code'
ab.newMM$newzone <- substr(ab.newMM$zone_fishery_code, 3, nchar(as.character(ab.newMM$zone_fishery_code)))

# determine zones for records where only block number is provided
nas$fishyear <- year(nas$msr.date)
nas %>%
 group_by(blocklist) %>%
 summarise(n(), min(shell.length)) %>%
 as.data.frame()


# split block list into seperate blocks to define regions
ab.newMM <- cSplit(ab.newMM, 'blocklist', ',', drop = F)

# create column for 'blockno' identfying the first block from the 'blocklist' to enable the Region_Recode 
# function to run
ab.newMM$blockno <- ab.newMM$blocklist_1

# subset into blacklip and greenlip to define regions
ab.newMM.sub.bl <- subset(ab.newMM, species == 1)
ab.newMM.sub.gl <- subset(ab.newMM, species == 2)

# code regions for blacklip and greenlip
ab.newMM.bl <- codeBlregion(ab.newMM.sub.bl)
ab.newMM.gl <- codeGlregion(ab.newMM.sub.gl)

# Re-join blacklip and greenlip df
ab.newMM.df <- bind_rows(ab.newMM.bl, ab.newMM.gl)

#### Join recent and historical data together ####

# convert 'unloading_dat' to POSIXct to allow bind_rows function to work
ab.newMM.df$unloading_date <- as.POSIXct(ab.newMM.df$unloading_date, format = '%Y-%m-%d')

# join old and new market measure data frames together
compiledMM.df <- bind_rows(ab.oldMM.df, ab.newMM.df)

# Add column for fishing year, month and quarter
compiledMM.df$fishyear <- year(compiledMM.df$msr.date)
compiledMM.df$fishmonth <- month(compiledMM.df$msr.date)
compiledMM.df$fishquarter <- quarter(compiledMM.df$msr.date)

#### Summaries ####
summ.zone.year <- compiledMM.df %>%
 group_by(newzone, fishyear) %>%
 summarise(n())

#### Plots ####



## Filter data for any combination containing block number (e.g. 9), and between 132-200 mm SL
plotdat <-
 compiledMM.df %>% filter_at(vars(blocklist_1, blocklist_2, blocklist_3, blocklist_4, blocklist_5), 
                           any_vars(. == 13)) %>% filter(between(shell.length, 132, 200)) 
unique(plotdat$blocklist)

unique(compiledMM.df$newzone)

plotdat <-
 compiledMM.df %>% filter(newzone == 'E' & (between(shell.length, 132, 200)))

plotdat <-
 compiledMM.df %>% filter(newzone == 'W' & (between(shell.length, 132, 200)))

plotdat <-
 compiledMM.df %>% filter(newzone == 'N' & (between(shell.length, 120, 200)))

plotdat <-
 compiledMM.df %>% filter(newzone == 'BS' & (between(shell.length, 120, 200)))

plotdat <-
 compiledMM.df %>% filter(newzone == 'CW' & (between(shell.length, 120, 200)))

plotdat <-
 compiledMM.df %>% filter(newzone == 'G' & (between(shell.length, 120, 200)))

# B&W plot by fishing year
boxplot(shell.length ~ fishyear, plotdat, xlab = 'Year', ylab = 'Shell length (mm)', main = 'Eastern Zone 1967-2015')
boxplot(shell.length ~ fishyear, plotdat, xlab = 'Year', ylab = 'Shell length (mm)', main = 'Western Zone 1967-2015')
boxplot(shell.length ~ fishyear, plotdat, xlab = 'Year', ylab = 'Shell length (mm)', main = 'Northern Zone 1967-2015')
boxplot(shell.length ~ fishyear, plotdat, xlab = 'Year', ylab = 'Shell length (mm)', main = 'Bass Strait Zone 1967-2015')
boxplot(shell.length ~ fishyear, plotdat, xlab = 'Year', ylab = 'Shell length (mm)', main = 'Central West Zone 1967-2015')
boxplot(shell.length ~ fishyear, plotdat, xlab = 'Year', ylab = 'Shell length (mm)', main = 'Greenlip Zone 1967-2015')

#plotdat$fishyear <- as.factor(plotdat$fishyear)
#bwplot(fishyear ~ shell.length, plotdat, ylab = 'Year', xlab = 'Shell length (mm)')


ggplot(plotdat) + geom_histogram(aes(x=shell.length, group = fishyear)) + facet_grid(~ fishyear)
ggplot(plotdat) + geom_density(aes(x=shell.length, group = factor(fishyear), colour = factor(fishyear), alpha= 2)) + scale_fill_brewer(palette = "Set1")


table(compiled.df$fishyear, compiled.df$fishyear2)
table(block13E$fishyear, block13E$fishyear2)


pick <- which(is.na(block13E$fishyear))
length(pick)

head(compiled.df[pick,])

compiled.df[423860,]



