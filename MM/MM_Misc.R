## continuation of MM_Analysis ####

saveRDS(compiled.df, 'C:/CloudStor/R_Stuff/MMLF/compiled_df.RDS')
saveRDS(compiled.docket.00.07, 'C:/CloudStor/R_Stuff/MMLF/compiled.docket.00.07.RDS')
saveRDS(compiled.docket.07.11, 'C:/CloudStor/R_Stuff/MMLF/compiled.docket.07.11.RDS')
saveRDS(compiled.docket.abdb, 'C:/CloudStor/R_Stuff/MMLF/compiled.docket.abdb.RDS')
saveRDS(compiled.docket.FeMM, 'C:/CloudStor/R_Stuff/MMLF/compiled.docket.FeMM.RDS')
saveRDS(docketinfo, 'C:/CloudStor/R_Stuff/MMLF/docketinfo.RDS')
saveRDS(Output.error.date, 'C:/CloudStor/R_Stuff/MMLF/Output.error.date.RDS')
saveRDS(Output.error.docket, 'C:/CloudStor/R_Stuff/MMLF/Output.error.docket.RDS')
saveRDS(Output.error.epro, 'C:/CloudStor/R_Stuff/MMLF/Output.error.epro.RDS')
saveRDS(Raw.MM.Output.error.docket, 'C:/CloudStor/R_Stuff/MMLF/Raw.MM.Output.error.docket.RDS')

## Load packages
detach('package:plyr')
library(dplyr)
library(lubridate)
library(tidyr)
library(tidyverse)
library(splitstackshape)
library(RODBC)
library(R.utils)
library(lubridate)
library(rgdal)
library(sp)
library(maptools)
library(tools)
library(openxlsx)
library(gdata)
library(doBy)
library(readxl)
library(data.table)

## Extract historical MM data from 1960s to 2000 and join to 'compiled.df' ####

channel <- odbcConnect('Asd_data')
sql <- "SELECT * 
FROM MMCommercialSamplesQuery;"
ab.oldMM <- sqlQuery(channel, sql)
close(channel)

## save a copy of raw ab.oldMM.R data as a backup prior to cleaning
saveRDS(ab.oldMM, 'C:/CloudStor/R_Stuff/MMLF/ab.oldMM_raw.RDS')
#write.csv(ab.oldMM, 'ab.oldMM.csv')

# reload data into console where needed
# compiled.df <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiled_df.RDS')
# compiled.docket.00.07 <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiled.docket.00.07.RDS')
# compiled.docket.07.11 <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiled.docket.07.11.RDS')
# compiled.docket.abdb <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiled.docket.abdb.RDS')
# compiled.docket.FeMM <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiled.docket.FeMM.RDS')
# docketinfo <- readRDS('C:/CloudStor/R_Stuff/MMLF/docketinfo.RDS')
# Output.error.date <- readRDS('C:/CloudStor/R_Stuff/MMLF/Output.error.date.RDS')
# Output.error.docket <- readRDS('C:/CloudStor/R_Stuff/MMLF/Output.error.docket.RDS')
# Output.error.epro <- readRDS('C:/CloudStor/R_Stuff/MMLF/Output.error.epro.RDS')
# Raw.MM.Output.error.docket <- readRDS('C:/CloudStor/R_Stuff/MMLF/Raw.MM.Output.error.docket.RDS')
# ab.oldMM <- readRDS('C:/CloudStor/R_Stuff/MMLF/MM_ab_oldMM.RDS')

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
 choice<-subset(n.per.sample.id, sampleid == b)
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

# NO duplicates were found using above code

# add a column 'datasource' to distinguish historic from more recent data
ab.oldMM.sub$datasource <- 'MM.pre00'

# add columns to match 'compiled.df' and populate with NA's
ab.oldMM.sub.var <- c('catch', 'received_location', 'processorname', 'joincode', 'zone_fishery_code',
                    'numblocks', 'numsubblocks', 'msr.date.diff')
ab.oldMM.sub[ab.oldMM.sub.var] <- NA

# populate additional columns where possible
ab.oldMM.sub$msr.date.diff <- ymd(ab.oldMM.sub$msr.date) - ymd(ab.oldMM.sub$unloading_date)
ab.oldMM.sub$numblocks <- count.fields(textConnection(as.character(ab.oldMM.sub$blocklist)), sep = ',')
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

# rename 'blocklist' to 'blockno' to enable region recode function to run
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
#write.csv(ab.oldMM.df, 'ab.oldMM.df.csv')

## Tidy-up and format some of the data for the compiled.df####

## Make a copy of the compiled.df
ab.newMM <- compiled.df

# The compiled.df contains some records with no docket.numbers but do have measure date, processorname and 
# sub-block information.  These arise from MM.00.07 data and are represented by 'below' and '27/03/' as the
# docket number.  The shell length data can still be used, therefore it was decided to convert the docket
# information to NAs.  Alternatively these can be removed (955 records).

# ab.newMM$docket.number[ab.newMM$docket.number %in% c('below', '27/03/')] <- NA
# remove records if needed
ab.newMM <- subset(ab.newMM, !(docket.number %in% c('below', '27/03/')))
ab.newMM$docket.number <- as.numeric(ab.newMM$docket.number)

# some sub-blocks have an 'N' making it read 'NA', remove N and replace with ''
ab.newMM$subblocklist <- gsub('N', '', ab.newMM$subblocklist)

# remove records with no shell length
ab.newMM<-ab.newMM[!is.na(ab.newMM$shell.length),]

# add column for fishyear
ab.newMM$fishyear <- year(ab.newMM$msr.date)

# there are 79124 records missing zone data to determine species, however
# block information is provided.  Most blocks are southern areas and can be assigned to blacklip
# however there are about 20 dockets from greenlip blocks which need to
# be assigned a species for plotting, etc.  Assuming only legal sized animals have been measured, 
# animals have been assigned a species based on the legal minimum length for species in that block.
# The greenlip blocks are 1-4, 31-49.  An initial examination of summary data
# indicates most of the records in question come from blocks 48 and 49 in the years 2000-2001
# when GL size limit was 140 mm. 

# subset data for animals where zone is missing
ab.newMM.nz <- subset(ab.newMM, is.na(zone_fishery_code))
ab.newMM.z <- subset(ab.newMM, !is.na(zone_fishery_code))

# create summary of min shell length for each docket id
ab.newMM.nz.sum <- ab.newMM.nz %>%
 group_by(docket.number, processorname, blocklist, fishyear) %>%
 summarise(n = length(shell.length), 
           meanSL = round(mean(shell.length), 1),
           minSL = round(min(shell.length), 1)) %>%
 as.data.frame()

# create additional column in summary data to identify species
ab.newMM.nz.sum.sp <- ab.newMM.nz.sum %>%
 mutate(species = ifelse(minSL < 132, 1,
                         ifelse(blocklist %in% c(seq(5, 40, 1)), 1,
                         ifelse(blocklist %in% c(seq(1, 4, 1)) & minSL >= 150, 2, 
                         ifelse(blocklist %in% c(seq(41, 49, 1)) & fishyear < 2002 & minSL >= 140, 2, 1)))))


codeZoneHistoric <- function(Bl) {
 {
  Bl$newzone[Bl$blocklist %in% seq(13, 31, 1)] <- "E"
  Bl$newzone[Bl$blocklist %in% c(seq(7, 12, 1))] <- "W"
  Bl$newzone[Bl$blocklist %in% c(1, 2, 3, 4, 5, 6, 39, 40)] <- "N"
  Bl$newzone[Bl$blocklist %in% c(seq(32, 38, 1), seq(41, 49, 1), seq(50, 57, 1))] <- "BS"
  Bl$newzone[Bl$species == 2] <- 'G'
  
 }
 return(Bl)
}

ab.newMM.nz.sum.sp.z <- codeZoneHistoric(ab.newMM.nz.sum.sp)

# join the summary data to the original data to identify species for records without zone 
ab.newMM.join <- left_join(ab.newMM.nz, ab.newMM.nz.sum.sp.z, 
                           by = c('docket.number', 'processorname', 'blocklist', 'fishyear'))

# remove previous summary data (.x) as filtering since HJ has removed some additional records since then
ab.newMM.join <- ab.newMM.join %>% select(-c(n.x, meanSL.x, minSL.x))

# keep new summary data (.y) and rename columns to match original data frame
ab.newMM.join <- ab.newMM.join %>% rename(n = n.y, meanSL = meanSL.y, minSL = minSL.y)

# re-join dataframes
ab.newMM.df <- bind_rows(ab.newMM.z, ab.newMM.join)


# identify species for records with 'zone_fishery_code'
ab.newMM.df$species <- ifelse(is.na(ab.newMM.df$zone_fishery_code), ab.newMM.df$species,
                           ifelse(as.character(ab.newMM.df$zone_fishery_code) == 'AQG', 2, 1))



# extract zone for records with 'zone_fishery_code'
ab.newMM.df$newzone <- ifelse(!is.na(ab.newMM.df$zone_fishery_code), substr(ab.newMM.df$zone_fishery_code, 3,
                                                                            nchar(as.character(ab.newMM.df$zone_fishery_code))),
                              ifelse(!is.na(ab.newMM.df$zone_fishery_code) & ab.newMM.df$species == 2, 'G',
                                     ab.newMM.df$newzone))


# split block list into seperate blocks to define regions
ab.newMM.df <- cSplit(ab.newMM.df, 'blocklist', ',', drop = F)

# create column for 'blockno' identfying the first block from the 'blocklist' to enable the Region_Recode 
# function to run
ab.newMM.df$blockno <- ab.newMM.df$blocklist_1

# subset into blacklip and greenlip to define regions
ab.newMM.sub.bl <- subset(ab.newMM.df, species == 1)
ab.newMM.sub.gl <- subset(ab.newMM.df, species == 2)
#ab.newMM.sub.na <- subset(ab.newMM.df, is.na(species))

# code regions for blacklip and greenlip
ab.newMM.bl <- codeBlregion(ab.newMM.sub.bl)
ab.newMM.gl <- codeGlregion(ab.newMM.sub.gl)

# Re-join blacklip and greenlip df
ab.newMM.df <- bind_rows(ab.newMM.bl, ab.newMM.gl)

#### Join recent and historical data together ####

# convert 'unloading_date' to POSIXct to allow bind_rows function to work
ab.newMM.df$unloading_date <- as.POSIXct(ab.newMM.df$unloading_date, format = '%Y-%m-%d')

# join old and new market measure data frames together
compiledMM.df <- bind_rows(ab.oldMM.df, ab.newMM.df)
compiledMM.df$docket.number <- as.integer(compiledMM.df$docket.number)
ab.2016MM.df$msr.date <- as.POSIXct(ab.2016MM.df$msr.date, format = '%Y-%m-%d')
compiledMM.df <- bind_rows(compiledMM.df, ab.2016MM.df)

# Add column for fishing year, month and quarter
compiledMM.df$fishyear <- ifelse(is.na(compiledMM.df$msr.date), year(compiledMM.df$unloading_date),
                                 year(compiledMM.df$msr.date))
compiledMM.df$fishmonth <- month(compiledMM.df$msr.date)
compiledMM.df$fishquarter <- quarter(compiledMM.df$msr.date)

#fishyear.na <- subset(compiledMM.df, is.na(fishyear))

#### Summaries ####
summ.zone.year <- compiledMM.df %>%
 group_by(newzone, fishyear) %>%
 summarise(n())

# There appears to be some strange data from the historical CSIRO ctach sampling particualry
# from 1971-1974 in the Western zone (e.g. the interquartile values are all the same).
# The following code has been developed to examine these data in more detail and by examining the original
# data file from Excel.

# import historical CSIRO data stored in an Excel spreadsheet on R:drive
ab.his <- read_excel('R:/TAFI/TAFI_MRL_Sections/Abalone/Section Shared/Abalone_databases/historic_data catch sampling.xls')

# examine variables (column names)
names(ab.his)

# sizes are presented as counts in 5 mm bin classes from 98-213 mm.  There are three series of values
# for each bin class representing a) the count of animals in each bin class for the entire catch b) the
# percentage of of catch in in bin class and c) the count of animals of each bin class for the sample weight
# based on a) and b).  Therefore remove values for a) and b).
ab.his.sub <- ab.his[, -c(26:73)]

# rename/simplfy the variable names and to lowercase
colnames(ab.his.sub) <- c('ref.no', 'rhon.ref.no', 'csa.type', 'diver', 'first.name', 'add.diver', 'boat.name', 'processor', 
'date.meas', 'date.captured', 'species', 'block', 'landing.port', 'wt.meas.kg', 'wt.land.kg', 'comments',
'hours', 'no.landed', 'cpue', 'x1', 'zone', 'live.wt.land.lb', 'wt.meas.lb', 'sample.size', 'fisherman', '98',
'103', '108', '113', '118', '123', '128', '133', '138', '143', '148', '153', '158', '163', '168', '173', 
'178', '183', '188', '193', '198', '203', '208', '213')

# transpose the grouped data into long format by expanding the size classes for each unique sample
# use the ref.no as the unique identifier and convert to a factor and create a column named 'count'
ab.his.sub$ref.no <- as.factor(ab.his.sub$ref.no)
ab.his.sub.long <- gather(ab.his.sub, shell.length, count, c(26:49), factor_key = T)

# remove size classes for unique samples where the size class count = 0
ab.his.sub.long.na <- ab.his.sub.long[!is.na(ab.his.sub.long$count),]

# expand each unique sample to an individual abalone length by repeating the count of size classes for each
ab.his.sub.long.2 <- expandRows(ab.his.sub.long.na, 'count')

# identify fishery zone for each individual
ab.his.sub.long.2 <- rename(ab.his.sub.long.2, blockno = block)
ab.his.sub.long.2 <- codeBlZoneHistoric(ab.his.sub.long.2)

# add column for fishyear
ab.his.sub.long.2$fishyear <- year(ab.his.sub.long.2$date.meas)

# convert the original length data from factor to numeric
ab.his.sub.long.2$shell.length <- as.numeric(levels(ab.his.sub.long.2$shell.length))[ab.his.sub.long.2$shell.length]

# filter and extract data required for plotting
plotdat <-
 ab.his.sub.long.2 %>% filter(species == 1 & newzone == 'W' & (between(shell.length, 140, 200))
              & (between(fishyear, 1970, 1974)))

# examine 70's data
x <- ab.oldMM
names(x)
x <- rename(x, blockno = CSA_Block)
x <- rename(x, shell.length = CSP_Length)
x <- codeBlZoneHistoric(x)
x$fishyear <- year(x$CSA_SampleDate)
plotdat <-
 x %>% filter(CSA_Species == 1 & newzone == 'W' & (between(shell.length, 140, 200))
              & (between(fishyear, 1970, 1974)))
#write.csv(plotdat, 'WZ70-74.csv')

plotdat %>% group_by(fishyear) %>%
 summarise(`25%`=quantile(shell.length, probs=0.25),
           `50%`=quantile(shell.length, probs=0.5),
           `75%`=quantile(shell.length, probs=0.75),
           avg=mean(shell.length),
           n=n())
#import Daves original excel file from CSIRO that was imported to database

#### Plots ####

## Plot shell lengths by year and zone

for (i in unique(compiledMM.df$newzone)){
 subdata <- subset(compiledMM.df, newzone == i)
 print(ggplot(subdata, aes(x = fishyear, y = shell.length, group = fishyear)) +
        geom_boxplot(outlier.colour = "orange", outlier.size = 1.5)) +
  ggtitle(as.character(i)) +
  xlab('Year') +
  ylab('Shell length (mm)') + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5))
}


dev.off()



plotdat <-
 compiledMM.df %>% filter(newzone == 'E' & (between(shell.length, 132, 200))
                          & (between(fishyear, 2000, 2016)))

plotdat <-
 compiledMM.df %>% filter(newzone == 'W' & (between(shell.length, 140, 200)))

plotdat <-
 compiledMM.df %>% filter(newzone == 'W')

plotdat <-
 compiledMM.df %>% filter(newzone == 'N' & (between(shell.length, 127, 200)))

plotdat <-
 compiledMM.df %>% filter(newzone == 'BS' & (between(shell.length, 100, 200)))

plotdat <-
 compiledMM.df %>% filter(newzone == 'CW' & (between(shell.length, 132, 200)))

plotdat <-
 compiledMM.df %>% filter(newzone == 'G' & (between(shell.length, 132, 200)))

plotdat <-
 compiledMM.df %>% filter(blockno == 13 & (between(shell.length, 132, 200)))


plotdat$fishyear <- as.factor(plotdat$fishyear)
#plotdat$fishquarter <- as.factor(plotdat$fishquarter)
df <- plotdat %>% group_by(fishyear) %>% summarize(n = n())
ggplot(plotdat, aes(x = fishyear, y = shell.length)) + 
 geom_boxplot(outlier.colour = "orange", outlier.size = 1.5) +
 geom_text(data = df, aes(y = 210, label = n,), size = 3, angle = 90) +
 geom_hline(aes(yintercept = 145), colour = 'red', linetype = 'dotted') +
 ggtitle('Western Zone 1967-2016') +
 xlab('Year') +
 ylab('Shell length (mm)')+ 
 theme_bw() + 
 theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
       axis.text.x = element_text(angle = 45, vjust = 0.5))



## Filter data for any combination containing block number (e.g. 9), and between 132-200 mm SL
#plotdat <- 
#compiledMM.df %>% filter_at(vars(blocklist_1, blocklist_2, blocklist_3, blocklist_4, blocklist_5), 
#any_vars(. == 13)) %>% filter(between(shell.length, 132, 200)) 


ggplot(plotdat) + geom_histogram(aes(x=shell.length, group = fishyear)) + facet_grid(~ fishyear)
ggplot(plotdat) + geom_density(aes(x=shell.length, group = factor(fishyear), colour = factor(fishyear), alpha= 2)) + scale_fill_brewer(palette = "Set1")


table(compiled.df$fishyear, compiled.df$fishyear2)
table(block13E$fishyear, block13E$fishyear2)


pick <- which(is.na(block13E$fishyear))
length(pick)

head(compiled.df[pick,])

compiled.df[423860,]



