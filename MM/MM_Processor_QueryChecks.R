setwd('c:/CloudStor/R_Stuff/AutoAssess')

library(tibble)
library(fuzzyjoin)

##-------------------------------------------------------------------------------------------------------##
## Load FILMS processor query data

# verison 1
docketprocs_films <- readRDS('docketprocs_films.RDS')
# version 2
docketinfo.2 <- readRDS('docketinfo2.RDS')
# original version 
load('AbProcessors_2018_10_09.RData')
rm(abprocs, docketblocks, docketblocks.bl, docketblocks.gl, docketlink, GaryMM)

## Load most recent compiled MM data (excluding post 2015 data)
compiledMM.df.final <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiledMM.df.final.RDS')

##-------------------------------------------------------------------------------------------------------##
## Processor data cleaning

# remove alpha characters from docket_number (create 'docket.number' to match MM dataframe)
docketinfo.2 <- docketinfo.2 %>%
 mutate(docket_number = as.numeric((gsub('[^0-9]', '', docket_number)))) %>%
 mutate(masterdocket = as.numeric((gsub('[^0-9]', '', masterdocket))))

# rename processors
colnames(docketinfo.2)[colnames(docketinfo.2) == "certificate_holder"] <- "processorname"

docketinfo.2$processorname <- as.character(docketinfo.2$processorname)
docketinfo.2$processorname[docketinfo.2$processorname == "AbaloneTasmania"] <- "ABALONE TASMANIA PTY LTD"
docketinfo.2$processorname[docketinfo.2$processorname == "Tasmanian Seafoods Pty Ltd (Margate)"] <- "TASMANIAN SEAFOODS PTY LTD"
docketinfo.2$processorname[docketinfo.2$processorname == "Tasmanian Seafoods Pty Ltd (Smithton)"] <- "TASMANIAN SEAFOODS PTY LTD"
docketinfo.2$processorname[docketinfo.2$processorname == "Ralphs (Electrona)"] <- "RALPH'S TASMANIAN SEAFOOD PTY LTD"
docketinfo.2$processorname[docketinfo.2$processorname == "Coastal Waters Seafoods"] <- "COASTAL WATERS SEAFOODS PTY LTD"
docketinfo.2$processorname[docketinfo.2$processorname == "Taslive Abalone"] <- "TAS LIVE ABALONE PTY LTD"
docketinfo.2$processorname[docketinfo.2$processorname == "Australian Wholesale Seafoods"] <- "AUSTRALIAN WHOLESALE SEAFOODS PTY LTD"
docketinfo.2$processorname[docketinfo.2$processorname == "Southern Unite Seafoods"] <- "SOUTHERN UNITED SEAFOOD AUSTRALIA PTY LTD"
docketinfo.2$processorname[docketinfo.2$processorname == "Watken Quality Tasmanian Abalone"] <- "WATKEN QUALITY TASMANIAN ABALONE PTY LTD"
docketinfo.2$processorname[docketinfo.2$processorname == "Osprey Seafoods Pty Ltd"] <- "OSPREY SEAFOODS PTY LTD"
docketinfo.2$processorname[docketinfo.2$processorname == "Hai Loong Seafood Export Pty Ltd"] <- "HAI LOONG SEAFOOD EXPORT PTY LTD"
docketinfo.2$processorname[docketinfo.2$processorname == "Hai Loong Lobster export"] <- "HAI LOONG SEAFOOD EXPORT PTY LTD"
docketinfo.2$processorname[docketinfo.2$processorname == "Southern Ocean Seafoods"] <- "SOUTHERN OCEAN SEAFOODS (NSW) PTY LTD"
docketinfo.2$processorname[docketinfo.2$processorname == "Krystal Harbour"] <- "KRYSTAL HARBOUR PTY LTD"
docketinfo.2$processorname[docketinfo.2$processorname == "A.R.(Tony) Garth Fish Processor Pty Ltd"] <- "A R GARTH FISH PROCESSOR PTY LTD"
docketinfo.2$processorname[docketinfo.2$processorname == "Pacific Shogi Pty Ltd"] <- "PACIFIC SHOJI PTY LTD"
docketinfo.2$processorname[docketinfo.2$processorname == "UNKWOWN"] <- NA
docketinfo.2$processorname[docketinfo.2$processorname == "Seafood Traders Pty Ltd"] <- "SEAFOOD TRADERS PTY LTD"

##-------------------------------------------------------------------------------------------------------##
## Data checking

# determine how many MM records are missing docket number. Most of these should be prior to year 2000.
df.1 <- subset(compiledMM.df.final, is.na(docket.number))
df.2 <- df.1 %>% 
 group_by(fishyear) %>%
 summarise(n = n())

## appears to be multiple measure dates for some docket numbers. Based on n for these records it looks like processors
## have measured catches that have been held in the processor for > 1 day. I suspect small batches of the overall catch
## being held were sampled as they were processed.
n.per.docket <- compiledMM.df.final %>%
 group_by(docket.number, msr.date, processorname) %>%
 summarise(n = length(shell.length), 
           meanSL = round(mean(shell.length), 1),
           minSL = round(min(shell.length), 1))

# determine where the source of these records. Import and join to newest processor data and old processor data to see 
# if there are any differences.
df.3 <- n.per.docket %>% 
 group_by(docket.number) %>% 
 filter(n() > 1)

df.3.dockets <- as.vector(unique(df.3$docket.number))

df.6 <- compiledMM.df.final %>%
 filter(docket.number %in% df.3.dockets) %>%
 group_by(datasource) %>%
 summarise(n = n()) # appears most source from MM.00.07

## also appears that multiple procesor names exist for the same docket number. I suspect
## this comes from the previous abprocesors dataframe where catches were sent to multiple processors but the docket numbers
## weren't eperated in the join
n.per.docket.2 <- compiledMM.df.final %>%
 group_by(docket.number, processorname) %>%
 summarise(n = length(shell.length), 
           meanSL = round(mean(shell.length), 1),
           minSL = round(min(shell.length), 1))

df.4 <- n.per.docket.2 %>% 
 group_by(docket.number) %>% 
 filter(n() > 1)


## check when using docket number as only grouping variable
n.per.docket.3 <- compiledMM.df.final %>%
 group_by(docket.number) %>%
 summarise(n = length(shell.length), 
           meanSL = round(mean(shell.length), 1),
           minSL = round(min(shell.length), 1))

df.5 <- n.per.docket.3 %>% 
 group_by(docket.number) %>% 
 filter(n() > 1)


# compare join of compiled.docket.00.07 dataframe with old and new processor data (generate summary facMM.uniq in MM_Anlaysis)
# facMM.uniq.copy <- facMM.uniq
# facMM.uniq <- facMM.uniq.copy

colnames(docketinfo)[colnames(docketinfo) == "docket_number"] <- "docket.number"
facMM.uniq$processorname <- as.character(facMM.uniq$processorname)
docketinfo$processorname <- as.character(docketinfo$processorname)

# join to old processor query - duplicates and adds 6 records
facMM.uniq.di <- left_join(facMM.uniq, docketinfo, by = c("docket.number", 'processorname'))

# join to new processor query - maintains 2822 records
colnames(docketinfo.2)[colnames(docketinfo.2) == "docket_number"] <- "docket.number"
facMM.uniq.di.2 <- left_join(facMM.uniq, docketinfo.2, by = c("docket.number", 'processorname'))




compiled.docket.00.07 <- inner_join(facMM, facMM.uniq.di, by = c("docket.number","processorname"))
compiled.docket.00.07.2 <- inner_join(facMM, facMM.uniq.di.2, by = c("docket.number","processorname"))















