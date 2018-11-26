ab.2016MM <- read.csv('R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/DataSummary_from_1Jan2016_to_31Dec2016.csv')
ab.2016MM.backup <- ab.2016MM
#ab.2016MM <- ab.2016MM.backup
ab.2016MM <- ab.2016MM[,-c(3, 4, 6)]
colnames(ab.2016MM) <- tolower(colnames(ab.2016MM))
ab.2016MM$mesrtime <- dmy_hms(ab.2016MM$mesrtime)
ab.2016MM$msr.date <- date(ab.2016MM$mesrtime)
ab.2016MM$downloaddate <- dmy_hms(ab.2016MM$downloaddate)
ab.2016MM$downloaddate <- date(ab.2016MM$downloaddate)
ab.2016MM <- ab.2016MM[,-c(5)]
colnames(ab.2016MM) <- c('e.processor','download.date', 'docket.number', 'shell.length', 'msr.date') 
ab.2016MM$filesource <- '2016MM.csv'


#remove records without docket number or docket = 0 or shell length = NA
ab.2016MM.Output.error.docket<-ab.2016MM[is.na(ab.2016MM$docket.number),]
ab.2016MM<-ab.2016MM[!is.na(ab.2016MM$docket.number),]
ab.2016MM<-ab.2016MM[!is.na(ab.2016MM$shell.length),]
ab.2016MM<-droplevels(subset(ab.2016MM, docket.number !=0))

#rename processors
ab.2016MM$e.processor <- as.character(ab.2016MM$e.processor)
ab.2016MM$e.processor[ab.2016MM$e.processor == "AbaloneTasmania"] <- "ABALONE TASMANIA PTY LTD"
ab.2016MM$e.processor[ab.2016MM$e.processor == "Tas Seafoods Margate"] <- "TASMANIAN SEAFOODS PTY LTD"
ab.2016MM$e.processor[ab.2016MM$e.processor == "Tasmanian Seafoods at Margate"] <- "TASMANIAN SEAFOODS PTY LTD"
ab.2016MM$e.processor[ab.2016MM$e.processor == "Tas Seafoods Smithton"] <- "TASMANIAN SEAFOODS PTY LTD"
ab.2016MM$e.processor[ab.2016MM$e.processor == "Ralphs"] <- "RALPH'S TASMANIAN SEAFOOD PTY LTD"
ab.2016MM$e.processor[ab.2016MM$e.processor == "Coastal Waters"] <- "COASTAL WATERS SEAFOODS PTY LTD"
ab.2016MM$e.processor[ab.2016MM$e.processor == "Tas Live Abalone"] <- "TAS LIVE ABALONE PTY LTD"
ab.2016MM$e.processor[ab.2016MM$e.processor == "MK Haulage"] <- "M & K HAULAGE (TAS) PTY LTD"
ab.2016MM$e.processor<-as.factor(ab.2016MM$e.processor)

eproname <- as.data.frame(unique(ab.2016MM$e.processor))
colnames(eproname) <- c("e.processor")
e.processors <- eproname$e.processor
docketinfo.epro <- droplevels(subset(docketinfo, processorname %in% e.processors))
colnames(docketinfo.epro)[colnames(docketinfo.epro)=="processorname"] <- "e.processor"

n.per.docket <- ab.2016MM %>% 
 group_by(docket.number) %>%
 summarise(n = length(shell.length),
           meanSL = round(mean(shell.length, na.rm = T), 1),
           minSL = round(min(shell.length), 1)) %>%
 inner_join(ab.2016MM, 'docket.number')

#match docketinfo.epro to the n.per.docket dataframe
docket.join <- inner_join(n.per.docket, docketinfo.epro, by = c("docket.number", "e.processor"))

#add date difference column
docket.join$msr.date.diff <- as.Date(docket.join$msr.date, format="yyyy-%mm-%dd")-as.Date(docket.join$unloading_date, format="yyyy-%mm-%dd")

#separate dupilicated dockets
#how many times does each docket.number occur
n_occur <- data.frame(table(docket.join$docket.number))
range(n_occur$Freq)

docket.uniq <- as.data.frame(docket.join[docket.join$docket.number %in% n_occur$Var1[n_occur$Freq == 1],])

#check
n_occur <- data.frame(table(docket.uniq$docket.number))
range(n_occur$Freq)

ab.2016MM.df<-subset(docket.join, select=c("docket.number",  "filesource", "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                              "total_landed_weight", "catch", "received_location", "e.processor", "joincode", 
                                              "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", "numsubblocks"))

ab.2016MM.df$species <- ifelse(is.na(ab.2016MM.df$zone_fishery_code == T), 1,
                           ifelse(ab.2016MM.df$zone_fishery_code == 'AQG', 2, 1))

# extract zone from 'zone_fishery_code'
ab.2016MM.df$newzone <- substr(ab.2016MM.df$zone_fishery_code, 3, nchar(as.character(ab.2016MM.df$zone_fishery_code)))

# split block list into seperate blocks to define regions
ab.2016MM.df <- cSplit(ab.2016MM.df, 'blocklist', ',', drop = F)

# create column for 'blockno' identfying the first block from the 'blocklist' to enable the Region_Recode 
# function to run
ab.2016MM.df$blockno <- ab.2016MM.df$blocklist_1

# subset into blacklip and greenlip to define regions
ab.2016MM.df.sub.bl <- subset(ab.2016MM.df, species == 1)
ab.2016MM.df.sub.gl <- subset(ab.2016MM.df, species == 2)

# code regions for blacklip and greenlip
ab.2016MM.df.sub.bl <- codeBlregion(ab.2016MM.df.sub.bl)

# Re-join blacklip and greenlip df
ab.2016MM.df <- ab.2016MM.df.sub.bl



