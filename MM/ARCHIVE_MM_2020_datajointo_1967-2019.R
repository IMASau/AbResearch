# This script joins Next Gen 4G measuring board data to previosly collated historical abalone 
# length frequency data

# remove unecessary variables and rename variables to match compiledMM.df
df.1 <- measure.board.pre.docket.df %>% 
 select(-c(rawutc, logger_date, local_date, latitude, longitude, abalonenum, zone, logname)) %>% 
 rename(msr.date = plaindate,
        docket.number = docketnum,
        shell.length = shelllength,
        whole.weight = wholeweight,
        e.processor = processor) %>% 
 mutate(e.processor = replace(e.processor, e.processor == 'RALPHS TASMANIAN SEAFOODS PTY LTD', "RALPH'S TASMANIAN SEAFOOD PTY LTD"))


# add variable to distinguish datasource
df.1$datasource <- '2020NextGen4G'                  

# extract docketinfo for e.processors listed in new data
eproname <- as.data.frame(unique(df.1$e.processor))
colnames(eproname) <- c("e.processor")
e.processors <- eproname$e.processor

docketinfo.epro <- droplevels(subset(docketinfo, processorname %in% e.processors)) %>% 
 rename(e.processor = processorname)

# summarise data for number of samples, mean and min shell length and add to dataframe to check for duplicates
n.per.docket <- df.1 %>% 
 group_by(docket.number, msr.date, e.processor) %>%
 summarise(n = length(shell.length),
           meanSL = round(mean(shell.length, na.rm = T), 1),
           minSL = round(min(shell.length), 1))

# match docketinfo.epro to the n.per.docket dataframe
docket.join <- inner_join(n.per.docket, docketinfo.epro, by = c("docket.number", "e.processor"))

# add date difference column
docket.join <- docket.join %>%
 ungroup() %>%
 mutate(msr.date = as.Date(msr.date)) %>%
 mutate(msr.date.diff = as.numeric(msr.date - daylist_max))

# check for and seperate out dupilicated dockets
n_occur <- data.frame(table(docket.join$docket.number))
range(n_occur$Freq)
docket.uniq <- as.data.frame(docket.join[docket.join$docket.number %in% n_occur$Var1[n_occur$Freq == 1],])

# check
n_occur <- data.frame(table(docket.uniq$docket.number))
range(n_occur$Freq)

# join unique dockets to df.1
df.1.unique <- inner_join(df.1, docket.uniq)

# subset data and filter out uneeded or duplicated variables
compiled.docket.20.20 <- df.1.unique %>%
 select(
  docket.number,
  msr.date,
  proc,
  e.processor,
  numprocs,
  proclist,
  newzone,
  numdays,
  daylist,
  daylist_max,
  msr.date.diff,
  numblocks,
  blocklist,
  numsubblocks,
  subblocklist,
  catch,
  n,
  meanSL,
  minSL,
  shell.length,
  whole.weight,
  datasource
 ) %>% 
 rename('processorname' = e.processor)

saveRDS(compiled.docket.20.20, 'C:/CloudStor/R_Stuff/MMLF/compiled.docket.20.20.RDS')

# join dataframe to running datfrane compilation
df.2 <- bind_rows(compiledMM.df, compiled.docket.20.20)
