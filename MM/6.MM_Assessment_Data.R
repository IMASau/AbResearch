# The following script joins historical measuring board data to the latest 
# compilation of NextGen measuring board data and performs a final data clean
# ready for analysis.
##----------------------------------------------------------------------------##
# load required libaries and functions for analysis
suppressPackageStartupMessages({
 library(RODBC)
 library(R.utils)
 library(lubridate)
 library(sp)
 library(tools)
 library(openxlsx)
 library(lubridate)
 library(gdata)
 library(dplyr)
 library(doBy)
 library(tidyr)
 library(tidyverse)
 library(splitstackshape)
 library(readxl)
 library(data.table)
 library(scales)
 library(gridExtra)
 library(rpart)
})

source("C:/GitCode/AbResearch/codeBLnewzone.r")

##----------------------------------------------------------------------------##
# identify data folder
mm_data_folder <- paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/MMdata/',
                                Sys.info()[["user"]]))

# Load most recent compile of historic data
# compiled.docket.pre.2020 <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiled.docket.pre.2020.RDS')
compiled.docket.pre.2020 <- readRDS(paste0(mm_data_folder,'compiled.docket.pre.2020.RDS'))

# Load most recent compile of NextGen measuring board data
# compiled.docket.next.gen <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiled.docket.next.gen.RDS')
# compiled.docket.non.modem <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiled.docket.non.modem.RDS')
compiled.docket.next.gen <- readRDS(paste0(mm_data_folder, 'compiled.docket.next.gen.RDS'))
compiled.docket.non.modem <- readRDS(paste0(mm_data_folder, 'compiled.docket.non.modem.RDS'))


# Combine historic and NextGen data
compiledMM.df <- bind_rows(compiled.docket.pre.2020, 
                           compiled.docket.next.gen,
                           compiled.docket.non.modem)

## Save compiledMM.df ####
# saveRDS(compiledMM.df, 'C:/CloudStor/R_Stuff/MMLF/compiledMM.df.RDS')
saveRDS(compiledMM.df, paste0(mm_data_folder, 'compiledMM.df.RDS'))

##----------------------------------------------------------------------------##
# Final data cleaning of compiledMM.df

# Make a copy of the compiledMM.df if needed

# compiledMM.df <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiledMM.df.RDS')

compiledMM.df.copy <- compiledMM.df
# compiledMM.df <- compiledMM.df.copy
# compiledMM.df <- compiledMM.df.2

# some sub-blocks have an 'N' making it read 'NA', remove N and replace with ''

compiledMM.df$subblocklist <- gsub('N', '', compiledMM.df$subblocklist)

# remove records with no shell length

compiledMM.df <- compiledMM.df[!is.na(compiledMM.df$shell.length),]

# add column for fishyear

compiledMM.df$fishyear <- year(compiledMM.df$daylist_max)

# there are records missing zone data to determine species, however
# block information is provided.  Most blocks are southern areas and can be assigned to blacklip
# however there are about 20 dockets from greenlip blocks which need to
# be assigned a species for plotting, etc.  Assuming only legal sized animals have been measured, 
# animals have been assigned a species based on the legal minimum length for species in that block.
# The greenlip blocks are 1-4, 31-49.  An initial examination of summary data
# indicates most of the records in question come from blocks 48 and 49 in the years 2000-2001
# when GL size limit was 140 mm. 

# subset compiledMM.df into seperate dataframes to identify species

compiledMM.df.spp <- subset(compiledMM.df, !is.na(newzone) & !is.na(species))
compiledMM.df.zone <- subset(compiledMM.df, !is.na(newzone) & is.na(species))
compiledMM.df.spp.nozone <- subset(compiledMM.df, is.na(newzone) & !is.na(species))

# identify species for records missing species but contain zone (i.e 'newzone')

compiledMM.df.zone.spp <- compiledMM.df.zone %>%
 mutate(species = ifelse(!is.na(species), species, 
                         ifelse(newzone == 'G', 2, 1)))

# rejoin dataframes 

compiledMM.df.2 <- bind_rows(compiledMM.df.zone.spp, compiledMM.df.spp.nozone, compiledMM.df.spp)

# split blocklist into seperate blocks to define region or regions where multiple blocks were fished

compiledMM.df.3 <- cSplit(compiledMM.df.2, 'blocklist', ',', drop = F)

# add unique ID column for each record to enable filtering operations

compiledMM.df.3 <- compiledMM.df.3 %>%
 mutate(id = row_number())

# create variables blockno and subblockno to run zone orignal region recode functions (these could be re-written at some stage)

compiledMM.df.3 <- compiledMM.df.3 %>%
 mutate(blockno = as.character(blocklist)) %>%
 mutate(subblockno = as.character(subblocklist))

# recode regions and zones 

# NOTE: for reporting at regional or block scale, include all data where there are multiple blocks
# fished. For reporting at a block scale remove all records where there are multiple blocks as we can not asign catch to 
# a single block. For reporting at a regional scale, remove records where the region recode
# does not match when calculated for each seperate block listed. This only applies to the
# recent data (i.e. >2000) for black and green lip.

# re-load region and zone recode function

source("C:/GitCode/AbResearch/codeBLnewzone.r")

# recode zone for data post 2000

compiledMM.df.recent.zone <- compiledMM.df.3 %>%
 filter(fishyear >= 2000) %>%
 codeBlnewzone() %>%
 mutate(newzone = ifelse(species == 2, 'G', newzone))

# recode zone for data pre 2000

compiledMM.df.historic.zone <- compiledMM.df.3 %>%
 filter(fishyear <= 1999) %>%
 codeBlnewZoneHistoric() %>%
 mutate(newzone = ifelse(species == 2,'G', newzone))

# recode region for data pre 2000

gl.historic <- codeGlRegionHistoric(subset(compiledMM.df.historic.zone, newzone == 'G')) %>%
 mutate(region.1 = gl.region) %>%
 mutate(same.region = 1) %>%
 select(-c(gl.region))

bl.historic <- codeBlRegionHistoric(subset(compiledMM.df.historic.zone, !newzone == 'G')) %>%
 mutate(region.1 = bl.region) %>%
 mutate(same.region = 1) %>%
 select(-c(bl.region))

# filter for blacklip and determine how many seperate data frames need to be created

bl.recent.multi.block <- subset(compiledMM.df.recent.zone, numblocks > 1 & species == 1)
max(bl.recent.multi.block$numblocks)

# for BLACKLIP create seperate data frames for each block within blocklist and determine their region

bl.recent.1 <- subset(compiledMM.df.recent.zone, !is.na(blocklist_1) & species == 1) %>%
 mutate(blockno.temp = blockno) %>%
 mutate(blockno = blocklist_1) %>%
 codeBlregion() %>%
 mutate(region.1 = bl.region) %>%
 mutate(blockno = blockno.temp) %>%
 select(-c(bl.region, blockno.temp))

bl.recent.2 <- subset(bl.recent.1, !is.na(blocklist_2)) %>%
 mutate(blockno.temp = blockno) %>%
 mutate(blockno = blocklist_2) %>%
 codeBlregion() %>%
 mutate(region.2 = bl.region) %>%
 mutate(blockno = blockno.temp) %>%
 select(-c(bl.region, region.1, blockno.temp))

bl.recent.3 <- subset(bl.recent.2, !is.na(blocklist_3)) %>%
 mutate(blockno.temp = blockno) %>%
 mutate(blockno = blocklist_3) %>%
 codeBlregion() %>%
 mutate(region.3 = bl.region) %>%
 mutate(blockno = blockno.temp) %>%
 select(-c(bl.region, region.2, blockno.temp))

# join seperate blacklip dataframes together to compare if the regions match

bl.recent.join <- left_join(bl.recent.1, bl.recent.2) %>%
 left_join(bl.recent.3)

# filter dataframe to exclude blacklip records where the regions DO match

bl.recent.same.region <- bl.recent.join %>% 
 mutate(same.region = if_else(!is.na(region.1) & is.na(region.2), 1,
                              if_else(is.na(region.3) & region.1 == region.2, 1, 
                                      if_else(region.1 == region.2 & region.2 == region.3, 1, 0)))) %>%
 filter(!same.region == 0)

# filter dataframe to exclude blacklip records where the regions DO NOT match

bl.recent.diff.region <- bl.recent.join %>% 
 mutate(same.region = if_else(!is.na(region.1) & is.na(region.2), 1,
                              if_else(is.na(region.3) & region.1 == region.2, 1, 
                                      if_else(region.1 == region.2 & region.2 == region.3, 1, 0)))) %>%
 filter(same.region == 0)

# filter for greenlip and determine how many seperate data frames need to be created
# repeating the steps above

gl.recent.multi.block <- subset(compiledMM.df.recent.zone, numblocks > 1 & species == 2)
max(gl.recent.multi.block$numblocks)

# for GREENLIP create seperate data frames for each block within blocklist and determine their region

gl.recent.1 <- subset(compiledMM.df.recent.zone, !is.na(blocklist_1) & species == 2 & fishyear >= 2006) %>%
 mutate(blockno.temp = blockno) %>%
 mutate(blockno = blocklist_1) %>%
 codeGlregion.post2006() %>%
 mutate(region.1 = gl.region) %>%
 mutate(blockno = blockno.temp) %>%
 select(-c(gl.region, blockno.temp))

gl.recent.2 <- subset(gl.recent.1, !is.na(blocklist_2)) %>%
 mutate(blockno.temp = blockno) %>%
 mutate(blockno = blocklist_2) %>%
 codeGlregion.post2006() %>%
 mutate(region.2 = gl.region) %>%
 mutate(blockno = blockno.temp) %>%
 select(-c(gl.region, region.1, blockno.temp))

gl.recent.3 <- subset(gl.recent.2, !is.na(blocklist_3)) %>%
 mutate(blockno.temp = blockno) %>%
 mutate(blockno = blocklist_3) %>%
 codeGlregion.post2006() %>%
 mutate(region.3 = gl.region) %>%
 mutate(blockno = blockno.temp) %>%
 select(-c(gl.region, region.2, blockno.temp))

gl.recent.4 <- subset(gl.recent.3, !is.na(blocklist_4)) %>%
 mutate(blockno.temp = blockno) %>%
 mutate(blockno = blocklist_4) %>%
 codeGlregion.post2006() %>%
 mutate(region.4 = gl.region) %>%
 mutate(blockno = blockno.temp) %>%
 select(-c(gl.region, region.3, blockno.temp))

gl.recent.5 <- subset(gl.recent.4, !is.na(blocklist_5)) %>%
 mutate(blockno.temp = blockno) %>%
 mutate(blockno = blocklist_5) %>%
 codeGlregion.post2006() %>%
 mutate(region.5 = gl.region) %>%
 mutate(blockno = blockno.temp) %>%
 select(-c(gl.region, region.4, blockno.temp))

# join seperate greenlip dataframes together to compare if the regions match

gl.recent.join <- left_join(gl.recent.1, gl.recent.2) %>%
 left_join(gl.recent.3) %>%
 left_join(gl.recent.4) %>%
 left_join(gl.recent.5)

# filter dataframe to exclude greenlip records where the regions DO match

gl.recent.same.region <- gl.recent.join %>%
 mutate(same.region = if_else(!is.na(region.1) & is.na(region.2), 1,
                              if_else(is.na(region.5) & is.na(region.4) & is.na(region.3) & region.1 == region.2, 1,
                                      if_else(is.na(region.5) & is.na(region.4) & region.1 == region.2 & region.2 == region.3, 1,
                                              if_else(is.na(region.5) & region.1 == region.2 & region.2 == region.3 & region.3 == region.4, 1,
                                                      if_else(!is.na(region.5) & region.5 == region.1, 1, 0)))))) %>%
 filter(!same.region == 0)


# filter dataframe to exclude greenlip records where the regions DO NOT match

gl.recent.diff.region <- gl.recent.join %>%
 mutate(same.region = if_else(!is.na(region.1) & is.na(region.2), 1,
                              if_else(is.na(region.5) & is.na(region.4) & is.na(region.3) & region.1 == region.2, 1,
                                      if_else(is.na(region.5) & is.na(region.4) & region.1 == region.2 & region.2 == region.3, 1,
                                              if_else(is.na(region.5) & region.1 == region.2 & region.2 == region.3 & region.3 == region.4, 1,
                                                      if_else(!is.na(region.5) & region.5 == region.1, 1, 0)))))) %>%
 filter(same.region == 0)

# rejoin all the seperate black and greenlip dataframes together with a region recode for each block in blocklist
# NOTE: there are 2802 records without a blocklist and these have been excluded

compiledMM.df.final <- bind_rows(gl.recent.diff.region, gl.recent.same.region, 
                                 bl.recent.diff.region, bl.recent.same.region,
                                 gl.historic, bl.historic)

# check if there are any missing regions following region recode

missing.region <- subset(compiledMM.df.final, is.na(region.1))
missing.zone <- subset(compiledMM.df.final, is.na(newzone))


# fix up subblocknumbers
compiledMM.df.final <- compiledMM.df.final %>% 
 mutate(blockno = if_else(blockno %in% c('1', '2', '3', '4', '5', '6', '7', '8', '9'), paste('0', blockno, sep = ''), blockno),
        subblockno = if_else(subblockno %in% c('A', 'B', 'C', 'D'), paste(blockno, subblockno, sep = ''), subblockno))

# populate blank sample.id column with docket number
compiledMM.df.final <- compiledMM.df.final %>% 
 mutate(sample.id = docket.number)
 # dplyr::mutate(sample.id = ifelse(is.na(sample.id), docket.number, sample.id))

##----------------------------------------------------------------------------##
# Size limits data ####
# Add size limit data to filter out measurement errors and for reference points on plots.

# load legal minimum length data
# size.limits <- read.csv("C:/CloudStor/R_Stuff/MMLF/AbaloneSizeLimits2.csv", fileEncoding="UTF-8-BOM")
size.limits <- read.csv(paste0(mm_data_folder, 'AbaloneSizeLimits_Dec2024.csv'), fileEncoding="UTF-8-BOM")
size_limits <- read.xlsx(paste0(mm_data_folder, 'AbaloneSizeLimits_Dec2024.xlsx'), detectDates = T)

df_1 <- size_limits %>% 
 gather(monthyear, sizelimit, `1962-01-01`:`2024-12-01`) %>% 
 mutate(monthyear = str_remove(format(as.Date(monthyear), "%m-%Y"), "^0+"), 
        sizelimit.index = paste(abzone, subblockno, monthyear, sep = '-')) %>% 
 select(sizelimit.index, sizelimit)


# # remove existing size limit data
# compiledMM.df.final <- compiledMM.df.final %>% 
#  select(-c(sizelimit, sizelimit.index))

# clean lml data
colnames(size.limits) <- tolower(colnames(size.limits))
names(size.limits) <- gsub('.', '-', names(size.limits), fixed = T)

# convert lml data to long format and create lml index variable
size.limits.tab <- size.limits %>%
 gather(monthyear, sizelimit, `jan-1962`:`dec-2024`) %>% 
 mutate(monthyear = gsub('jan', 1, monthyear)) %>% 
 mutate(monthyear = gsub('feb', 2, monthyear)) %>% 
 mutate(monthyear = gsub('mar', 3, monthyear)) %>% 
 mutate(monthyear = gsub('apr', 4, monthyear)) %>% 
 mutate(monthyear = gsub('may', 5, monthyear)) %>% 
 mutate(monthyear = gsub('jun', 6, monthyear)) %>% 
 mutate(monthyear = gsub('jul', 7, monthyear)) %>% 
 mutate(monthyear = gsub('aug', 8, monthyear)) %>% 
 mutate(monthyear = gsub('sep', 9, monthyear)) %>% 
 mutate(monthyear = gsub('oct', 10, monthyear)) %>% 
 mutate(monthyear = gsub('nov', 11, monthyear)) %>% 
 mutate(monthyear = gsub('dec', 12, monthyear)) %>% 
 mutate(sizelimit.index = paste(abzone, subblockno, monthyear, sep = '-')) %>% 
 select(sizelimit.index, sizelimit)

size.limits.tab <- df_1

# add columns that count number of blocks and subblocks in compiledMM.df.final
compiledMM.df.blockcount <- compiledMM.df.final %>%
 mutate(subblocklist = ifelse(is.na(subblocklist), subblockno, subblocklist)) %>%
 mutate(blocklist = ifelse(is.na(blocklist), as.numeric(gsub("([0-9]+).*$", "\\1", subblocklist)), blocklist)) %>%
 mutate(numblocks = count.fields(textConnection(blocklist), sep = ',')) %>%
 mutate(numsubblocks = count.fields(textConnection(subblocklist), sep = ','))

# add column to determine if the same block was fished where multiple sub-blocks are listed in compiledMM.df.blockcount
compiledMM.df.same.blockcount <- compiledMM.df.blockcount %>%
 mutate(same.block = if_else(!is.na(blocklist_1) & is.na(blocklist_2), 1,
                             if_else(is.na(blocklist_5) & is.na(blocklist_4) & is.na(blocklist_3) & blocklist_1 == blocklist_2, 1,
                                     if_else(is.na(blocklist_5) & is.na(blocklist_4) & blocklist_1 == blocklist_2 & blocklist_2 == blocklist_3, 1,
                                             if_else(is.na(blocklist_5) & blocklist_1 == blocklist_2 & blocklist_2 == blocklist_3 & blocklist_3 == blocklist_4, 1,
                                                     if_else(!is.na(blocklist_5) & blocklist_5 == blocklist_1, 1, 0))))))

# create lml index variable in compiledMM.df.same.blockcount
compiledMM.df.lml.index <- compiledMM.df.same.blockcount %>%
 mutate(sizelimit.index = if_else(
  numsubblocks == 1 & subblockno %in% c('A', 'B', 'C', 'D', 'E'),
  paste(
   newzone,
   blockno,
   lubridate::month(daylist_max),
   fishyear,
   sep = '-'
  ),
  if_else(
   numsubblocks == 1 & is.na(subblockno),
   paste(
    newzone,
    blockno,
    lubridate::month(daylist_max),
    fishyear,
    sep = '-'
   ),
   if_else(
    numsubblocks == 1,
    paste(
     newzone,
     subblockno,
     lubridate::month(daylist_max),
     fishyear,
     sep = '-'
    ),
    if_else(
     numsubblocks > 1 & same.block == 1,
     paste(
      newzone,
      blockno,
      lubridate::month(daylist_max),
      fishyear,
      sep = '-'
     ),        
     NA_character_
     
    )
   ))))

# join size limit data and compiledMM.df to include size limit for each observation
compiledMM.df.join <- left_join(compiledMM.df.lml.index, size.limits.tab, "sizelimit.index")
# compiledMM.df.1 <- left_join(compiledMM.df.final, size.limits.tab, "sizelimit.index")

# convert zero weights and weights >2000 g to NAs
compiledMM.df.join <- compiledMM.df.join %>% 
 mutate(whole.weight = if_else(whole.weight > 2000, 0, whole.weight),
        whole.weight = na_if(whole.weight, 0))

compiledMM.df.final <- compiledMM.df.join 

# compiledMM.df.final <- compiledMM.df.final %>% 
#   select(-c(sizelimit.y)) %>% 
#    dplyr::rename(sizelimit = sizelimit.x)

# add quarter variable
compiledMM.df.final <- compiledMM.df.final %>% 
 mutate(fishquarter = quarter(daylist_max))

# add weight grading
compiledMM.df.final <- compiledMM.df.final %>% 
 mutate(grade = dplyr::if_else(whole.weight == 0, NA_character_,
                               dplyr::if_else(between(whole.weight, 1, 400), 'xsmall',
                                              dplyr::if_else(between(whole.weight, 401, 600), 'small',
                                                             dplyr::if_else(between(whole.weight, 601, 800), 'medium', 'large')))),
        whole.weight = replace(whole.weight, whole.weight == 0, NA))

##----------------------------------------------------------------------------##
# save RDS file

# saveRDS(compiledMM.df.final, 'C:/CloudStor/R_Stuff/MMLF/compiledMM.df.final.RDS')
saveRDS(compiledMM.df.final, paste0(mm_data_folder, 'compiledMM.df.final.RDS'))


# compiledMM.df.final <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiledMM.df.final.RDS')
