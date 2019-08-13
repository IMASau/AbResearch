library(RODBC)
library(RODBCext)
library(tidyverse)
library(lubridate)

## connection to Research database on sby server
channel <- odbcConnect('AbResearch_sby') 

## Georges data

sql <- "SELECT *
  FROM [IMASAbResearch].[dbo].[George3SF]" 
g3.sf <- sqlQuery(channel, sql)
colnames(g3.sf) <- tolower(colnames(g3.sf))

g3.sf <- g3.sf %>% rename(samp_year = year, samp_month = month)

sql <- "SELECT *
  FROM [IMASAbResearch].[dbo].[George3H]" 
g3.h <- sqlQuery(channel, sql)
colnames(g3.h) <- tolower(colnames(g3.h))
colnames(g3.h) <- gsub(x = colnames(g3.h), pattern = "\\%", replacement = "pc")  
g3.h <-
  g3.h %>% rename(
    tafi_index = `tafi index`,
    pc_necrotic = pcnecrotic,
    pc_spawning = pcspawning,
    pc_mature = pcmature,
    pc_locules = pclocules,
    pc_rec_dev = pcrec_dev
  )

sql <- "SELECT *
  FROM [IMASAbResearch].[dbo].[George3G]" 
g3.g <- sqlQuery(channel, sql)
colnames(g3.g) <- tolower(colnames(g3.g))

g3.g <- g3.g %>% mutate(
  samp_year = year(sample_date),
  samp_month = month(sample_date)
)

## Shag Rock Bay data

sql <- "SELECT *
  FROM [IMASAbResearch].[dbo].[ShagRockSF]" 
sr.sf <- sqlQuery(channel, sql)
colnames(sr.sf) <- tolower(colnames(sr.sf))
sr.sf <- sr.sf %>% rename(samp_year = year, samp_month = month)

sql <- "SELECT *
  FROM [IMASAbResearch].[dbo].[ShagRockH]" 
sr.h <- sqlQuery(channel, sql)
colnames(sr.h) <- tolower(colnames(sr.h))
colnames(sr.h) <- gsub(x = colnames(sr.h), pattern = "\\%", replacement = "pc")  

sql <- "SELECT *
  FROM [IMASAbResearch].[dbo].[ShagRockG]" 
sr.g <- sqlQuery(channel, sql)
colnames(sr.g) <- tolower(colnames(sr.g))

sr.g <- sr.g %>% mutate(
  samp_year = year(sample_date),
  samp_month = month(sample_date)
)



## Stinking bay data

sql <- "SELECT *
  FROM [IMASAbResearch].[dbo].[StinkingBaySF]" 
sb.sf <- sqlQuery(channel, sql)
colnames(sb.sf) <- tolower(colnames(sb.sf))
sb.sf <- sb.sf %>% rename(samp_year = year, samp_month = month)


sql <- "SELECT *
  FROM [IMASAbResearch].[dbo].[StinkingBayH]" 
sb.h <- sqlQuery(channel, sql)
colnames(sb.h) <- tolower(colnames(sb.h))
colnames(sb.h) <- gsub(x = colnames(sb.h), pattern = "\\%", replacement = "pc")  

sql <- "SELECT *
  FROM [IMASAbResearch].[dbo].[StinkingBayG]" 
sb.g <- sqlQuery(channel, sql)
colnames(sb.g) <- tolower(colnames(sb.g))

sb.g <- sb.g %>% mutate(
  samp_year = year(sample_date),
  samp_month = month(sample_date)
)


close(channel) 

## --------------------------------------------------------------------------##
##  https://stackoverflow.com/questions/53661139/connect-ms-access-to-r-via-odbc-driver
## 
## Retrieve Oocyte data from Leighs Access datbase
db.oocyte <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=C:/CloudStor/Fisheries/Research/Abalone/Reproductive biology/Histology/Leigh's P Drive/Databases LJG/Georges1991oocyte.mdb")
sqlTables(db.oocyte)

sqlColumns(db.oocyte, "Data")

g3.oocyte <- sqlQuery(db.oocyte, paste("select * from Data"))

colnames(g3.oocyte) <- c("Month", "AB_ID", "Location", "Area", "Feret", "Maj_axis_l", "Min_axis-l", "Perimeter", "Shape_factor", "Volume", "Stage" )

colnames(g3.oocyte) <- tolower(colnames(g3.oocyte))

## save data
save.image("C:/CloudStor/R_Stuff/AbResearch/AbRepro_2019_08_13.RData")




