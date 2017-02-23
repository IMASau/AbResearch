####MARKET MEASURE ANALYSIS######################


library(RODBC)
library(R.utils)
library(lubridate)
library(plyr)
library(rgdal)
library(sp)
library(maptools)
library(tools)
#library(xlsx)
library(lubridate)



setwd('R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring')

#extract FactoryEmeasure data from research Database table FactoryEmeasure
channel <- odbcConnect('ASD_App2k')
sql <- "SELECT 
mb.CSA_Docket,
mb.CSP_Length,
mb.CSA_CatchDate,
mb.CSA_Block,
mb.CSA_SubBlock,
mb.COD_Name,
mb.CON_Surname,
mb.CON_ORGName,
mb.CatchWeight,
FROM 2000-2007 mm data mb
Order BY mb.CSA_CatchDate"
m.measure.00.07 <- sqlQuery(channel, sql)
close(channel)






#extract FactoryEmeasure data from research Database table FactoryEmeasure
                                           channel <- odbcConnect('Asd_data')
                                           sql <- "SELECT FactoryEmeasure.[Sequence number], 
                                                         FactoryEmeasure.[Docket number], 
                                                         FactoryEmeasure.Length, 
                                                         FactoryEmeasure.Time, 
                                                         FactoryEmeasure.Date, 
                                                         FactoryEmeasure.block, 
                                                         FactoryEmeasure.zone, 
                                                         FactoryEmeasure.diver, 
                                                         FactoryEmeasure.Factory, 
                                                         FactoryEmeasure.[est blips], 
                                                         FactoryEmeasure.[est glips], 
                                                         FactoryEmeasure.[landed weight], 
                                                         FactoryEmeasure.[sample quality]
                                                   FROM FactoryEmeasure;"
                                           MM <- sqlQuery(channel, sql)
                                           close(channel)
   
   #quick look and summary of data                                       
   MM$Year<-substr(MM$Date, 1, 4)
   MM$Year<-as.numeric(MM$Year)
   
   #subsetting for compariosn to other MM datasources
   subMM<-(droplevels(subset(MM, MM$Year<=2011 & MM$Year>=2006)))
   unique(subMM$Factory)
   range(subMM$Year)
   
   
   #ddply by factory of SubMM
   Qsummary<-ddply(subMM,.(Factory, Date), summarize,   n= length(Length))
   
   
                                           

                                           
#extract Emeasure data from gary Database table data from 2010 (part) to 2015 
                                          channel <- odbcConnect('Gary_abMM')
                                          sql <- "SELECT DataLoggerImport.DownlaodEventID, DataLoggerImport.measureID, DataLoggerImport.DownloadFile, DataLoggerImport.Order, DataLoggerImport.Zone, DataLoggerImport.Entitlement, DataLoggerImport.Docket, DataLoggerImport.Size, DataLoggerImport.msrDate, DataLoggerImport.msrTime
                                          FROM DataLoggerImport
                                          ORDER BY DataLoggerImport.DownloadFile, DataLoggerImport.msrDate, DataLoggerImport.msrTime;
                                          "
                                          GaryMM <- sqlQuery(channel, sql)
                                          close(channel)
                      #summary and make year of data accessable
                      summary(GaryMM)
                      GaryMM$Year<-substr(GaryMM$msrDate, 1, 4)
                      GaryMM$Year<-as.numeric(GaryMM$Year)
                      GaryMM$Date<-substr(GaryMM$msrDate, 6, 10)
#Quick look at data
   range(GaryMM$Year, na.rm = T)
   
   subGaryMM<-(droplevels(subset(GaryMM, GaryMM$Year>2007)))
   range(subGaryMM$Year, na.rm = T)
   
   
   subGaryMM<-(droplevels(subset(GaryMM, GaryMM$Year==2009)))
   range(subGaryMM$Year, na.rm = T)

   
   ####DOCKET matching Gary MM data to CE
   
   Docket_GaryMM<-unique(GaryMM$Docket)
   #extract dockets that match unique cases
   Docket_CEextract<-blacklipCE[blacklipCE$docket %in% Docket_GaryMM,]
   summary(Docket_CEextract)
   
   #view duplicates
   dupes<-Docket_CEextract[duplicated(Docket_CEextract[,1]),]
   dupesUnique<-unique(dupes$docket)
   #Extract all duplicates in df
   dupesExtract<-Docket_CEextract[Docket_CEextract$docket %in% dupesUnique,]
   #Cut duplicates from DF
   Docket_CE<-Docket_CEextract[!Docket_CEextract$docket %in% dupesUnique,]
   #match unique docket records to Gary data
   Gary_MMExtract<-GaryMM[GaryMM$Docket %in% Docket_CE$docket,]
   length(unique(Gary_MMExtract$Docket))
   
   names(Gary_MMExtract)[names(Gary_MMExtract)=="Docket"] <- "docket"
   
   #combine non-duplicated MM records to CE data. 
   GaryMM_CE<-join(Gary_MMExtract, Docket_CE, match="all", type="full")
   

   #ddply by factory and fishyear(CE) and year(RAW_MM)
   GaryMM_CEsummary<-ddply(GaryMM_CE,.(DataSource, Year), summarize,   n= length(Size))
   GaryMM_CEsummaryFY<-ddply(GaryMM_CE,.(DataSource, fishyear), summarize,   n= length(Size))
   


#####RawDat from Emeasureboards prior to Gary in 2010 

#
setwd("R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/MM_2007_2010_by_Processor")

#loop to serach for summary files
                MM_files<-c()
                #change the grep to search based on folder names
                for (dir_finals in grep('a',list.files(path='.',all.files=FALSE,full.names=TRUE),value=TRUE))  
                {
                 MM_files<-c(MM_files,grep('MM_RawData_',list.files(path = dir_finals, all.files = FALSE, full.names = TRUE, recursive = TRUE),value=TRUE) )
                }
                all_data<-NULL
#combine files
     Raw_MM<-do.call(rbind,lapply(MM_files,read.csv))
     summary(Raw_MM)
     
     

#extract last digits from vector Date to make year
     Raw_MM$Date<-as.character(Raw_MM$Date)
     #function to substr from right to left
                 substrRight <- function(x, n){
                  substr(x, nchar(x)-n+1, nchar(x))
                 }

Raw_MM$Year<-substrRight(Raw_MM$Date, 4)
Raw_MM$Year<-as.numeric(Raw_MM$Year, na.rm=F)
Raw_MM$Docket<-as.integer(Raw_MM$Docket)
unique(Raw_MM$Year)
#ddply by factory
Raw_MMsummary<-ddply(Raw_MM,.(DataSource), summarize,   n= length(Size))


#Raw data v Database data by Factory for specified years

#ABTAS
unique(Raw_MM$DataSource)
Raw_Abtas<-droplevels(subset(Raw_MM, Raw_MM$DataSource == 'AbaloneTasmania'))
unique(subMM$Factory)
DB_Abtas<-droplevels(subset(subMM, subMM$Factory == 'ABALONE TASMANIA PTY LTD'))

Raw_MMsummary<-ddply(Raw_Abtas,.(Year), summarize,   Raw_n= length(Size))
DB_MMsummary<-ddply(DB_Abtas,.(Year), summarize,   DB_n= length(Length))

# names(DB_MMsummary)[names(DB_MMsummary)=="Date"] <- "DB_Date"

#combine non-duplicated MM records to CE data. 
AbTassummary<-join(Raw_MMsummary, DB_MMsummary, by= "Year", match="all", type="full")


#Ralphs
unique(Raw_MM$DataSource)
Raw_Ralphs<-droplevels(subset(Raw_MM, Raw_MM$DataSource == 'Ralphs'))
unique(subMM$Factory)
DB_Ralphs<-droplevels(subset(subMM, subMM$Factory == "RALPH'S TASMANIAN SEAFOOD PTY LTD"))
unique(GaryMM$--------------------------------------)
DB_Ralphs<-droplevels(subset(subMM, subMM$Factory == "RALPH'S TASMANIAN SEAFOOD PTY LTD"))



Raw_MMsummary<-ddply(Raw_Ralphs,.(Year), summarize,   Raw_n= length(Size))
DB_MMsummary<-ddply(DB_Ralphs,.(Year), summarize,   DB_n= length(Length))

# names(DB_MMsummary)[names(DB_MMsummary)=="Date"] <- "DB_Date"

#combine non-duplicated MM records to CE data. 
Ralphsummary<-join(Raw_MMsummary, DB_MMsummary, by= "Year", match="all", type="full")


####DOCKET matching RAW data to CE

Docket_RawMM<-unique(Raw_MM$Docket)
#extract dockets that match unique cases
Docket_CEextract<-blacklipCE[blacklipCE$docket %in% Docket_RawMM,]
summary(Docket_CEextract)

#view duplicates
dupes<-Docket_CEextract[duplicated(Docket_CEextract[,1]),]
dupesUnique<-unique(dupes$docket)
#Extract all duplicates in df
dupesExtract<-Docket_CEextract[Docket_CEextract$docket %in% dupesUnique,]
#Cut duplicates from DF
Docket_CE<-Docket_CEextract[!Docket_CEextract$docket %in% dupesUnique,]
#match unique docket records to Raw data
Raw_MMExtract<-Raw_MM[Raw_MM$Docket %in% Docket_CE$docket,]
length(unique(Raw_MMExtract$Docket))

names(Raw_MMExtract)[names(Raw_MMExtract)=="Docket"] <- "docket"

#combine non-duplicated MM records to CE data. 
RawMM_CE<-join(Raw_MMExtract, Docket_CE, match="all", type="full")

#ddply by factory and fishyear(CE) and year(RAW_MM)
RawMM_CEsummary<-ddply(RawMM_CE,.(DataSource, Year), summarize,   n= length(Size))
RawMM_CEsummaryFY<-ddply(RawMM_CE,.(DataSource, fishyear), summarize,   n= length(Size))

