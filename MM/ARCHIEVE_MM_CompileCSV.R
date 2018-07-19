library(rgdal)
library(sp)
library(maptools)
library(tools)
library(xlsx)
library(plyr)
library(lubridate)



## Choose directory where raw GPSMK5 csv files are located
setwd(choose.dir())
setwd("R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/MM_2007_2010_by_Processor")
##########################################################################################

                      TF = "R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/MM_2007_2010_by_Processor"
                      ##List of subfolder files where file name starts with "0906"##
                      SF <- list.files(TF,recursive=T, pattern=paste('*.CSV',sep=""))
                      ##Define the list of files to search for##
                      x <- (c('1234' ,'1345','1456','1560')
                            ##Create a conditional to skip over the non-csv files in each folder##
                            if (is.integer(x)){
                              sources.files  <- list.files(SF, recursive=T,full.names=T)}
                            
                            dat <- do.call(rbind,lapply(SF,read.csv))
                            #the warnings thrown are ok--these are generated due to the fact that some of the folders contain .xls files
                            write.table(dat,file="H:/working/TC/TMS/June09Output/June09Batched.csv",row.names=FALSE,sep=",")

Processors<-list.files(all.files=FALSE,full.names=TRUE)

# #Get list of files with .csv extension
# dirlist <- list_files_with_exts(getwd(),"csv", full.names=FALSE)
# dirlist
# #dirlist <- dirlist[c(1,5,9,13,17,21)]

dirlistdf <- as.data.frame(SF)
dirlistdf$OutFileName <- as.character(NA)
colnames(dirlistdf) <- c("InFileName", "OutFileName")
dirlistdf$OutFileName <-sub(".csv", "_MGA.csv", dirlistdf$InFileName, ignore.case =FALSE, fixed=FALSE)
dirlistdf$NumRecords <- as.numeric(NA)
dirlistdf$NumDocket <- as.numeric(NA)
dirlistdf$Date <- as.POSIXct(NA)
dirlistdf$Processor <- as.character(NA)

dirlistdf$Processor <- substr(dirlistdf$InFileName,1,27)
dirlistdf$License <-sub("_", "", dirlistdf$License, ignore.case =FALSE, fixed=FALSE)

dirlistdf <- dirlistdf[order(dirlistdf$Processor, dirlistdf$InFileName),]

numfiles <- nrow(dirlistdf)
numfiles
i <- 623
rm("fullGPSAudit") 
for (i in 1:numfiles) {
  infile <- as.character(dirlistdf$InFileName[i])
  vLicense <- dirlistdf$License[i]
  ExistsMGA <- grepRaw("B0_MGA.csv", infile , ignore.case=FALSE, fixed=FALSE) 
  
  if (length(ExistsMGA)==0) {
    outfile <- as.character(dirlistdf$OutFileName[i])
    gpsdat <- read.csv(file = as.character(dirlistdf$InFileName[i]), header=TRUE, sep=',', dec='.', as.is=TRUE,colClasses=c("character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character"))  
    ##Load GPSMK5 csv file
    ##gpsdat <- read.csv(infile, header=TRUE, sep=',', dec='.', as.is=TRUE,colClasses=c("character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character"))
    
    ## Count records with V
    dirlistdf$NumVRecords[i] <- nrow(subset(gpsdat, Status=="V"))
    ## Count records with Batt ERR
    dirlistdf$NumBattErr[i] <- nrow(subset(gpsdat, Batt=="ERR"))
    
    
    ##Strip Status="V" records from data set
    gpsdat <- subset(gpsdat, Status=="A") 
    
    ## reset Batt field records to -9 where value = ERR
    gpsdat$Batt[gpsdat$Batt =="ERR"] <- " -9"
    
    ## Convert from NMEA to Decimal degrees - numeric approach
    gpsdat$Longitude <- floor(as.numeric(gpsdat$Long_raw)/100) + (as.numeric(gpsdat$Long_raw) - floor(as.numeric(gpsdat$Long_raw)/100)*100 )/60
    gpsdat$Latitude<- (floor(as.numeric(gpsdat$Lat_raw)/100) + (as.numeric(gpsdat$Lat_raw) - floor(as.numeric(gpsdat$Lat_raw)/100)*100 )/60)*-1
    
    
    ## Convert to a SpatialPointsDataFrame
    gpsdat.spdf <- SpatialPointsDataFrame(gpsdat[,c("Longitude","Latitude")], gpsdat[], match.ID = TRUE,proj4string = CRS("+proj=longlat +datum=WGS84") )
    
    ## Re-project from WGS84 to GDA94 MGA Zone53
    gpsdat.utm <- spTransform(gpsdat.spdf, MGA53projstring)
    
    ## Extract coodinates (Eastings & Northings) to new dataframe
    EastNorth <- as.data.frame(coordinates(gpsdat.utm))
    colnames(EastNorth) <- c('Easting', 'Northing')
    
    ## Bind Eastings and Northings to spdf
    gpsdat.utm <- spCbind(gpsdat.utm, EastNorth )
    
    ## Convert spdf to dataframe
    gpsdat.new <- as.data.frame(gpsdat.utm)
    delvars <- names(gpsdat.new) %in% c("Latitude", "Longitude", "Longitude.1", "Latitude.1" )
    gpsdat.new <- gpsdat.new[!delvars]
    
    ## Write filtered and projected data file back to disk as csv
    write.csv(gpsdat.new, file = as.character(dirlistdf$OutFileName[i]), quote=FALSE, row.names=FALSE,na="")
    dirlistdf$TimeOfProcessing <- Sys.time()
    
    
    gpsdat.new$FishDate <- as.POSIXct(strptime(paste(gpsdat$Date_raw, gpsdat$Time_raw), "%d%m%y %H%M%S"), tz = 'GMT') 
    gpsdat.new$FishDateSA <- gpsdat.new$FishDate + 34200
    attr(gpsdat.new$FishDateSA, "tzone") <- "Australia/Adelaide"
    FishDateList <- unique(as.Date(gpsdat.new$FishDateSA))
    FishDateListTZ <- unique(as.Date(gpsdat.new$FishDateSA, tz="Australia/Adelaide",usetz=TRUE))
    
    gpsdat.new$FishDateSA <- as.Date(gpsdat.new$FishDateSA)

    DataDateAudit <- ddply(gpsdat.new, .(FishDateSA),  summarize, NRec=length(RecordNum))
    DataDateAudit$License <- vLicense
    #DataDateAuditOutFileName <-sub(".csv", "_GPS_DataDateAudit.csv", dirlistdf$InFileName[i], ignore.case =FALSE, fixed=FALSE)
    #write.csv(DataDateAudit, file = as.character(DataDateAuditOutFileName), quote=FALSE, row.names=FALSE,na="")
    
    #write.csv(gpsdat.new, file = "GPSDate.csv", quote=FALSE, row.names=FALSE,na="")
    
  }
  
  if (exists("fullGPSAudit")) 
    fullGPSAudit <- rbind(fullGPSAudit, DataDateAudit)
   else
    fullGPSAudit <- DataDateAudit
}


subset(fullGPSAudit, FishDateSA=="2014-03-10")
