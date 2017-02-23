library(rgdal)
library(sp)
library(maptools)
library(tools)
library(xlsx)
library(plyr)
library(lubridate)



## Choose directory where raw .csv files are located
setwd(choose.dir())

#CHOOSE which factory to compile
setwd("R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/MM_2007_2010_by_Processor/MK Haulage")
##########################################################################################


#loop through the WkDir subfolders and extract all.CSV file names
MM_files<-c()
#change the grep to search based on folder names
  for (dir_finals in grep('MK Haulage',list.files(path='.',all.files=FALSE,full.names=TRUE),value=TRUE))  
  {
    MM_files<-c(MM_files,grep('.CSV',list.files(path = dir_finals, all.files = FALSE, full.names = TRUE, recursive = TRUE),value=TRUE) )
  }

all_data<-NULL
for (file in MM_files)
{
  helper<- function(file,df)
  {
    data<-read.csv(file,header=FALSE)
    data$file_name<-file
    if (is.null(df)) 
    { df <- data } else { df<-rbind(data,df)}
    return(df)
  }
  
  try(all_data<-helper(file,all_data))
}
head(all_data)

colnames(all_data)<-c("Order", "Docket", "Size", "Time", "Date", "FileSource")

#Change the factory settings below BEFORE SAVING
all_data$DataSource <- "MK Haulage"
length(all_data$DataSource)

write.csv(all_data, file="MM_RawData_Pre2010_MK Haulage.csv")

