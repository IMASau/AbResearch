## MARKET MEASURE QA
## Original code by Jaime McAllister 20th July 2018

## Searching for date fields in docket.number of compiled.df ####
## docket.number type = int
## therefore search for fields which are not in correct format

abalonetasmania <-
 read.csv(file = "R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/MM_2007_2010_by_Processor/AbaloneTasmania/MM_RawData_Pre2010_AbaloneTasmania.csv", header = T, sep = ",")

coastalwaters <-
 read.csv(file = "R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/MM_2007_2010_by_Processor/Coastal Waters/MM_RawData_Pre2010_Coastal Waters.csv", header = T, sep =
           ",")
mkhaulage <-
 read.csv(file = "R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/MM_2007_2010_by_Processor/MK Haulage/MM_RawData_Pre2010_MK Haulage.csv", header = T, sep =
           ",")
ralphs <-
 read.csv(file = "R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/MM_2007_2010_by_Processor/Ralph's/MM_RawData_Pre2010_Compiled_Ralphs.csv", header = T, sep =
           ",")
tasliveabalone <-
 read.csv(file = "R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/MM_2007_2010_by_Processor/Tas Live Abalone/MM_RawData_Pre2010_Tas Live Abalone.csv", header = T, sep =
           ",")
tfsmargate <-
 read.csv(file = "R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/MM_2007_2010_by_Processor/Tasmanian Seafoods Margate/MM_RawData_Pre2010_Compiled_TFS_Margate.csv", header = T, sep =
           ",")
tfssmithton <-
 read.csv(file = "R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/MM_2007_2010_by_Processor/Tasmanian Seafoods Smithton/MM_RawData_Pre2010_TFS_Smithton.csv", header = T, sep =
           ",")

## check for int of docket; if bad search for records

str(abalonetasmania) ##good but [30] = 0
unique(abalonetasmania$Docket)

str(coastalwaters) ##good but [15] = 0
unique(coastalwaters$Docket)

str(mkhaulage) ##good
unique(mkhaulage$Docket)

str(ralphs) ##bad
unique(ralphs$Docket) ## [34] = 0, [365, 367] = additional number in parentheses, [371] = NA

str(tasliveabalone)##good
unique(tasliveabalone$Docket)

str(tfsmargate) ##bad
unique(tfsmargate$Docket) ##[140] = 0, [468, 469, 471, 474, 476, 477] = have additional characters, [478] = NA

str(tfssmithton) ##good but [10] = 0
unique(tfssmithton$Docket)

class(all_data$Date)
all_data_backup <- all_data
#all_data <- all_data_backup
all_data$Date.2 <- as.Date(all_data$Date, format = "%d/%m/%Y")
unique(all_data$Date)
