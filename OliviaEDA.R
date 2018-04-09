library(sp)
library(rgdal)
library(tidyverse)
library(lubridate)


load("R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/30. Centro Re Survey/Olivia - honours/divermaster_all.RData")

## Export spatial data
##Write to shapefile
#writeOGR(divermaster.all, dsn=paste("D:\\R_Stuff\\RAbTrack", "alldivers.shp", sep="/") ,layer="alldivers", driver="ESRI Shapefile" )


##Extract data slot from the spatialpolygondatafframe
divermaster.df <- divermaster.all@data

coords <- coordinates(divermaster.all)
colnames(coords) <- c("long", "lat")

divermaster.df <- cbind(divermaster.df, coords)

##-------------------------------------------------------------------------##
## Section to organise and prepare dataframe for analysis ####

## look at the structure oif the df (df = dataframe)
str(divermaster.df)

### rname the "Height variabel to depthband
divermaster.df <- rename(divermaster.df, depthband = Height)

## print a list of the unique depthband values
unique(divermaster.df$depthband)

## cross check the number of records in depthband
table(divermaster.df$depthband)

## factorise and re-order the levels of the depthband field
divermaster.df$depthband <- ordered(divermaster.df$depthband, levels=c("0.00 - 5.00", "5.00 - 10.00", "10.00 - 15.00", "15.00 - 20.00", "20.00 - 25.00", "25.00 - 30.00", "30.00 - 35.00", "35.00 - 40.00", "40.00 - 45.00"))
unique(divermaster.df$depthband)

## check that you still have the same number of records in each group
pick <- which(!is.na(divermaster.df$med.bc))
table(divermaster.df$depthband[pick])


##-------------------------------------------------------------------------##
## Section to create summary dataframes for graphing ####
filter(divermaster.df, !is.na(med.bc)) %>% 
     summarise(n=n())

View(filter(divermaster.df, !is.na(med.bc)) %>% 
      group_by(depthband) %>%
      summarise(n=n()))

View(filter(divermaster.df, !is.na(med.bc)) %>% 
     group_by(depthband, med.bc) %>%
     summarise(n=n()))

filter(divermaster.df, !is.na(med.bc)) %>% 
     group_by(depthband, med.bc) %>%
     summarise(n=n()) %>%
     tidyr::spread(depthband)



summary.depth <- filter(divermaster.df, !is.na(med.bc)) %>% 
  group_by(depthband, med.bc) %>%
  summarise(n=n())

##-------------------------------------------------------------------------##
## Create graphs ####
ggplot(summary.depth, aes(x=depthband, y=n, fill=factor(med.bc))) +
  geom_histogram( stat='identity')


ggplot(summary.depth, aes(x=depthband, y=n, fill=factor(med.bc))) +
  geom_bar(position = "fill",stat = "identity")

#Spatial code
summary.depth.wave <- filter(divermaster.df, !is.na(med.bc) & med.bc > 4) %>% 
  group_by(depthband) %>%
  summarise(med2=median(med.bc),
            mnwv=mean(logwvindex),
                      n=n())

## EDA ####






