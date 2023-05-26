library(tidyverse)
library(dplyr)
library(openxlsx)

# Set working directory to user OneDrive folder

setwd(sprintf("C:/Users/%s/OneDrive - University of Tasmania/Jaime McAllister - VUE Export Data",
                  Sys.info()[["user"]]))

# Import .csv acoustic detection data for schoolies and elephants (Note: elephants split by years to reduce VUE export size)

shark_dat <- list.files(path = sprintf("C:/Users/%s/OneDrive - University of Tasmania/Jaime McAllister - VUE Export Data",
        Sys.info()[["user"]]), pattern = "*.csv") %>%
 map_df(~read_csv(.))

shark_meta_dat <- read.xlsx("AcousticTagDeployments_June2014_JaimeMcAllister_VUE TagID.xlsx",
                  detectDates = T)

# Join meta data to detection data

shark_nurse_dat <- left_join(shark_dat, shark_meta_dat, by = c("Transmitter" = "VUE.TagID.(Freq-Space-ID)"))

# Quick summary of detection counts by species

shark_nurse_dat %>% group_by(CommonName) %>% 
 summarise(n_detections = n(),
           n = n_distinct(Transmitter))

# Clean station/site names for shark nursery acoustic data
shark_nurse_dat <- shark_nurse_dat %>% 
 filter(!is.na(`Station Name`)) %>% 
 mutate(`Station Name` = gsub('UP', 'UPW', `Station Name`),
        `Station Name` = gsub('LP', 'LPW', `Station Name`),
        `Station Name` = gsub('LPWW', 'LPW', `Station Name`))

