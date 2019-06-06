## Recode points into BL zones

codeBlnewzone <- function(Bl) {
 {
  Bl$newzone[grepl(paste0('\\b', c(seq(7, 12, 1)), '\\b', collapse = '|'), Bl$blocklist) | grepl(paste0('\\b', c("13A", "13B", "06D"), '\\b', collapse = '|'), Bl$subblockno)] <- "W"
  Bl$newzone[grepl(paste0('\\b', c(seq(32, 38, 1), seq(41, 49, 1), seq(50, 57, 1)),'\\b', collapse = '|'), Bl$blocklist)] <- "BS" # must be before Northern
  Bl$newzone[grepl(paste0('\\b', c('01', 2, 3, 4, 5, 39, 40), '\\b', collapse = '|'), Bl$blocklist)| grepl(paste0('\\b', c('01A', '01B', '01C', '6A', '6C', '6D', "06A", "06B", "06C", "49D", "31B"), '\\b', collapse = '|'), Bl$subblockno)] <- "N"
  Bl$newzone[grepl(paste0('\\b', c(seq(14, 30, 1)), '\\b', collapse = '|'), Bl$blocklist) | grepl(paste0('\\b', c("13C", "13D", "13E", "31A"), '\\b', collapse = '|'), Bl$subblockno)] <- "E"
  
 } 
 return(Bl)
}

## Recode points into historic BL zones

codeBlnewZoneHistoric <- function(Bl) {
 {
  Bl$newzone[grepl(paste0('\\b', c(seq(13, 31, 1)), '\\b', collapse = '|'), Bl$blocklist)] <- "E"
  Bl$newzone[grepl(paste0('\\b', c(seq(7, 12, 1)), '\\b', collapse = '|'), Bl$blocklist)] <- "W"
  Bl$newzone[grepl(paste0('\\b', c(1, 2, 3, 4, 5, 6, 39, 40), '\\b', collapse = '|'), Bl$blocklist)] <- "N"
  Bl$newzone[grepl(paste0('\\b', c(seq(32, 38, 1), seq(41, 49, 1), seq(50, 57, 1)), '\\b', collapse = '|'), Bl$blocklist)] <- "BS"
  
 }
 return(Bl)
}

## Recode points into Greenlip regions
codeGlregion <- function(Gl) {
 {
  Gl$gl.region[Gl$blockno %in% c(1, 2, 3, 4)] <- "KingIsland" 
  Gl$gl.region[Gl$blockno %in% c(5, 48, 49) | Gl$subblockno %in% c("48B", "48C", "48D", "49C")] <- "NorthWest"
  Gl$gl.region[Gl$subblockno %in% c("48A")] <- "PerkinsBay"
  Gl$gl.region[Gl$blockno %in% c(30, 31,39,40)] <- "NorthEast" 
  Gl$gl.region[Gl$blockno %in% c(41, 42, 43, 44, 45, 46, 47)] <- "CentralNorth" 
  Gl$gl.region[Gl$blockno %in% c(32, 33, 34, 35, 36, 37, 38)] <- "FurneauxGroup" 
  Gl$gl.region[Gl$blockno %in% c(51, 52, 53, 54, 55)] <- "BassStraitIslands" 
  
 }
 return(Gl)
}


## Recode points into Greenlip Regions
codeGlRegionHistoric <- function(Gl) {
 {
  Gl$gl.region[Gl$blockno %in% c(1, 2, 3, 4)] <- "KingIsland"
  Gl$gl.region[Gl$blockno %in% c(5, 48, 49)] <- "NorthWest"
  Gl$gl.region[Gl$blockno %in% c(30, 31, 39, 40)] <- "NorthEast"
  Gl$gl.region[Gl$blockno %in% c(41, 42, 43, 44, 45, 46, 47)] <- "CentralNorth"
  Gl$gl.region[Gl$blockno %in% c(32, 33, 34, 35, 36, 37, 38)] <- "FurneauxGroup"
  Gl$gl.region[Gl$blockno %in% c(51, 52, 53, 54, 55)] <- "BassStraitIslands"
  
 }
 return(Gl)
}

## Historic MM data systems (i.e. without subblockno) ####
## Recode points into Blacklip Regions

codeBlRegionHistoric <- function(Bl) {
 {
  Bl$bl.region[Bl$blockno %in% c(13)] <- "Actaeons"
  Bl$bl.region[Bl$blockno %in% c(14, 15)] <- "Channel"
  Bl$bl.region[Bl$blockno %in% c(16)] <- "BrunyIsland"
  Bl$bl.region[Bl$blockno %in% c(17, 18, 19, 20, 21)] <- "StormBay"
  Bl$bl.region[Bl$blockno %in% c(22, 23, 24)] <- "Fortescue"
  Bl$bl.region[Bl$blockno %in% c(25, 26, 27, 28)] <- "BichenoFreycinet"
  Bl$bl.region[Bl$blockno %in% c(29, 30, 31)] <- "StHelens"
  Bl$bl.region[Bl$blockno %in% c(39, 40)] <- "NorthEast"
  Bl$bl.region[Bl$blockno %in% c(1, 2, 3, 4)] <- "KingIsland"
  Bl$bl.region[Bl$blockno %in%  c(5, 6)] <- "NorthWest"
  Bl$bl.region[Bl$blockno %in%  c(47, 48, 49)] <- "HunterIsland"
  Bl$bl.region[Bl$blockno %in% c(7, 8)] <- "Granville"
  Bl$bl.region[Bl$blockno %in% c(9)] <- "Strahan"
  Bl$bl.region[Bl$blockno %in% c(10, 11)] <- "SouthWest"
  Bl$bl.region[Bl$blockno %in% c(12)] <- "SouthCoast"
  Bl$bl.region[Bl$blockno %in% c(32, 33, 34, 35, 36, 37, 38)] <- "FurneauxGroup"
  Bl$bl.region[Bl$blockno %in% c(41, 42, 43, 44, 45, 46)] <- "CentralNorth"
  Bl$bl.region[Bl$blockno %in% c(50, 51, 52, 53, 54, 55, 56, 57)] <- "BassStraitIslands"
  
 }
 return(Bl)
}

## Recode points into Blacklip regions
codeBlregion <- function(Bl) {
 {
  Bl$bl.region[Bl$blockno %in% c(13)] <- "Actaeons"
  Bl$bl.region[Bl$blockno %in% c(14, 15)] <- "Channel"
  Bl$bl.region[Bl$blockno %in%  c(16,17,18,19,20,21)] <- "StormBay"
  Bl$bl.region[Bl$blockno %in%  c(22, 23, 24)] <- "Fortescue"
  Bl$bl.region[Bl$blockno %in%  c(25, 26, 27, 28)]  <- "BichenoFreycinet"
  Bl$bl.region[Bl$blockno %in%  c(29,30,31)] <- "StHelens"
  Bl$bl.region[Bl$blockno %in%  c(39, 40)] <- "NorthEast"
  Bl$bl.region[Bl$blockno %in%  c(5, 6)] <- "NorthWest"
  Bl$bl.region[Bl$blockno %in%  c(47, 48, 49)] <- "HunterIsland"
  Bl$bl.region[Bl$blockno %in% c(1, 2, 3, 4)] <- "KingIsland" 
  Bl$bl.region[Bl$blockno %in% c(7, 8)] <- "Granville"
  Bl$bl.region[Bl$blockno %in% c(9)] <- "Strahan" 
  Bl$bl.region[Bl$blockno %in% c(10,11)] <- "SouthWest"
  Bl$bl.region[Bl$blockno %in% c(12)] <- "SouthCoast"
  Bl$bl.region[Bl$blockno %in% c(32, 33, 34, 35, 36, 37, 38)] <- "FurneauxGroup" 
  Bl$bl.region[Bl$blockno %in% c(41, 42, 43, 44, 45, 46)] <- "CentralNorth" 
  Bl$bl.region[Bl$blockno %in% c(50, 51, 52, 53, 54, 55, 56, 57)] <- "BassStraitIslands"  
  
 }
 return(Bl)
}