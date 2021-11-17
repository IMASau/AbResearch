
library(openxlsx)
SexRatio <- samdata %>% group_by(Site_Name, SamplePeriod, SizeC,Sex) %>% summarise(n=n()) %>% 
 pivot_wider(id_cols = c(Site_Name, SamplePeriod, SizeC), names_from = Sex, values_from = n, values_fill = list(freq =0))


wb <- loadWorkbook("CatchbyDiver.xlsx")
addWorksheet(wb, "blacklip")
addWorksheet(wb, "greenlip")
writeData(wb, sheet = "blacklip", ce.summary.bl, colNames = T)
writeData(wb, sheet = "greenlip", ce.summary.gl, colNames = T)
saveWorkbook(wb,"CatchbyDiver.xlsx",overwrite = T)

## Using openxlsx
wb <- createWorkbook("d:/SexRatio.xlsx")
addWorksheet(wb, "SexRatio")
writeData(wb, sheet = "SexRatio", SexRatio, colNames = T)
saveWorkbook(wb,"d:/SexRatio.xlsx",overwrite = T)

