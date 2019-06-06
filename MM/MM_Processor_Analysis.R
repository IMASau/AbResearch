## LF boxplot by processor to examine if there are market drivers of size structure information

## quick summary of the number of abalone measured each processor for a time period, 
## removing processors who measured < 500 animals (approx. five catches)
processor.n <- compiledMM.df.final %>%
 filter(fishyear >= 2000) %>%
 group_by(fishyear, processorname) %>%
 summarise(n = n()) %>%
 arrange(fishyear, desc(n)) %>%
 filter(n > 500)

## 1. select zone and region
zone <- 'W'
region <- 'PerkinsBay'
processor <- "RALPH'S TASMANIAN SEAFOOD PTY LTD"

## 2. extract data
plotdat <-
 compiledMM.df.final %>% 
 filter(newzone %in% zone &
         same.region == 1 &
         (between(shell.length, 100, 220)) & 
         (between(fishyear, 2000, 2015)) &
         processorname == processor)

## 3. convert required grouping variable to factor for boxplot
plotdat$fishyear <- as.factor(plotdat$fishyear)
#plotdat$fishquarter <- as.factor(plotdat$fishquarter)

## 4. generate a count of records for each year to add to boxplot
plotdat.n <- plotdat %>% 
 group_by(fishyear) %>% 
 summarize(n = n())

## 5. generate boxplot of shell lengths for chosen grouping variable above.
mm.zone.boxplot <- ggplot(plotdat, aes(x = fishyear, y = shell.length)) + 
 geom_boxplot(outlier.colour = "orange", outlier.size = 1.5) +
 geom_text(data = plotdat.n, aes(y = 220, label = n,), size = 3, angle = 90) +
 geom_hline(aes(yintercept = 145), colour = 'red', linetype = 'dotted')+
 geom_hline(aes(yintercept = 132), colour = 'darkgreen', linetype = 'dotted')+
 #ggtitle('Western Zone 1967-2016') +
 xlab('Year') +
 ylab('Shell Length (mm)')+ 
 coord_cartesian(ylim = c(100, 225))+
 theme_bw() + 
 theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
       axis.text.x = element_text(angle = 0, vjust = 0.5))

print(mm.zone.boxplot)
