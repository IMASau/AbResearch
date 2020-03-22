plot.zone <- 'G'

plotdat.block <- compiledMM.df.final %>% 
 filter(newzone == plot.zone
        & between(fishyear, 1981, 2019)
        & between(shell.length, 100, 220))

# convert required grouping variable to factor for boxplot

plotdat.block$fishyear <- as.factor(plotdat.block$fishyear)

# generate a count of records for each year to add to boxplot

plotdat.n <- plotdat.block %>% 
 group_by(fishyear) %>% 
 summarize(n = n())

# generate boxplot of shell lengths for chosen grouping variable

mm.zone.boxplot <- ggplot(plotdat.block, aes(x = fishyear, y = shell.length)) + 
 geom_boxplot(outlier.colour = "orange", outlier.size = 1.5) +
 geom_text(data = plotdat.n, aes(y = 220, label = n,), size = 3, angle = 90) +
 geom_hline(aes(yintercept = 140), colour = 'red', linetype = 'dotted')+
 xlab(paste('Zone', plot.zone)) +
 # ylab(paste('BlockNo', i, 'Shell Length (mm)'))+ 
 ylab('Shell length (mm)')+
 coord_cartesian(ylim = c(100, 225))+
 theme_bw() + 
 theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
       axis.text.x = element_text(angle = 90, vjust = 0.5))

print(mm.zone.boxplot)

ggsave(filename = paste('Zone', plot.zone, '_1981-2019', '.pdf', sep = ''), 
       plot = mm.zone.boxplot, width = 7.4, height = 5.57, units = 'in')

##########################
library(dplyr)
library(tidyr)
library(readxl)
library(gsubfn)

# size.limits <- read.csv("C:/Users/jaimem/Desktop/AbaloneSizeLimits2.csv")
size.limits <- read.csv("C:/Users/jaimem/Desktop/AbaloneSizeLimits2.csv", fileEncoding="UTF-8-BOM")

colnames(size.limits) <- tolower(colnames(size.limits))
names(size.limits) <- gsub('.', '-', names(size.limits), fixed = T)

size.limits.tab <- size.limits %>%
        gather(monthyear, sizelimit, `jan-1962`:`dec-2019`) %>% 
        mutate(monthyear = gsub('jan', 1, monthyear)) %>% 
        mutate(monthyear = gsub('feb', 2, monthyear)) %>% 
        mutate(monthyear = gsub('mar', 3, monthyear)) %>% 
        mutate(monthyear = gsub('apr', 4, monthyear)) %>% 
        mutate(monthyear = gsub('may', 5, monthyear)) %>% 
        mutate(monthyear = gsub('jun', 6, monthyear)) %>% 
        mutate(monthyear = gsub('jul', 7, monthyear)) %>% 
        mutate(monthyear = gsub('aug', 8, monthyear)) %>% 
        mutate(monthyear = gsub('sep', 9, monthyear)) %>% 
        mutate(monthyear = gsub('oct', 10, monthyear)) %>% 
        mutate(monthyear = gsub('nov', 11, monthyear)) %>% 
        mutate(monthyear = gsub('dec', 12, monthyear)) %>% 
        mutate(sizelimit.index = paste(abzone, subblockno, monthyear, sep = '-')) %>% 
        select(sizelimit.index, sizelimit)

compiledMM.df.final <- compiledMM.df.final %>%
 mutate(subblocklist = ifelse(is.na(subblocklist), subblockno, subblocklist)) %>%
 mutate(blocklist = ifelse(is.na(blocklist), as.numeric(gsub("([0-9]+).*$", "\\1", subblocklist)), blocklist)) %>%
 mutate(numblocks = count.fields(textConnection(blocklist), sep = ',')) %>%
 mutate(numsubblocks = count.fields(textConnection(subblocklist), sep = ','))

compiledMM.df.final <- compiledMM.df.final %>%
        mutate(sizelimit.index = if_else(
                numsubblocks == 1 & subblockno %in% c('A', 'B', 'C', 'D', 'E'),
                paste(
                        newzone,
                        blockno,
                        lubridate::month(daylist_max),
                        fishyear,
                        sep = '-'
                ),
                if_else(
                        numsubblocks == 1 & is.na(subblockno),
                        paste(
                                newzone,
                                blockno,
                                lubridate::month(daylist_max),
                                fishyear,
                                sep = '-'
                        ),
                if_else(
                        numsubblocks == 1,
                        paste(
                                newzone,
                                subblockno,
                                lubridate::month(daylist_max),
                                fishyear,
                                sep = '-'
                        ),
                        NA_character_
                        
                )
        )))

df.2 <- left_join(compiledMM.df.final, size.limits.tab, "sizelimit.index")


df.1 %>% 
        filter(sizelimit.index == 'E-13-4-1987') %>% 
        select(sizelimit)
        

compiledMM.df.final %>% 
        filter(numsubblocks < 2) %>% 
        distinct(sizelimit.index)

unique(compiledMM.df.final$sizelimit.index)
##########################
tas.live.lobster <- bind_rows(IMAS055, IMAS056)

str(tas.live.lobster)
unique(tas.live.lobster$docketnum)

tas.live.lobster.dat <- tas.live.lobster %>% 
        filter(docketnum %in% c(812108, 812302))

tas.live.lobster.dat <- tas.live.lobster.dat %>% 
        mutate(grade = dplyr::if_else(wholeweight <= 400, 'xsmall',
                                      dplyr::if_else(between(wholeweight, 401, 600), 'small',
                                                     dplyr::if_else(between(wholeweight, 601, 800), 'medium', 'large'))))


df.1 <- tas.live.lobster.dat %>% 
        group_by(docketnum) %>% 
        summarise(ab.meas = n())

df.2 <- tas.live.lobster.dat %>% 
        group_by(docketnum, grade) %>% 
        summarise(grade.meas = n())

df.3 <- left_join(df.2, df.1, by = 'docketnum') %>% 
        mutate(grade.perc = (grade.meas / ab.meas) * 100)

df.4 <- tas.live.lobster.dat %>% 
        group_by(docketnum) %>% 
        summarise(mean.weight = mean(wholeweight),
                  min.weight = min(wholeweight),
                  max.weight = max(wholeweight),
                  mean.size = mean(shelllength),
                  min.size = min(shelllength),
                  max.size = max(shelllength))

df.6 <- df.3 %>% 
        group_by(docketnum, grade) %>% 
        mutate(position2 = cumsum(grade.perc - 0.5 * grade.perc))

ggplot(df.6, aes(x = 1, y = grade.perc, fill = grade))+
        geom_bar(width = 1, stat = 'identity', position = position_fill())+
        coord_polar(theta = 'y')+
        theme(axis.ticks = element_blank(), axis.title = element_blank(), 
              axis.text.y = , panel.grid  = element_blank(),
              axis.text.x = element_blank())+ 
        geom_text(aes(label = sprintf("%1.2f%%", 100 * grade.perc), y = position2))+
        facet_grid(. ~ docketnum)        

############
library(dplyr)
library(tidyr)


