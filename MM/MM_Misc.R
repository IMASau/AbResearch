compiled.df.backup <-  compiled.df


df <- data.frame(x = c("x: 123", "y: error: 7"))
df %>% separate(x, c("key", "value", "third"), ": ", extra = "merge")
df %>% separate(x, into=c("key", "value", "third"), sep= ":", fill= "right" )

#colnames(compiled.df) <- tolower(colnames(compiled.df))

compiled.df  <- compiled.df %>% separate(blocklist, into=c("bl1", "bl2", "bl3", "bl4", "bl5"), sep = ", " , fill= "right", remove = FALSE ) %>%
 as.data.frame()

## recode fishyear
compiled.df$fishyear2 <- year(compiled.df$msr.date)


## see the link below
##https://dplyr.tidyverse.org/reference/filter_all.html
##work out a way to filter for multiple blocks on multiple columns
##Note: the grepl function pulls out out nore records than we want e.g.
## grepl ("9" ...  finds all rows with a 9 in the blocklist column (e.g. 9, 19, 29 etc)
## So this is not what we want



## Filter data by block
x <- c("10", "9", "11")
plotdat <- filter(compiled.df, grepl("9", blocklist), between(shell.length, 132, 200) )
plotdat <- filter(compiled.df, grepl(paste(x, collapse = "|"), blocklist), between(shell.length, 132, 200) )
unique(plotdat$blocklist)

ggplot(plotdat) + geom_histogram(aes(x=shell.length, group = fishyear2)) + facet_grid(~ fishyear2)
ggplot(plotdat) + geom_density(aes(x=shell.length, group = factor(fishyear2), colour = factor(fishyear2), alpha= 2)) + scale_fill_brewer(palette = "Dark2")


table(compiled.df$fishyear, compiled.df$fishyear2)
table(block13E$fishyear, block13E$fishyear2)


pick <- which(is.na(block13E$fishyear))
length(pick)

head(compiled.df[pick,])

compiled.df[423860,]

