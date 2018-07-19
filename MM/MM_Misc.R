df <- data.frame(x = c("x: 123", "y: error: 7"))
df %>% separate(x, c("key", "value", "third"), ": ", extra = "merge")
df %>% separate(x, into=c("key", "value", "third"), sep= ":", fill= "right" )

#colnames(compiled.df) <- tolower(colnames(compiled.df))

test <- compiled.df %>% separate(subblocklist, into=c("sub1", "sub2", "sub3", "sub4"), sep = ", " , fill= "right", remove = FALSE ) %>%
 as.data.frame()

## recode fishyear
compiled.df$fishyear2 <- year(compiled.df$msr.date)

 block13E <- filter(compiled.df, grepl("13E", subblocklist))

ggplot(block13E) + geom_histogram(aes(x=shell.length, group = fishyear2)) + facet_grid(~ fishyear2)

table(compiled.df$fishyear, compiled.df$fishyear2)
table(block13E$fishyear, block13E$fishyear2)


pick <- which(is.na(block13E$fishyear))
length(pick)

head(compiled.df[pick,])


