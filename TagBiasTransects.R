library(dplyr)

## create tagging methods
tag.method <- c('spring.insitu', 'spring.deck', 'rivet.insitu', 'rivet.deck')
tag.method.short <- c('s.i', 's.d', 'r.i', 'r.d' ) 

## establish proportion of sample to be tagged with each method (in this case equal proportions)
prob <- c(0.25, 0.25, 0.25, 0.25)

## create dataframe of each belt transect and randomise the tagging method to apply
df.1 <- data.frame('belt.transect' = rep.int(seq(1, 24, 1), 1),
                   'tag.method' = sample(rep(tag.method, 24*prob)))

## check for equal distribution of tagging methods
df.1 %>% 
        group_by(tag.method) %>% 
        summarise(n = n())

## export table        
write.table(df.1, 'C:/Users/jaimem/Documents/Abalone_research/AB_tagging/tagging.method.transect.belt.txt', sep = '\t')

