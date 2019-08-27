source("C:/GitCode/AbResearch/stderr.r")

## add month.yr variable to dataframe
stagedat <- stagedat %>%
 mutate(sample_month = lubridate::month(sample_date, label = TRUE, abbr = TRUE),
        sample_year = year(sample_date),
        month.yr = interaction(sample_month, sample_year))

## summarise the stagedat dataframe for each gonad stage recorded in each month.yr
stagedat.summ <- stagedat %>%
 group_by(sample_year, sample_month, month.yr) %>%
 summarise_each(funs(mean), pc_rec_dev:pc_necrotic)

## ensure month.yr is a factor
stagedat.summ$month.yr <- as.factor(stagedat.summ$month.yr)

## transform the stagedat summary from wide to long format
stagedat.summ.long <- melt(stagedat.summ, id.vars = c('sample_year', 'sample_month', 'month.yr'))

## plot mean for each gonad stage for month.yr
stagedat.summ.long %>% 
 ggplot(aes(x = month.yr, y = value, colour = variable, group = variable)) +
 geom_point()+
 geom_line() +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))

## plot mean for each gonad stage for month and year 
stagedat.summ.long %>% 
 ggplot(aes(x = sample_month, y = value, colour = variable, group = 1)) +
 geom_point()+
 geom_line() +
 facet_grid(variable ~ sample_year) + 
 ylab('Area of mature gonad (%)') +
 xlab('Month') +
 theme_bw() +
 theme(legend.position = 'none')+
 theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
 

## plot mean for each gonad stage for month and year with error bars

## select gonad pc stage data and convert to long format
stagedat.1 <- stagedat %>%
 select(abalone_id,sample_year, sample_month, month.yr, 
        pc_rec_dev, pc_locules, pc_mature, pc_spawning, pc_necrotic) %>% 
 melt(id.vars = c('abalone_id', 'sample_year', 'sample_month', 'month.yr'))

## summarise gonad pc stage data for mean and se
stagedat.summ.long.1 <- stagedat.1 %>% 
 group_by(sample_year, sample_month, month.yr, variable) %>%
 summarise(mean.pc = mean(value),
           se.pc = stderr(value))

## plot mean for each gonad stage for month and year including error bars
stagedat.summ.long.1 %>% 
 ggplot(aes(x = sample_month, y = mean.pc, colour = variable, group = 1)) +
 geom_point()+
 geom_line() +
 geom_errorbar(aes(ymin = mean.pc - se.pc, ymax = mean.pc + se.pc), width = 0.5)+
 facet_grid(variable ~ sample_year) +
 ylab('Area of mature gonad (%)') +
 xlab('Month') +
 theme_bw() +
 theme(legend.position = 'none')+
 theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

## plot mean pc of each gonad stage in each month (all years)
stagedat.summ.long.1 %>% 
 ggplot(aes(x = sample_month, y = mean.pc, fill = variable)) +
 geom_bar(position = 'fill', stat = 'identity') +
 scale_y_continuous(labels = scales::percent_format()) +
 ylab('Area of mature gonad (%)') +
 xlab('Month')

## plot mean pc of each gonand stage in each yr.month
stagedat.summ.long.1 %>% 
 ggplot(aes(x = month.yr, y = mean.pc, fill = variable)) +
 geom_bar(position = 'fill', stat = 'identity') +
 scale_y_continuous(labels = scales::percent_format()) +
 ylab('Area of mature gonad (%)') +
 xlab('Month') +
 theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
