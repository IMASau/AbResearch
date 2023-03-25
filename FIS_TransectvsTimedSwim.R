# extract timed swim survey data for black reef in 2021
std.ts.dat <- readRDS(paste(samp.year.folder, '/std.ts.dat.RDS', sep = ''))

df.1 <- std.ts.dat %>% 
 filter(site %in% c('AB-BRS-LEG-1-0', 'AB-BRS-LEG-2-0')) %>% 
  group_by(site, sizeclass.2021) %>% 
  summarise(n = sum(sizeclass_freq)) %>% 
  mutate(freq = n / sum(n),
         method = 'timedswim',
         string = if_else(grepl('LEG-1', site), '1', '2')) %>% 
 dplyr::rename(ts.size.class = sizeclass.2021)

#extract FIS leg survey data for black reef corresponding to 2021 timed swim survey dates
arm.leg.sl <- readRDS('C:/CloudStor/R_Stuff/FIS/arm.leg.sl.RDS')

df.2 <- arm.leg.sl %>% 
 filter(site == 'BRS' &
         sampmethod == 'LEG' &
         sampyear == 2021 &
         season == 'Summer') %>% 
 mutate(ts.size.class = cut(sllength.leg, breaks = c(0, 100, 120, 140, 160, 180, 200, 220), 
                            labels = c('0-100', '100-120', '120-140', '140-160', '160-180', '180-200', '200-220'))) %>% 
 group_by(site, string, ts.size.class) %>% 
 summarise(n = n()) %>% 
 mutate(freq = n / sum(n),
        method = 'transect',
        site.string = paste(site, string, sep = '-'),
        string = if_else(grepl('BRS-1', site.string), '1', '2'))

#combine data
df.3 <- bind_rows(df.1, df.2) %>% 
 mutate(site.string = if_else(is.na(site.string), paste('BRS', string, sep = '-'), site.string),
        method = replace(method, method == 'timedswim', 'Timed swim'),
        method = replace(method, method == 'transect', 'Transect'))

#summarise data
df.4 <- df.3 %>% 
 group_by(method, site.string) %>% 
 summarise(n = sum(n)) %>% 
 mutate(label = paste('n = ', n))

ts.vs.trans <- df.3 %>% 
 filter(freq > 0) %>% 
 ggplot(aes(x = ts.size.class))+
 geom_line(aes(y = freq*100,
               group = factor(method),
               colour = factor(method)),
           stat = 'identity', position = position_dodge2(0.1),
           size = 1)+
 facet_grid(site.string ~ .)+
 theme_bw()+
 geom_vline(aes(xintercept = 3.5),
            linetype = 'dashed', colour = 'red', size = 0.5)+
 geom_text(data = df.4, aes(x = 4.1, y = 10, label = label, 
                                       colour = factor(method)), size = 3,
           show.legend = F, position = position_stack(), vjust = 0)+
 scale_color_manual(values = c('red', 'blue'))+
 xlab("Shell Length (mm)")+
 ylab(paste("Percentage (%)"))+
 guides(size = 'legend', 
        colour = guide_legend(title = 'Survey method'))+
 theme(legend.position = c(0.9, 0.9))

setwd(ts.plots.folder)
ggsave(filename = paste('BRS-2021_TimedSwimSurvey_vs_Transect', '.pdf', sep = ''),
       plot = ts.vs.trans, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('BRS-2021_TimedSwimSurvey_vs_Transect', '.png', sep = ''),
       plot = ts.vs.trans, units = 'mm', width = 190, height = 250)

# vessel track
       