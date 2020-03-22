#clear environment
rm(list=ls(all=TRUE))
library(tidyverse)
library(broom)
library(lubridate)
library(GGally)
library(scales)
library(gdata)
library(effsize)
library(openxlsx)
library(Hmisc)
library(reshape2)
library(dplyr)
library(plotrix)
library(multcomp)
library(DescTools)
library(RVAideMemoire)
library(kSamples)
library(PMCMRplus)

tag.bias.data <- read.xlsx(
 "C:/Users/jaimem/Documents/Abalone_research/AB_tagging/Abalone_TagBiasExperiment_TaggingData_March2020_JaimeMcAllister.xlsx",
 sheet = "Sheet1",
 detectDates = TRUE
)

tag.summary <- tag.bias.data %>% 
 group_by(tag.method) %>% 
 summarise(tagged = paste('n =', n()),
           min.sl = min(`shell.length.(mm)`),
           max.sl = max(`shell.length.(mm)`),
           med.sl = median(`shell.length.(mm)`),
           avg.sl = mean(`shell.length.(mm)`))

tag.bias.data$tag.method <- factor(tag.bias.data$tag.method)

tag.summary.plot <- ggplot(tag.bias.data, aes(x = `shell.length.(mm)`, fill = tag.method, color = tag.method)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 theme_bw()+
 geom_histogram(alpha = 0.2, binwidth = 5) +
 # xlim(50, 170)+
 ylim(0, 60)+
 theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
 theme(legend.position="none") +
 facet_grid(tag.method ~ ., scales = "free_y")+
 geom_vline(aes(xintercept = 138), colour = 'red', linetype = 'dashed', size = 0.5)+
 geom_vline(aes(xintercept = 120), colour = 'blue', linetype = 'dashed', size = 0.5)+
 geom_vline(aes(xintercept = 80), colour = 'blue', linetype = 'dashed', size = 0.5)+
 geom_text(data = tag.summary, aes(x = 160, y = 50, label = tagged), 
           colour = 'black', inherit.aes = F, parse = F, size = 3.5)

print(tag.summary.plot)

setwd('C:/CloudStor/R_Stuff/FIS')
ggsave(filename = paste('TAG_BIAS_EXPERIMENT_LF_', '.pdf', sep = ''),
       plot = tag.summary.plot, units = 'mm', width = 200, height = 120)
ggsave(filename = paste('TAG_BIAS_EXPERIMENT_LF_', '.wmf', sep = ''),
       plot = tag.summary.plot, units = 'mm', width = 200, height = 120)
ggsave(filename = paste('TAG_BIAS_EXPERIMENT_LF_', '.png', sep = ''),
       plot = tag.summary.plot, units = 'mm', width = 200, height = 120)


tag.summary.boxplot <- ggplot(tag.bias.data, aes(x = tag.method, y = `shell.length.(mm)`)) +
 geom_boxplot(outlier.colour = "orange", outlier.size = 1.5)+
 theme_bw()+
 theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
 theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
 xlab('Tag method')+
 ylab(bquote('Shell length ('*mm*')'))+
 geom_hline(yintercept = 138, colour = 'red', linetype = 'dashed', size = 0.5)+
 geom_hline(yintercept = 120, colour = 'blue', linetype = 'dashed', size = 0.5)+
 geom_hline(yintercept = 80, colour = 'blue', linetype = 'dashed', size = 0.5)+
 geom_text(data = tag.summary, aes(y = 165, label = tagged,), size = 3, angle = 0)

 print(tag.summary.boxplot)
 
 setwd('C:/CloudStor/R_Stuff/FIS')
 ggsave(filename = paste('TAG_BIAS_EXPERIMENT_BOX_', '.pdf', sep = ''),
        plot = tag.summary.boxplot, units = 'mm', width = 200, height = 120)
 ggsave(filename = paste('TAG_BIAS_EXPERIMENT_BOX_', '.wmf', sep = ''),
        plot = tag.summary.boxplot, units = 'mm', width = 200, height = 120)
 ggsave(filename = paste('TAG_BIAS_EXPERIMENT_BOX_', '.png', sep = ''),
        plot = tag.summary.boxplot, units = 'mm', width = 200, height = 120)
 