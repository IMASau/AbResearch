## Plot 1: Length ####
# overlay length frequency histogram with boxplot

for (i in new.dockets) {
 plot.length.freq.dat <- measure.board.next.gen.df %>%
  filter(docketnum == i &
          between(shelllength, 120, 200))
 
 length.freq.plot <- ggplot(plot.length.freq.dat, aes(shelllength)) +
  geom_histogram(
   aes(y = ..density.. * 5),
   fill = '#EFC000FF',
   col = 'black',
   binwidth = 5,
   alpha = 0.6
  ) +
  coord_cartesian(xlim = c(130, 200), ylim = c(0, 0.4)) +
  theme_bw() +
  xlab("Shell Length (mm)") +
  ylab(paste("Docket no.", i, " Percentage (%)")) +
  # geom_vline(aes(xintercept = 138), colour = 'red',
  #            linetype = 'dashed', size = 0.5)+
  geom_vline(
   aes(xintercept = ifelse(zone == 'AW', 145, 138)),
   linetype = 'dashed',
   colour = 'red',
   size = 0.5
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1, suffix = ''))
 
 # print(length.freq.plot)
 
 xbp.len <- ggplot(plot.length.freq.dat,
                   aes(
                    x = factor(docketnum),
                    y = shelllength,
                    group = docketnum
                   )) +
  geom_boxplot(
   fill = 'lightgray',
   outlier.colour = "black",
   outlier.size = 1.5,
   position = position_dodge(0.85),
   width = 0.5
  ) +
  rotate() +
  theme_transparent()
 
 # print(xbp.len)
 
 xbp_grob <- ggplotGrob(xbp.len)
 xmin.len <- min(plot.length.freq.dat$shelllength)
 xmax.len <- max(plot.length.freq.dat$shelllength)
 
 length.plot <- length.freq.plot +
  annotation_custom(
   grob = xbp_grob,
   xmin = xmin.len,
   xmax = xmax.len,
   ymin = 0.3
  )
 
 setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2020ProcessorSummaries')
 file.zone <- unique(plot.length.freq.dat$zone)
 file.date <- unique(plot.length.freq.dat$plaindate)
 file.processor <- unique(plot.length.freq.dat$processor)
 ggsave(
  filename = paste(paste(file.zone, i, sep = ''), '_LENGTHSUMMARYPLOT_', file.date, '_', file.processor, '.pdf', sep = ''),
  plot = length.plot,
  width = 7.4,
  height = 5.57,
  units = 'in'
 )

## Plot 2: Weight####
# overlay weight frequency with boxplot and pie chart of grading

 plot.weight.freq.dat <- measure.board.next.gen.df %>%
  filter(docketnum == i &
          wholeweight != 0)
 
 weight.freq.plot <- ggplot(plot.weight.freq.dat, aes(wholeweight)) +
  geom_histogram(
   aes(y = ..density.. * 50),
   fill = '#0073C2FF',
   col = 'black',
   binwidth = 50,
   alpha = 0.6
  ) +
  coord_cartesian(xlim = c(300, 1200),
                  ylim = c(0, 0.35)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  xlab("Whole weight (g)") +
  ylab(paste("Docket no.", j, " Percentage (%)")) +
  geom_vline(aes(xintercept = 400),
             colour = 'red',
             linetype = 'dotted') +
  geom_vline(aes(xintercept = 600),
             colour = 'red',
             linetype = 'dotted') +
  geom_vline(aes(xintercept = 800),
             colour = 'red',
             linetype = 'dotted') +
  geom_text(
   data = grades,
   aes(
    x = x,
    y = y,
    label = lab
   ),
   inherit.aes = FALSE,
   vjust = 0.5,
   hjust = 0.5,
   size = 4,
   angle = 0,
   colour = c(
    'small' = '#EFC000FF',
    'medium' = '#0073C2FF',
    'large' = '#CD534CFF'
   )
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1, suffix = ''))
 
 # print(weight.freq.plot)
 
 xbp.wt <- ggplot(
  plot.weight.freq.dat,
  aes(
   x = factor(docketnum),
   y = wholeweight,
   group = docketnum
  )
 ) +
  geom_boxplot(
   fill = 'lightgray',
   outlier.colour = "black",
   outlier.size = 1.5,
   position = position_dodge(0.85),
   width = 0.3
  ) +
  stat_summary(fun.y = mean, geom = 'point', shape = 20, size = 3, colour = 'red', fill = 'red')+
  rotate() +
  theme_transparent()
 
 # print(xbp.wt)
 
 xbp_grob <- ggplotGrob(xbp.wt)
 xmin.wt <- min(plot.weight.freq.dat$wholeweight)
 xmax.wt <- max(plot.weight.freq.dat$wholeweight)
 
 # weight.freq.plot +
 #         annotation_custom(grob = xbp_grob, xmin = xmin.wt, xmax = xmax.wt,  ymin = 0.17)
 
 # add pie chart to weight frequency plot
 docketnum.grade.summary <-
  left_join(docknum.n.meas, docknum.grade.meas, by = 'docketnum') %>%
  mutate(grade.perc = round((grade.meas / ab.meas) * 100))
 
 pie.plot.dat <- docketnum.grade.summary %>%
  filter(docketnum == i) %>%
  mutate(
   lab.position = cumsum(grade.perc),
   lab.y.position = lab.position - grade.perc / 2,
   lab = paste0(grade.perc, "%")
  )
 
 docket.pie.plot <-
  ggplot(data = pie.plot.dat, aes(
   x = "",
   y = grade.perc,
   fill = grade
  )) +
  geom_bar(stat = "identity", colour = 'white') +
  coord_polar(theta = "y") +
  geom_text(
   aes(label = lab),
   position = position_stack(vjust = 0.5),
   colour = 'white',
   size = 5
  ) +
  theme_void() +
  theme(legend.position = 'none') +
  scale_fill_manual(
   values = c(
    'small' = '#EFC000FF',
    'medium' = '#0073C2FF',
    'large' = '#CD534CFF'
   )
  )
 # labs(fill = (paste('Grade')))
 
 
 # print(docket.pie.plot)
 
 pp_grob <- ggplotGrob(docket.pie.plot)
 
 wt.plot <- weight.freq.plot +
  annotation_custom(
   grob = xbp_grob,
   xmin = xmin.wt,
   xmax = xmax.wt,
   ymin = 0.25
  ) +
  annotation_custom(
   grob = pp_grob,
   xmin = 900,
   xmax = 1200,
   ymax = 0.4
  )
 
 setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2020ProcessorSummaries')
 file.zone <- unique(plot.weight.freq.dat$zone)
 file.date <- unique(plot.weight.freq.dat$plaindate)
 file.processor <- unique(plot.weight.freq.dat$processor)
 ggsave(
  filename = paste(paste(file.zone, i, sep = ''), '_WEIGHTSUMMARYPLOT_', file.date, '_', file.processor, '.pdf', sep = ''),
  plot = wt.plot,
  width = 7.4,
  height = 5.57,
  units = 'in'
 )

## combine arm and leg plot on the same page
plot.a <- grid.arrange(
 arrangeGrob(cowplot::plot_grid(wt.plot, length.plot, align = 'v', 
                                ncol = 1), ncol = 1))
ggsave(
 filename = paste(paste(file.zone, i, sep = ''), '_SUMMARYPLOT_', file.date, '_', file.processor, '.pdf', sep = ''),
 plot = plot.a,
 width = 200,
 height = 297,
 units = 'mm')

}
