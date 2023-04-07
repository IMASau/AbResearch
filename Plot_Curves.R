library(tidyverse)


scurve <- function(x){
  y <- exp(x) / (1 + exp(x))
  return(y)
}

pcurve <- function(x){
 y <-  exp(x) / (1 + exp(x))
 return(y)
}

p <- ggplot(data = data.frame(x = c(0, 1)), aes(x))



p + stat_function(fun = scurve, n = 100) 

p + stat_function(fun = scurve, n = 100) 



curve(200 * x/(0.25 + x))

micmen <- function(x, a, b) {
  (a * x)/(b + x)
}


curve(micmen(x, 200, 0.15^0.5))


logist <- function(x, a, b) {
  exp(a + b*x)/( 1 + exp(a + b*x))
}

curve(logist(x, 2, 5))


holl3 <- function(x, a, b) {
  (a*x^2)/(b^2 + x^2)
}

curve(holl3(x, 100, 0.2))

holl4 <- function(x, a, b, c) {
  (a*x^2)/(b + c*x^2 + x^2)
}

curve(holl4(x, 100, 0.04, 0.05))

q <- 0.5
curve(q*x^.75)




g  <- tibble(x = 1:10,
             y = rnorm(10, x^(0.2), 0.05))


library(ggplot2)

mod <- lm(log(y) ~ log(x), data = g)




## For hyperstability plots
pwr <- function(x) a*x^lbda

x <- 1:500
lbda <- 1
a <- 1
dat_lbda1 <- data.frame(x, y = pwr(x),lbda = "Assumed")

lbda <- 0.85
a <- 2.51
dat_lbda85 <- data.frame(x, y = pwr(x),lbda = "Spatial")


 lbda <- 0.5
 a <- 17
 dat_lbda5A <- data.frame(x, y = pwr(x),lbda = "Hyper")


lbda <- 0.5
a <- 22.25
dat_lbda5 <- data.frame(x, y = pwr(x),lbda = "Hyperstable")


lbda_dat <- bind_rows(dat_lbda1, dat_lbda85, dat_lbda5, dat_lbda5A) %>% 
 mutate(lbda = as.factor(lbda))


p <-
 filter(lbda_dat,
        #lbda %in% c("Assumed", "Hyperstable", "Spatial", "Hyper")) %>%
        lbda %in% c("Assumed",  "Hyper")) %>%
 ggplot(aes(
  x = x,
  y = y,
  group = lbda,
  colour = lbda
 )) +
 geom_point(size = 1) +
 scale_color_manual(values = c(
  "Assumed" = "black",
  "Spatial" = "green",
  "Hyperstable" = "red",
  "Hyper" = "red"
 )) +
 theme_light() +
 theme(
  text = element_text(size = 16),
  axis.text = element_blank(),
  legend.title = element_blank()
 )


pwr_plot <- update_labels(p, list(x = "Abundance/Biomass", y = "Catch Rate"))

savedir <- "C:/Users/cmundy/OneDrives/OneDrive - University of Tasmania/Research/Presentations/Conferences/IntAbSymp/IAS2023"
ggsave(file.path(savedir, "Biomass_index_spatial.svg"), pwr_plot, device = "svg", width = 12, height = 10, units = "cm")
