

#Join bigabs and juveniles
#


# ## USe all data -------------------##
# 

#dat <- filter(juv, ab_sl <40 ) %>%  # filter below 40mm
dat <- juv %>%  # use all data
 group_by(survindex) %>%
 summarise(ab_n =n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

## calculate abs per square metre 
dat$absm <- dat$ab_n * (1/platearea)

## unpack survindex var
abcounts <- data.frame(separate(dat, survindex, sep = "_", into = c("site", "survdate", "string","plate"), convert = TRUE), dat$survindex, dat$ab_n, dat$absm)

abcounts$survdate <- as.Date(strptime(abcounts$survdate, "%Y-%m-%d"))
abcounts$sampyear <- as.factor(year(abcounts$survdate)) 
abcounts$season <- getSeason(abcounts$survdate) 
## recode autumn samples as summer
abcounts$season <- gsub( "Autumn", "Summer", abcounts$season)
abcounts$season <- as.factor(abcounts$season)
abcounts$season <- ordered(abcounts$season, levels=c("Summer","Winter","Spring"))
abcounts$yr.season <- interaction(abcounts$sampyear,abcounts$season)
abcounts$yr.season <-
 ordered(abcounts$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", "2016.Summer", "2016.Winter", "2016.Spring", "2017.Summer", "2017.Winter", "2017.Spring", '2018.Summer', '2018.Winter', '2018.Spring'))
pick <- which(abcounts$location == "TG")
abcounts$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", abcounts$yr.season[pick])
abcounts$yr.season <- droplevels(abcounts$yr.season)


levels(abcounts$yr.season)



## Filter for sub-legal abalone
bigabsub <- bigabs %>% filter(sllength <= 137) %>%
 #bigabdat <- bigabs %>%
 group_by(survindex) %>%
 summarise(ab_n = n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

## unpack survindex var
bigabsub <- data.frame(separate(bigabsub, survindex, sep = "_", into = c("site", "survdate", "string","transect"), convert = TRUE), bigabdat$survindex, bigabdat$ab_n, bigabdat$absm)

bigabsub$survdate <- as.Date(strptime(bigabsub$survdate, "%Y-%m-%d"))
bigabsub$sampyear <- year(bigabsub$survdate) 
bigabsub$season <- getSeason(bigabsub$survdate) 
## recode autumn samples as summer
bigabsub$season <- gsub( "Autumn", "Summer", bigabsub$season)
bigabsub$season <- as.factor(bigabsub$season)
bigabsub$season <- ordered(bigabsub$season, levels=c("Summer","Winter","Spring"))
bigabsub$yr.season <- interaction(bigabsub$sampyear,bigabsub$season)
bigabsub$yr.season <-
 ordered(bigabsub$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", "2016.Summer", "2016.Winter", "2016.Spring", "2017.Summer", "2017.Winter", "2017.Spring", '2018.Summer', '2018.Winter', '2018.Spring'))
pick <- which(bigabsub$location == "TG")
bigabsub$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", bigabsub$yr.season[pick])
bigabsub$yr.season <- droplevels(bigabsub$yr.season) 


## calculate abs per square metre 
bigabsub$absm <- bigabsub$ab_n / 15

unique(bigabsub$site)
bigabsub <- subset(bigabsub, site %in% c("BI","BRB","BRS","GIII", "SP", "TG")) #, "MB", "T"))


## All abs greater than 137mm 
bigablegal <- bigabs %>% filter(sllength >= 138) %>%
 group_by(survindex) %>%
 summarise(ab_n = n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

## unpack survindex var
bigablegal <- data.frame(separate(bigablegal, survindex, sep = "_", into = c("site", "survdate", "string","transect"), convert = TRUE), bigabdat$survindex, bigabdat$ab_n, bigabdat$absm)

bigablegal$survdate <- as.Date(strptime(bigablegal$survdate, "%Y-%m-%d"))
bigablegal$sampyear <- year(bigablegal$survdate) 
bigablegal$season <- getSeason(bigablegal$survdate) 
## recode autumn samples as summer
bigablegal$season <- gsub( "Autumn", "Summer", bigablegal$season)
bigablegal$season <- as.factor(bigablegal$season)
bigablegal$season <- ordered(bigablegal$season, levels=c("Summer","Winter","Spring"))
bigablegal$yr.season <- interaction(bigablegal$sampyear,bigablegal$season)
bigablegal$yr.season <-
 ordered(bigablegal$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", "2016.Summer", "2016.Winter", "2016.Spring", "2017.Summer", "2017.Winter", "2017.Spring", '2018.Summer', '2018.Winter', '2018.Spring'))
pick <- which(bigablegal$location == "TG")
bigablegal$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", bigablegal$yr.season[pick])
bigablegal$yr.season <- droplevels(bigablegal$yr.season) 

## calculate abs per square metre 
bigablegal$absm <- bigablegal$ab_n / 15

unique(bigablegal$site)
bigablegal <- subset(bigablegal, site %in% c("BI","BRB","BRS","GIII", "SP", "TG")) #, "MB", "T"))



juvmeans <- abcounts %>% 
 group_by(site, yr.season) %>%
 summarise(Juvenile = mean(absm)) 


bigabsubmeans <- bigabsub %>% 
 group_by(site, yr.season) %>%
 summarise(SubAdult = mean(absm)) 

bigablegalmeans <- bigablegal %>% 
 group_by(site, yr.season) %>%
 summarise(Adult = mean(absm)) 


## plot total juveniles against total sublegal and legal
pooled <- left_join(juvmeans, bigabsubmeans, by = c("site", "yr.season"))
pooled <- left_join(pooled, bigablegalmeans, by = c("site", "yr.season"))

GGally::ggpairs(pooled, columns = 3:5,  aes(colour = site), diag='blank') +
 theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="right") +
 xlab(bquote('Abalone abundance ('*~m^2*')')) + 
 ylab(bquote('Abalone abundance ('*~m^2*')')) 


## recode by habitat complexity




juvmeans <- abcounts %>% 
 group_by(habitat, site, yr.season) %>%
 summarise(Juvenile = mean(absm)) 



#---

pick1 <- which(bigabsub$site %in% c("TG", "SP", "BRB"))
bigabsub$habitat[pick1] <- "complex"

pick2 <- which(bigabsub$site %in% c("BI", "G3"))
bigabsub$habitat[pick2] <- "low"

pick3 <- which(bigabsub$site %in% c("BRS"))
bigabsub$habitat[pick3] <- "slab"


bigabsubmeans <- bigabsub %>% 
 group_by(habitat, site, yr.season) %>%
 summarise(SubAdult = mean(absm)) 


pick1 <- which(bigablegal$site %in% c("TG", "SP", "BRB"))
bigablegal$habitat[pick1] <- "complex"

pick2 <- which(bigablegal$site %in% c("BI", "G3"))
bigablegal$habitat[pick2] <- "low"

pick3 <- which(bigablegal$site %in% c("BRS"))
bigablegal$habitat[pick3] <- "slab"


bigablegalmeans <- bigablegal %>% 
 group_by(habitat, site, yr.season) %>%
 summarise(Adult = mean(absm)) 




## plot total juveniles against total sublegal and legal
pooled <- left_join(juvmeans, bigabsubmeans, by = c("site", "yr.season"))
pooled <- left_join(pooled, bigablegalmeans, by = c("site", "yr.season"))

ggpairs(pooled, columns = 3:5,  aes(colour = site), diag='blank') +
 theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="right") +
 xlab(bquote('Abalone Abundance ('*~m^2*')')) + 
 ylab(bquote('Abalone Abundance ('*~m^2*')')) 

## end recode by habitat complexity


# lag plots for adults and juvs
lag1.l <- filter(bigablegalmeans, yr.season == "2015.Spring") %>%
 select(site, Adult) %>%
 rename(Adult.2015.Spring = Adult)
lag2.l <- filter(bigablegalmeans, yr.season == "2016.Spring") %>%
 select(site, Adult) %>%
 rename(Adult.2016.Spring = Adult)
lag3.l <- filter(bigablegalmeans, yr.season == "2017.Spring") %>%
 select(site, Adult) %>%
 rename(Adult.2017.Spring = Adult)

lag1.j <- filter(juvmeans, yr.season == "2016.Spring") %>%
select(site, Juvenile) %>%
 rename(Juv.2016.Spring = Juvenile)
lag2.j <- filter(juvmeans, yr.season == "2017.Summer") %>%
select(site, Juvenile) %>%
 rename(Juv.2017.Summer = Juvenile)
lag3.j <- filter(juvmeans, yr.season == "2017.Spring") %>%
 select(site, Juvenile) %>%
 rename(Juv.2017.Spring = Juvenile)
lag4.j <- filter(juvmeans, yr.season == "2018.Spring") %>%
 select(site, Juvenile) %>%
 rename(Juv.2018.Spring = Juvenile)
lag5.j <- filter(juvmeans, yr.season == "2018.Summer") %>%
 select(site, Juvenile) %>%
 rename(Juv.2018.Summer = Juvenile)

lag <- left_join(lag1.l, lag2.l, lag3.l, by = c("site"))
lag <- left_join(lag, lag1.j, by = c("site"))
lag <- left_join(lag, lag2.j, by = c("site"))
lag <- left_join(lag, lag3.j, by = c("site"))
lag <- left_join(lag, lag4.j, by = c("site"))
lag <- left_join(lag, lag5.j, by = c("site"))

ggpairs(lag, columns = 2:8, aes(colour = site), diag='blank', upper='blank') +
 theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="right") +
 xlab(bquote('Abalone Abundance ('*~m^2*')')) + 
 ylab(bquote('Abalone Abundance ('*~m^2*')'))

